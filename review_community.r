# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at trends in reporting on community indicators
# Hazim Timimi, May 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# file_name:          Name of the PDF output file
#
# The next two are set using set_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


source("set_environment.r")  #particular to each person so this file is in the ignore list

file_name_bmu_ref_data     <- paste0(outfolder, "bmu_ref_data_graphs_", Sys.Date(), ".pdf")
file_name_comm_ref_pcnt     <- paste0(outfolder, "community_ref_pcnt_graphs_", Sys.Date(), ".pdf")

file_name_bmu_rxsupport_data     <- paste0(outfolder, "bmu_rxsuport_data_graphs_", Sys.Date(), ".pdf")
file_name_comm_rxsupport_pcnt     <- paste0(outfolder, "community_rxsupport_pcnt_graphs_", Sys.Date(), ".pdf")
file_name_comm_rxsupport_tsr     <- paste0(outfolder, "community_rxsupport_tsr_graphs_", Sys.Date(), ".pdf")


# load packages ----
library(RODBC)
library(ggplot2)
library(dplyr)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
#
# I prefer to do this via SQL, but could be done of course with the pure views
# and some R jiggery pokey
#
# The query combines data from the master notification view with latest data
# reported as retreived from the dcf views (dcf = data collection form)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sql_ref <- "SELECT	dcf.latest_strategy.[country]
		,dcf.latest_strategy.[year]
		,bmu
		,bmu_ref_data
		,notified_ref
		,notified_ref_community
FROM	dcf.latest_strategy
WHERE 	ISNULL(bmu_ref_data,0) > 0
UNION ALL
SELECT	view_TME_master_strategy.[country]
		,view_TME_master_strategy.[year]
		,bmu
		,bmu_ref_data
		,notified_ref
		,notified_ref_community
 FROM	view_TME_master_strategy
 WHERE	year BETWEEN 2015 AND (SELECT MAX(year) - 1 FROM dcf.latest_strategy)
		AND
		iso2 IN (SELECT iso2 FROM dcf.latest_strategy WHERE ISNULL(bmu_ref_data,0) > 0 )
ORDER BY country,year;"

sql_rxsupport <- "SELECT	dcf.latest_strategy.[country]
		,dcf.latest_strategy.[year] - 1 AS year
		,bmu
		,bmu_rxsupport_data
		,bmu_rxsupport_data_coh
		,rxsupport_community_coh
		,rxsupport_community_succ
FROM	dcf.latest_strategy
WHERE	ISNULL(bmu_rxsupport_data,0) > 0
UNION ALL
SELECT	view_TME_master_strategy.[country]
		,view_TME_master_strategy.[year] - 1 AS year
		,bmu
		,bmu_rxsupport_data
		,bmu_rxsupport_data_coh
		,rxsupport_community_coh
		,rxsupport_community_succ
FROM	view_TME_master_strategy
WHERE	year BETWEEN 2015 AND (SELECT MAX(year) - 1 FROM dcf.latest_strategy)
		AND
		iso2 IN (SELECT iso2 FROM dcf.latest_strategy WHERE ISNULL(bmu_rxsupport_data,0) > 0 )
ORDER BY country,year;"


# Extract data from the database
channel <- odbcDriverConnect(connection_string)

data_ref_to_plot <- sqlQuery(channel,sql_ref)
data_rxsupport_to_plot <- sqlQuery(channel,sql_rxsupport)

# get list of countries
countries_ref <- sqlQuery(channel, "SELECT country FROM dcf.latest_strategy WHERE ISNULL(bmu_ref_data,0) > 0 ORDER BY country")
countries_rxsupport <- sqlQuery(channel, "SELECT country FROM dcf.latest_strategy WHERE ISNULL(bmu_rxsupport_data,0) > 0 ORDER BY country")

close(channel)

# Calculate pcnt of BMUs with community referral data and pcnt of notifications referred by ommunity ----
# - - - - - - - - - - -

data_ref_to_plot <- data_ref_to_plot %>%
  mutate(pcnt_bmu_ref_data = ifelse(bmu > 0, bmu_ref_data * 100 / bmu, NA),
         pcnt_community_ref = ifelse(notified_ref > 0, notified_ref_community * 100 / notified_ref, NA))


# Calculate pcnt of BMUs with community treatment support data, pcnt of patients receiving support, and treatment success rates ----
# - - - - - - - - - - -

data_rxsupport_to_plot <- data_rxsupport_to_plot %>%
  mutate(pcnt_bmu_rxsupport_data = ifelse(bmu > 0, bmu_rxsupport_data * 100 / bmu, NA),
         pcnt_community_rxsupport = ifelse(bmu_rxsupport_data_coh > 0, rxsupport_community_coh * 100 /  bmu_rxsupport_data_coh, NA),
         tsr_community_rxsupport = ifelse(rxsupport_community_coh > 0, rxsupport_community_succ * 100 / rxsupport_community_coh, NA))


# Simple rounding function that returns a string rounded to the nearest integer and
# uses a space as the thousands separator as per WHO standard.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rounder <- function(x) {

    ifelse(is.na(x), NA,
           formatC(round(x,0), big.mark=" ", format="d")
           )
}

# Define graph layout ----
# - - - - - - - - - - -

plot_bmu_ref_data <- function(df){

  # Blue line/dots  = Total BMUs
  # Green line/dots = BMUs with data on community referrals

graphs <- qplot(year, bmu, data=df, geom="line", colour=I("blue")) +
          geom_point(aes(year, bmu), colour=I("blue")) +

          geom_line(aes(year, bmu_ref_data), colour=I("green")) +
          geom_point(aes(year, bmu_ref_data), colour=I("green")) +

          # Use space separators for the y axis
          scale_y_continuous(name = "Number of BMUs (blue) and number with data on community referrals (green)",
                             labels = rounder) +

          scale_x_continuous(name="", breaks = c(2015, 2017, 2019)) +

          facet_wrap(~country, scales="free_y") +

          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="none")

  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)
}


plot_community_ref_data <- function(df){

  # Blue line/dots  = Percent of BMUs providing community referral data
  # Green line/dots = Percent of notified cases that were from community referrals

graphs <- qplot(year, pcnt_bmu_ref_data, data=df, geom="line", colour=I("blue")) +
          geom_point(aes(year, pcnt_bmu_ref_data), colour=I("blue")) +

          geom_line(aes(year, pcnt_community_ref), colour=I("green")) +
          geom_point(aes(year, pcnt_community_ref), colour=I("green")) +

          # Use space separators for the y axis
          scale_y_continuous(name = "% BMUs with data on community referrals (blue), % of cases that were from community referrals (green)") +

          scale_x_continuous(name="", breaks = c(2015, 2017, 2019)) +

          facet_wrap(~country, scales="free_y") +

          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="none")

  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)
}


plot_bmu_rxsupport_data <- function(df){

  # Blue line/dots  = Total BMUs
  # Green line/dots = BMUs with data on community treatment support

graphs <- qplot(year, bmu, data=df, geom="line", colour=I("blue")) +
          geom_point(aes(year, bmu), colour=I("blue")) +

          geom_line(aes(year, bmu_rxsupport_data), colour=I("green")) +
          geom_point(aes(year, bmu_rxsupport_data), colour=I("green")) +

          # Use space separators for the y axis
          scale_y_continuous(name = "Number of BMUs (blue) and number with data on community treatment support (green)",
                             labels = rounder) +

          scale_x_continuous(name="", breaks = c(2014, 2016, 2018)) +

          facet_wrap(~country, scales="free_y") +

          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="none")

  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)
}

plot_community_rxsupport_data <- function(df){

  # Blue line/dots  = Percent of BMUs providing treatment support data
  # Green line/dots = Percent of treatment cohort that received community support

graphs <- qplot(year, pcnt_bmu_rxsupport_data, data=df, geom="line", colour=I("blue")) +
          geom_point(aes(year, pcnt_bmu_rxsupport_data), colour=I("blue")) +

          geom_line(aes(year, pcnt_community_rxsupport), colour=I("green")) +
          geom_point(aes(year, pcnt_community_rxsupport), colour=I("green")) +

          # Use space separators for the y axis
          scale_y_continuous(name = "% BMUs with data on community treatment support (blue), % of cohort that received community support (green)") +

          scale_x_continuous(name="", breaks = c(2014, 2016, 2018)) +

          facet_wrap(~country, scales="free_y") +

          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="none")

  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)
}


plot_community_rxsupport_tsr <- function(df){

  # Blue line/dots  = Treatment success rate for patients who received community support

graphs <- qplot(year, tsr_community_rxsupport, data=df, geom="line", colour=I("blue")) +
          geom_point(aes(year, tsr_community_rxsupport), colour=I("blue")) +

          # Use space separators for the y axis
          scale_y_continuous(name = "Treatment success rate for patients who received community support (%)") +

          scale_x_continuous(name="", breaks = c(2014, 2016, 2018)) +

          facet_wrap(~country, scales="free_y") +

          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="none")

  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)
}



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")


plot_blocks_to_pdf(data_ref_to_plot, countries_ref, file_name_bmu_ref_data, plot_function = plot_bmu_ref_data)

plot_blocks_to_pdf(data_ref_to_plot, countries_ref, file_name_comm_ref_pcnt, plot_function = plot_community_ref_data)

plot_blocks_to_pdf(data_rxsupport_to_plot, countries_rxsupport, file_name_bmu_rxsupport_data, plot_function = plot_bmu_rxsupport_data)

plot_blocks_to_pdf(data_rxsupport_to_plot, countries_rxsupport, file_name_comm_rxsupport_pcnt, plot_function = plot_community_rxsupport_data)

plot_blocks_to_pdf(data_rxsupport_to_plot, countries_rxsupport, file_name_comm_rxsupport_tsr, plot_function = plot_community_rxsupport_tsr)




