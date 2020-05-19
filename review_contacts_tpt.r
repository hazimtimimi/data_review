# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at trends in number of contacts provided with TB preventive therapy
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

file_name     <- paste0(outfolder, "contacts_tpt_graphs_", Sys.Date(), ".pdf")


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

sql <- "SELECT	country, year, newinc_con_prevtx, newinc_con04_prevtx
FROM	dcf.latest_strategy
WHERE	COALESCE(newinc_con_prevtx, newinc_con04_prevtx) IS NOT NULL
UNION ALL
SELECT country, year, newinc_con_prevtx, newinc_con04_prevtx
FROM view_TME_master_strategy
WHERE year BETWEEN 2015 AND (SELECT MAX(year - 1) FROM dcf.latest_strategy) AND
iso2 IN (SELECT iso2 from dcf.latest_strategy WHERE COALESCE(newinc_con_prevtx, newinc_con04_prevtx) IS NOT NULL)
ORDER BY country,year;"

# Extract data from the database
channel <- odbcDriverConnect(connection_string)
data_to_plot <- sqlQuery(channel,sql)

# get list of countries
countries <- sqlQuery(channel, "SELECT country FROM dcf.latest_strategy WHERE COALESCE(newinc_con_prevtx, newinc_con04_prevtx) IS NOT NULL ORDER BY country")

close(channel)


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

plot_faceted <- function(df){

  # Blue line  = Contacts started on TPT
  # Green dots = Contacts aged 0-4 started on TPT


graphs <- qplot(year, newinc_con_prevtx, data=df, geom="point", colour=I("blue")) +
          geom_line(aes(year, newinc_con_prevtx), colour=I("blue"), size = 1.25) +

          geom_line(aes(year, newinc_con04_prevtx), colour=I("green")) +
          geom_point(aes(year, newinc_con04_prevtx), colour=I("green"), size = 1) +

          # Use space separators for the y axis
          scale_y_continuous(name = "Contacts started on TPT (blue) and Contacts aged 0-4 started on TPT (green) (number)",
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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")


plot_blocks_to_pdf(data_to_plot, countries, file_name, plot_function = plot_faceted)






