# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at trends in number of notifications from PPM
# Hazim Timimi, June 2018
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
source("set_plot_themes.r")

# Define list of regions in SQL format if we don't want to plot all countries
# (If not keep it as an empty string)
# region_filter <- "AND g_whoregion IN ('AFR', 'EMR','SEA', 'WPR')"

region_filter <- ""


file_name     <- paste0(outfolder, "ppm_graphs_", Sys.Date(), ".pdf")
file_name_pct <- paste0(outfolder, "ppm_pct_graphs_", Sys.Date(), ".pdf")



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

sql <- "SELECT dcf.latest_strategy.country, dcf.latest_strategy.year, priv_new_dx, pub_new_dx, c_newinc
                FROM dcf.latest_strategy
                  INNER JOIN dcf.latest_notification ON
                    dcf.latest_strategy.iso2 = dcf.latest_notification.iso2 AND
                    dcf.latest_strategy.year = dcf.latest_notification.year
                WHERE COALESCE(pub_new_dx, priv_new_dx) IS NOT NULL
                UNION ALL
                SELECT view_TME_master_strategy.country, view_TME_master_strategy.year, priv_new_dx, pub_new_dx, c_newinc
                FROM view_TME_master_strategy
                  INNER JOIN view_TME_master_notification ON
                    view_TME_master_strategy.iso2 = view_TME_master_notification.iso2 AND
                    view_TME_master_strategy.year = view_TME_master_notification.year
                WHERE view_TME_master_strategy.year BETWEEN 2015 AND (SELECT MAX(year - 1) FROM dcf.latest_strategy) AND
  					    view_TME_master_strategy.iso2 IN (SELECT iso2 FROM dcf.latest_strategy WHERE COALESCE(pub_new_dx, priv_new_dx) IS NOT NULL)
				        ORDER BY country,year"


# Extract data from the database
channel <- odbcDriverConnect(connection_string)
data_to_plot <- sqlQuery(channel,sql)

# get list of countries
countries <- sqlQuery(channel,
                      paste("SELECT country FROM dcf.latest_strategy",
                            "WHERE COALESCE(pub_new_dx, priv_new_dx) IS NOT NULL",
                            region_filter,
                            "ORDER BY country"))

close(channel)

# Calculate pcnt PPM contributions ----
# - - - - - - - - - - -

data_to_plot <- data_to_plot %>%
                mutate( pcnt_ppm_private = ifelse(c_newinc > 0, priv_new_dx * 100 / c_newinc, NA),
                        pcnt_ppm_public  = ifelse(c_newinc > 0, pub_new_dx * 100 / c_newinc, NA) )


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

graphs <- qplot(year, priv_new_dx, data=df, geom="point", colour=I("blue")) +
  geom_line(aes(year, priv_new_dx), colour=I("blue"), size = 1) +

  geom_point(aes(year, pub_new_dx), colour=I("green"), size = 1) +
  geom_line(aes(year, pub_new_dx), colour=I("green")) +

          # Use space separators for the y axis
          scale_y_continuous(name = "Private-public (blue) and public-public (green) notifications (number per year)",
                             labels = rounder) +

          scale_x_continuous(name="", breaks = c(2015, 2018, 2021)) +

          facet_wrap(~country,
                     scales="free_y",
                     # Use the labeller function to make sure long country names are wrapped in panel headers
                     labeller = label_wrap_gen(width = 24)) +

            expand_limits(y=0) +

            theme_gtbr_2021(base_size=8, axis_text_size = 6)

  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)

}


plot_faceted_pcnt <- function(df){

graphs <- qplot(year, pcnt_ppm_private, data=df, geom="point", colour=I("blue")) +
  geom_line(aes(year, pcnt_ppm_private), colour=I("blue"), size = 1) +

  geom_point(aes(year, pcnt_ppm_public), colour=I("green"), size = 1) +
  geom_line(aes(year, pcnt_ppm_public), colour=I("green")) +

          # Use space separators for the y axis
          scale_y_continuous(name = "Private-public (blue) and public-public (green) notifications (% of total new and relapse)",
                             labels = rounder) +

          scale_x_continuous(name="", breaks = seq(min(df$year), max(df$year), by=3)) +

          facet_wrap(~country,
                     scales="free_y",
                     # Use the labeller function to make sure long country names are wrapped in panel headers
                     labeller = label_wrap_gen(width = 24)) +

            expand_limits(y=0) +

            theme_gtbr_2021(base_size=8, axis_text_size = 6)

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
plot_blocks_to_pdf(data_to_plot, countries, file_name_pct, plot_function = plot_faceted_pcnt)

