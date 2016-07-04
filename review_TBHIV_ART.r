# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at TB/HIV and ART trends using data from previous years and
# the latest data reported to us and separately to UNAIDS (via GARPR)
# Hazim Timimi, June 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# clear the decks
rm(list=ls())

# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# scriptsfolder:      Folder containing these scripts
# file_name:          Name of the PDF output file
#
# The next two are set using set_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

scriptsfolder <- getSrcDirectory(function(x) {x})  # See http://stackoverflow.com/a/30306616

file_name     <- paste0("TBHIV_ART_graphs_", Sys.Date(), ".pdf")

setwd(scriptsfolder)

source("set_environment.r")  #particular to each person so this file is in the ignore list


# load packages ----
library(RODBC)
library(ggplot2)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
#
# I prefer to do this via SQL, but could be done of course with the pure views
# and some R jiggery pokey
#
# The query combines data from the master notification view with latest data
# reported as retreived from the dcf views (dcf = data collection form)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sql <- "SELECT country, year, newrel_hivpos AS hivtest_pos, newrel_art AS hiv_art, hiv_tbrx_art
              FROM dcf.latest_notification
              WHERE newrel_hivpos IS NOT NULL
             UNION ALL
             /* master data from previous years */
             SELECT country, year,
                    COALESCE(hivtest_pos_f, hivtest_pos_p) AS hivtest_pos,
                    COALESCE(hiv_art_f, hiv_art_p) AS hiv_art,
                    NULL AS hiv_tbrx_art
             FROM view_TME_master_notification
             WHERE year BETWEEN 2006 AND (SELECT max(year - 1) from dcf.latest_notification) AND
             iso2 IN (SELECT iso2 from dcf.latest_notification WHERE newrel_hivpos IS NOT NULL)
             ORDER BY country,year;"


# Extract data from the database
channel <- odbcDriverConnect(connection_string)
data_to_plot <- sqlQuery(channel,sql)

# get list of countries
countries <- sqlQuery(channel, "SELECT country FROM dcf.latest_notification WHERE newrel_hivpos IS NOT NULL ORDER BY country;")

close(channel)


# Define graph layout ----
# - - - - - - - - - - -

plot_faceted <- function(df){

  # Red line  = number of HIV+ TB cases
  # Blue line = number of HIV+ TB cases on ART, as reported by the NTP to us
  # Black dot = number of HIV+ TB cases on ART, as reported by the NAP to the UNAIDS GARPR system

  graphs <- qplot(year, hivtest_pos, data=df, geom="line", colour=I("red")) +
            geom_line(aes(year, hiv_art), colour=I("blue")) +
            geom_point(aes(year, hiv_tbrx_art), colour=I("black")) +
            facet_wrap(~country, scales="free_y") +
            xlab("year") + ylab("cases") +
            expand_limits(y=0) +
            theme_bw(base_size=8) +
            theme(legend.position="bottom")

  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)

}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")

setwd(outfolder)

plot_blocks_to_pdf(data_to_plot, countries, file_name)

# clear the decks
rm(list=ls())





