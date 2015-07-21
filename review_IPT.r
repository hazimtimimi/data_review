# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at IPT trends using data from previous years and
# the latest data reported to UNAIDS (via GARPR)
# Hazim Timimi, May 2015
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
# The next two are set using get_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

scriptsfolder <- getSrcDirectory(function(x) {x})  # See http://stackoverflow.com/a/30306616

file_name     <- paste0("ipt_graphs_", Sys.Date(), ".pdf")

setwd(scriptsfolder)

source("get_environment.r")  #particular to each person so this file is in the ignore list


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

sql <- "SELECT country, year, hiv_ipt FROM dcf.latest_notification
                WHERE hiv_ipt is not null
                UNION ALL
                SELECT country, year, hiv_ipt FROM view_TME_master_notification
                WHERE year BETWEEN 2006 AND (SELECT max(year - 1) from dcf.latest_notification) AND
  					    iso2 IN (SELECT iso2 from dcf.latest_notification WHERE hiv_ipt is not null)
				        ORDER BY country,year"


# Extract data from the database
channel <- odbcDriverConnect(connection_string)
data_to_plot <- sqlQuery(channel,sql)

# get list of countries
countries <- sqlQuery(channel, "SELECT country FROM dcf.latest_notification WHERE hiv_ipt is not null ORDER BY country")

close(channel)


# Define graph layout ----
# - - - - - - - - - - -

plot_faceted <- function(df){


graphs <- qplot(year, hiv_ipt, data=df, geom="line", colour=I("blue")) +
          facet_wrap(~country, scales="free_y") +
          xlab("year") + ylab("IPT (no. of patients)") +
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





