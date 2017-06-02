# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at trends in number of notifications among foreigners
# Hazim Timimi, June 2017
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

file_name     <- paste0("foreigner_graphs_", Sys.Date(), ".pdf")
file_name_pcnt     <- paste0("foreigner_pcnt_graphs_", Sys.Date(), ".pdf")

setwd(scriptsfolder)

source("set_environment.r")  #particular to each person so this file is in the ignore list


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

sql <- "SELECT country, year, notif_foreign, c_newinc FROM dcf.latest_notification
                WHERE notif_foreign IS NOT NULL
                UNION ALL
                SELECT country, year, notif_foreign, c_newinc FROM view_TME_master_notification
                WHERE year BETWEEN 2006 AND (SELECT MAX(year - 1) FROM dcf.latest_notification) AND
  					    iso2 IN (SELECT iso2 from dcf.latest_notification WHERE notif_foreign IS NOT NULL)
				        ORDER BY country,year"


# Extract data from the database
channel <- odbcDriverConnect(connection_string)
data_to_plot <- sqlQuery(channel,sql)

# get list of countries
countries <- sqlQuery(channel, "SELECT country FROM dcf.latest_notification WHERE notif_foreign IS NOT NULL ORDER BY country")

close(channel)

# Calculate pcnt foreigners ----
# - - - - - - - - - - -

data_to_plot <- data_to_plot %>%
                mutate( pcnt_foreign = ifelse(c_newinc > 0, notif_foreign * 100 / c_newinc, NA))



# Define graph layout ----
# - - - - - - - - - - -

plot_faceted <- function(df){

  # Blue line  = New and relapse cases
  # Green dots = Cases among foreigners


graphs <- qplot(year, c_newinc, data=df, geom="line", colour=I("blue")) +
          geom_point(aes(year, notif_foreign), colour=I("green")) +
          facet_wrap(~country, scales="free_y") +
          xlab("year") + ylab("cases") +
          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="bottom")

  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)

}

plot_faceted_pcnt <- function(df){

  # Blue line  = % cases among foreigners

graphs <- qplot(year, pcnt_foreign, data=df, geom="line", colour=I("blue")) +
          geom_point(aes(year, pcnt_foreign), colour=I("blue")) +
          facet_wrap(~country, scales="free_y") +
          xlab("year") + ylab("% foreign") +
          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="bottom")
  print(graphs)

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")

setwd(outfolder)

plot_blocks_to_pdf(data_to_plot, countries, file_name, plot_function = plot_faceted)

plot_blocks_to_pdf(data_to_plot, countries, file_name_pcnt, plot_function = plot_faceted_pcnt)

# clear the decks
rm(list=ls())





