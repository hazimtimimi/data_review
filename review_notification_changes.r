# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at % and absolute change in notifications using data from previous years and
# the latest data reported to us
# Hazim Timimi, November 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# clear the decks
rm(list=ls())

# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# scriptsfolder:      Folder containing these scripts
# file_name_pcnt:     Name of the PDF output file for % changes
# file_name_delta:    Name of the PDF output file for changes in absolute numers
#
# start_year:         Starting year for the graphs
# minimum_notifs:     Minimum number of notifications reported in the data collection form
#                     for the coutnry to be included in the graphs
#
# The next two are set using set_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

scriptsfolder <- getSrcDirectory(function(x) {x})  # See http://stackoverflow.com/a/30306616
setwd(scriptsfolder)

file_name_pcnt <- paste0("notif_change_pcnt_", Sys.Date(), ".pdf")
file_name_delta <- paste0("notif_change_delta_", Sys.Date(), ".pdf")

start_year <- 2007
minimum_notifs <- 1000

source("set_environment.r")  #particular to each person so this file is in the ignore list


# load packages ----
library(dplyr)
library(RODBC)
library(ggplot2)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Extract data from the database
channel <- odbcDriverConnect(connection_string)

# latest notifications reported in the dcf views (dcf = data collection form)
notifs_dcf <- sqlQuery(channel, "SELECT country, year, c_newinc
                                FROM dcf.latest_notification",
                       stringsAsFactors = FALSE)

# notifications reported in previous years
notifs_historic <- sqlQuery(channel,
                      paste("SELECT country, year, c_newinc
                            FROM view_TME_master_notification
                            WHERE year BETWEEN ", start_year,
                            " AND (SELECT max(year - 1) from dcf.latest_notification)"),
                       stringsAsFactors = FALSE)

close(channel)


# Calculate % change in notifications  ----
# - - - - - - - - - - -

# Identify countries exceeding notification threshold to show in the output
countries <- notifs_dcf %>%
              filter(c_newinc >= minimum_notifs) %>%
              select(country) %>%
              arrange(country)


# Combine the dcf and historic notifications and do the maths
notifs <- union(notifs_historic, notifs_dcf) %>%
          arrange(country, year) %>%
          group_by(country) %>%
          mutate(c_newinc_prev = lag(c_newinc)) %>%
          ungroup() %>%
          mutate(c_newinc_pcnt = ifelse(c_newinc_prev == 0,
                                        NA,
                                        (c_newinc - c_newinc_prev) * 100 / c_newinc_prev),
                 c_newinc_delta = c_newinc - c_newinc_prev)


# Define graph layout ----
# - - - - - - - - - - -

plot_faceted_pcnt <- function(df){

  # Blue line  = Year on year % change in new and relapse cases

  graphs <- qplot(year, c_newinc_pcnt, data=df, geom="line", colour=I("blue")) +
            facet_wrap(~country, scales="free_y") +
            xlab("year") + ylab("Annual change in new and relapse cases (%)") +
            expand_limits(y=c(-10,10)) +
            theme_bw(base_size=8) +
            theme(legend.position="bottom") +
            # Hide background gridlines
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            # Add a black line to highlight 0%
            geom_hline(aes(yintercept=0), colour = "gray", linetype = "dashed")

  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)

}

plot_faceted_delta <- function(df){

  # Blue line  = Year on year change in absolute number of new and relapse cases

  graphs <- qplot(year, c_newinc_delta, data=df, geom="line", colour=I("blue")) +
            facet_wrap(~country, scales="free_y") +
            xlab("year") + ylab("Annual change in new and relapse cases (number)") +
            expand_limits(y=0) +
            theme_bw(base_size=8) +
            theme(legend.position="bottom")  +
            # Hide background gridlines
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            # Add a black line to highlight 0
            geom_hline(aes(yintercept=0), colour = "gray", linetype = "dashed")

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

plot_blocks_to_pdf(notifs, countries, file_name_pcnt, plot_function = plot_faceted_pcnt)
plot_blocks_to_pdf(notifs, countries, file_name_delta, plot_function = plot_faceted_delta)






