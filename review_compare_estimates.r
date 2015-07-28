# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Compare estimates of incidence, prevalence and mortality from three separate
# Global TB reports
# Hazim Timimi, December 2014, based on original code from Babis Sismanidis
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

file_name     <- paste0("estimates_graphs_", Sys.Date(), ".pdf")

setwd(scriptsfolder)

source("set_environment.r")  #particular to each person so this file is in the ignore list


# load packages ----
library(RODBC)
library(ggplot2)
library(plyr)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
#
# I prefer to do this via SQL, but could be done of course with the pure views
# and some R jiggery pokey
#
# Extract historical estimates for a given report/date
# and rename them using the pattern variablename_version
#
# therefore inc_lo_series1 is the low bound incidence for version called series1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

get_historical_estimates <- function(channel,version,limiting_date){

  sql <- paste0("SELECT country, iso3, year,
               e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
               e_prev_100k, e_prev_100k_lo, e_prev_100k_hi,
               e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi
               FROM	epi_estimates_at_date(CAST('", limiting_date, "' AS DATE))")

  # Extract data from the database
  estimates <- sqlQuery(channel,sql)


  # rename variables to include version using plyr
  estimates <- rename(estimates, c("e_inc_100k" = paste0("inc_",version),
                                     "e_inc_100k_lo" = paste0("inc_lo_",version),
                                     "e_inc_100k_hi" = paste0("inc_hi_",version),
                                     "e_prev_100k" = paste0("prev_",version),
                                     "e_prev_100k_lo" = paste0("prev_lo_",version),
                                     "e_prev_100k_hi" = paste0("prev_hi_",version),
                                     "e_mort_exc_tbhiv_100k" = paste0("mort_",version),
                                     "e_mort_exc_tbhiv_100k_lo" = paste0("mort_lo_",version),
                                     "e_mort_exc_tbhiv_100k_hi" = paste0("mort_hi_",version)))


  return(estimates)
}


# Extract data from the database
ch <- odbcDriverConnect(connection_string)


estimates_series_1 <- get_historical_estimates(ch, "series1","2013-12-31")    #2013 report (with post-publication changes)
estimates_series_2 <- get_historical_estimates(ch, "series2","2015-05-31")    #2014 report (with post-publication changes)
estimates_series_3 <- get_historical_estimates(ch, "series3","2015-07-31")    #2015 report (run #1)

# get list of countries
countries <- sqlQuery(ch, "SELECT country FROM view_TME_master_report_country ORDER BY country")

close(ch)

# combine the three series into a single wider one called changes

changes <- merge(estimates_series_3, estimates_series_2, all.x=TRUE)
changes <- merge(changes, estimates_series_1, all.x=TRUE)

rm(list=c("estimates_series_1", "estimates_series_2", "estimates_series_3"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Define graph layout ----
#
# Plot 3 sets of graphs (incidence, prevalence, mortality)
# Each graph shows three series for the same indicator, each from a separate global report.
# Series will overlap, hence make them transparent -- I've used alpha=0.2
#
# Assumes the input dataframe is wide, with the three data series labelled series1, series2 and series3
# and indicators called inc, prev and mort, with uncertainty intervals called lo and hi
#
# variable format is variable_series
#
# therefore inc_lo_series2 is the low bound incidence from series2
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

plot_incprevmort <- function(df){

  # Blue bands  = series 1 (2012 estimates)
  # Red bands   = series 2 (2013 estimates)
  # Green bands = series 3 (2014 estimates)

  #note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)

  print(qplot(year, inc_series1, data=df, geom="line", colour=I("blue")) +
        geom_ribbon(aes(year,
                        ymin=inc_lo_series1,
                        ymax=inc_hi_series1),
                    fill=I("blue"), alpha=0.2) +

        geom_line(aes(year, inc_series2), colour=I("red")) +
        geom_ribbon(aes(year,
                        ymin=inc_lo_series2,
                        ymax=inc_hi_series2),
                    fill=I("red"), alpha=0.2) +

        geom_line(aes(year, inc_series3), colour=I("green")) +
        geom_ribbon(aes(year,
                        ymin=inc_lo_series3,
                        ymax=inc_hi_series3),
                    fill=I("green"), alpha=0.2) +
        facet_wrap(~country, scales="free_y") +
        xlab("") +
        ylab("Incidence rate per 100 000 population per year") +
        expand_limits(y=0) +
        theme_bw(base_size=8) +
        theme(legend.position="bottom"))


  #plot prevalence (faceted)
  print(qplot(year, prev_series1, data=df, geom="line", colour=I("blue")) +
        geom_ribbon(aes(year,
                        ymin=prev_lo_series1,
                        ymax=prev_hi_series1),
                    fill=I("blue"), alpha=0.2) +

        geom_line(aes(year, prev_series2), colour=I("red")) +
        geom_ribbon(aes(year,
                        ymin=prev_lo_series2,
                        ymax=prev_hi_series2),
                    fill=I("red"), alpha=0.2) +

        geom_line(aes(year, prev_series3), colour=I("green")) +
        geom_ribbon(aes(year,
                        ymin=prev_lo_series3,
                        ymax=prev_hi_series3),
                    fill=I("green"), alpha=0.2) +
        facet_wrap(~country, scales="free_y") +
        xlab("") +
        ylab("Prevalence rate per 100 000 population") +
        expand_limits(y=0) +
        theme_bw(base_size=8) +
        theme(legend.position="none"))


  #plot mortality (faceted)
  print(qplot(year, mort_series1, data=df, geom="line", colour=I("blue")) +
        geom_ribbon(aes(year,
                        ymin=mort_lo_series1,
                        ymax=mort_hi_series1),
                    fill=I("blue"), alpha=0.2) +

        geom_line(aes(year, mort_series2), colour=I("red")) +
        geom_ribbon(aes(year,
                        ymin=mort_lo_series2,
                        ymax=mort_hi_series2),
                    fill=I("red"), alpha=0.2) +

        geom_line(aes(year, mort_series3), colour=I("green")) +
        geom_ribbon(aes(year,
                        ymin=mort_lo_series3,
                        ymax=mort_hi_series3),
                    fill=I("green"), alpha=0.2) +
        facet_wrap(~country, scales="free_y") +
        xlab("") +
        ylab("Mortality rate per 100 000 population per year") +
        expand_limits(y=0) +
        theme_bw(base_size=8) +
        theme(legend.position="none"))

}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")

setwd(outfolder)

plot_blocks_to_pdf(changes, countries, file_name, plot_function = plot_incprevmort)

# clear the decks
rm(list=ls())





