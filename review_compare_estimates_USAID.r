# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Compare estimates of incidence from the 4 most recent global TB reports
# for USAID (based on original code in review_compare_estimates.r)
# Hazim Timimi
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# clear the decks
rm(list=ls())

# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# scriptsfolder:      Folder containing these scripts
# file_name_*:        Names of the PDF output files
#
# The next two are set using set_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


file_name_inc      <- paste0("inc_graphs_USAID_", Sys.Date(), ".pdf")



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
               e_inc_100k, e_inc_100k_lo, e_inc_100k_hi
               FROM	epi_estimates_rawvalues_at_date(CAST('", limiting_date, "' AS DATE))")

  # Extract data from the database
  estimates <- sqlQuery(channel,sql)


  # rename variables to include version using plyr
  estimates <- rename(estimates, c("e_inc_100k" = paste0("inc_",version),
                                     "e_inc_100k_lo" = paste0("inc_lo_",version),
                                     "e_inc_100k_hi" = paste0("inc_hi_",version)))


  return(estimates)
}


# Extract data from the database
ch <- odbcDriverConnect(connection_string)


estimates_series_1 <- get_historical_estimates(ch, "series1", series_1_date)
estimates_series_2 <- get_historical_estimates(ch, "series2", series_2_date)
estimates_series_3 <- get_historical_estimates(ch, "series3", series_3_date)
estimates_series_4 <- get_historical_estimates(ch, "series4", series_4_date)

# get list of countries
countries <- sqlQuery(ch, "SELECT country FROM view_TME_master_report_country
                          WHERE iso2 in ('AF','BD','KH','CD','ET','IN','ID','KE','KG','MW','MZ','MM','NG','PH','ZA','SS','TJ','UG','UA','TZ','UZ','ZM','ZW')
                          ORDER BY country")

close(ch)

# combine the three series into a single wider one called changes

changes <- merge(estimates_series_4, estimates_series_3, all.x=TRUE)
changes <- merge(changes, estimates_series_2, all.x=TRUE)
changes <- merge(changes, estimates_series_1, all.x=TRUE)

rm(list=ls(pattern = "^estimates_series"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Define graph layout ----
#
# Plot set of incidence graphs
# Each graph shows 4 series for the same indicator, each from a separate global report.
# Series will overlap, hence make them transparent -- I've used alpha=0.2
#
# Assumes the input dataframe is wide, with the three data series labelled series1, series2 and series3
# and indicators called inc, prev and mort, with uncertainty intervals called lo and hi
#
# variable format is variable_series
#
# therefore inc_lo_series2 is the low bound incidence from series2
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

plot_inc <- function(df){

  #plot incidence (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3
  # Yellow bands  = series 4

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

        geom_line(aes(year, inc_series4), colour=I("yellow")) +
        geom_ribbon(aes(year,
                        ymin=inc_lo_series4,
                        ymax=inc_hi_series4),
                    fill=I("yellow"), alpha=0.2) +

        facet_wrap(~country, scales="free_y") +
        xlab("") +
        ylab("Incidence rate per 100 000 population per year") +
        expand_limits(y=0) +
        theme_bw(base_size=8) +
        theme(legend.position="bottom"))
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")

setwd(outfolder)

plot_blocks_to_pdf(changes, countries, file_name_inc,     plot_function = plot_inc)


# clear the decks
rm(list=ls())





