# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Compare estimates of DR-TB as a timeseries, also
# showing changes in estimates from the different rounds of the most
# recent year
# Hazim Timimi, August 2018
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# clear the decks
rm(list=ls())

# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# file_name_*:        Names of the PDF output files
#
# The next variables are set using set_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# series_1_date:      Date at which estimates for series 1 were valid
# series_2_date:      Date at which estimates for series 2 were valid
# series_3_date:      Date at which estimates for series 3 were valid
#
# series_1_startyear: Year to start series 1
# series_2_startyear: Year to start series 1
# series_3_startyear: Year to start series 1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


file_name_dr    <- paste0("dr_graphs_", Sys.Date(), ".pdf")


source("set_environment.r")  # particular to each person so this file is in the ignore list


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
# Extract historical estimates for a given report/date
# and rename them using the pattern variablename_version
#
# therefore inc_lo_series1 is the low bound incidence for version called series1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

get_historical_estimates <- function(channel,version,limiting_date, start_year){

  sql <- paste0("SELECT country, year,
               e_rr_prop_new, e_rr_prop_new_lo, e_rr_prop_new_hi,
               e_rr_prop_ret, e_rr_prop_ret_lo, e_rr_prop_ret_hi
               FROM	dr_estimates_rawvalues_at_date(CAST('", limiting_date, "' AS DATE), ", start_year, ")")

  # Extract data from the database
  estimates <- sqlQuery(channel,sql)


  # rename variables to include version using plyr
  estimates <- estimates %>%
               rename_at(vars(starts_with("e_rr_prop")),
                         funs(paste0(.,"_" , version)))

  return(estimates)
}


# Extract data from the database
ch <- odbcDriverConnect(connection_string)

estimates_series_1 <- get_historical_estimates(ch, "series1", series_1_date, series_1_startyear)
estimates_series_2 <- get_historical_estimates(ch, "series2", series_2_date, series_2_startyear)
estimates_series_3 <- get_historical_estimates(ch, "series3", series_3_date, series_3_startyear)

# get list of countries
countries <- sqlQuery(ch, "SELECT country FROM view_TME_master_report_country ORDER BY country")

close(ch)

# combine the three series into a single wider one called changes
dr_estimates <- estimates_series_1 %>%
                full_join(estimates_series_2) %>%
                full_join(estimates_series_3)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Define graph layout ----
#
# Plot 4 sets of graphs (incidence, TB/HIV incidence, mortality excl HIV, TB/HIV mortality)
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

plot_dr <- function(df){

  #plot incidence (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3

  #note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)


  print(
        ggplot(data = df) +

          geom_pointrange(aes(x=year,
                              y=e_rr_prop_new_series1,
                              ymin=e_rr_prop_new_lo_series1,
                              ymax=e_rr_prop_new_hi_series1),
                          colour="blue") +

          geom_pointrange(aes(x=year,
                              y=e_rr_prop_ret_series1,
                              ymin=e_rr_prop_ret_lo_series1,
                              ymax=e_rr_prop_ret_hi_series1),
                          colour="blue") +

          geom_pointrange(aes(x=year,
                              y=e_rr_prop_new_series2,
                              ymin=e_rr_prop_new_lo_series2,
                              ymax=e_rr_prop_new_hi_series2),
                          colour="gray",
                          size = 1.3) +

          geom_pointrange(aes(x=year,
                              y=e_rr_prop_ret_series2,
                              ymin=e_rr_prop_ret_lo_series2,
                              ymax=e_rr_prop_ret_hi_series2),
                          colour="gray",
                          size = 1.3) +

          geom_pointrange(aes(x=year,
                              y=e_rr_prop_new_series3,
                              ymin=e_rr_prop_new_lo_series3,
                              ymax=e_rr_prop_new_hi_series3),
                          colour="black") +

          geom_pointrange(aes(x=year,
                              y=e_rr_prop_ret_series3,
                              ymin=e_rr_prop_ret_lo_series3,
                              ymax=e_rr_prop_ret_hi_series3),
                          colour="black") +

          facet_wrap(~country, scales="free_y") +
          xlab("") +
          ylab("Proportion of cases with rifampicin resistance") +
          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="bottom"))
  }


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")

#setwd(outfolder)

plot_blocks_to_pdf(dr_estimates,
                   countries,
                   paste0(outfolder, "/", file_name_dr),
                   plot_function = plot_dr)

# clear the decks
rm(list=ls())





