# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Compare estimates of incidence and mortality from three separate
# Global TB reports
# Hazim Timimi, December 2014, based on original code from Babis Sismanidis
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
# g_whoregion:        WHO regional code to filter charts
#                     (if not specified then charts are produced for all countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


source("set_environment.r")  #particular to each person so this file is in the ignore list


file_name_inc      <- paste0(g_whoregion, "incidence_graphs_", Sys.Date(), ".pdf")
file_name_inc_hiv  <- paste0(g_whoregion, "inccidence_hiv_graphs_", Sys.Date(), ".pdf")
file_name_mort     <- paste0(g_whoregion, "mortality_graphs_", Sys.Date(), ".pdf")
file_name_mort_hiv <- paste0(g_whoregion, "mortality_hiv_graphs_", Sys.Date(), ".pdf")


# load packages ----
library(RODBC)
library(ggplot2)
library(dplyr)
library(stringr)


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
               e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi,
               e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
               e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo, e_mort_tbhiv_100k_hi
               FROM	epi_estimates_rawvalues_at_date(CAST('", limiting_date, "' AS DATE))")

  # Extract data from the database
  estimates <- sqlQuery(channel,sql)


  # rename variables -- shorten and add version using dplyr
  estimates <- estimates %>%
               rename_at(vars(starts_with("e_inc_100k")),
                         funs(paste0(str_replace(., "e_inc_100k" , "inc"),"_" , version))) %>%

               rename_at(vars(starts_with("e_inc_tbhiv_100k")),
                         funs(paste0(str_replace(., "e_inc_tbhiv_100k" , "inc_hiv"),"_" , version))) %>%

               rename_at(vars(starts_with("e_mort_exc_tbhiv_100k")),
                         funs(paste0(str_replace(., "e_mort_exc_tbhiv_100k" , "mort"),"_" , version))) %>%

               rename_at(vars(starts_with("e_mort_tbhiv_100k")),
                         funs(paste0(str_replace(., "e_mort_tbhiv_100k" , "mort_hiv"),"_" , version)))

  # estimates <- rename(estimates, c("e_inc_100k" = paste0("inc_",version),
  #                                    "e_inc_100k_lo" = paste0("inc_lo_",version),
  #                                    "e_inc_100k_hi" = paste0("inc_hi_",version),
  #                                    "e_inc_tbhiv_100k" = paste0("inc_hiv_",version),
  #                                    "e_inc_tbhiv_100k_lo" = paste0("inc_hiv_lo_",version),
  #                                    "e_inc_tbhiv_100k_hi" = paste0("inc_hiv_hi_",version),
  #                                    "e_mort_exc_tbhiv_100k" = paste0("mort_",version),
  #                                    "e_mort_exc_tbhiv_100k_lo" = paste0("mort_lo_",version),
  #                                    "e_mort_exc_tbhiv_100k_hi" = paste0("mort_hi_",version),
  #                                    "e_mort_tbhiv_100k" = paste0("mort_hiv_",version),
  #                                    "e_mort_tbhiv_100k_lo" = paste0("mort_hiv_lo_",version),
  #                                    "e_mort_tbhiv_100k_hi" = paste0("mort_hiv_hi_",version)))


  return(estimates)
}


# Extract data from the database
ch <- odbcDriverConnect(connection_string)

estimates_series_1 <- get_historical_estimates(ch, "series1", series_1_date)
estimates_series_2 <- get_historical_estimates(ch, "series2", series_2_date)
estimates_series_3 <- get_historical_estimates(ch, "series3", series_3_date)

# get list of countries
country_sql <- ifelse(g_whoregion == "",
                    "SELECT country FROM view_TME_master_report_country ORDER BY country",
                    paste0("SELECT country FROM view_TME_master_report_country WHERE g_whoregion = '", g_whoregion, "' ORDER BY country"))



countries <- sqlQuery(ch, country_sql)
close(ch)

# combine the three series into a single wider one called changes

changes <- merge(estimates_series_3, estimates_series_2, all.x=TRUE)
changes <- merge(changes, estimates_series_1, all.x=TRUE)

rm(list=c("estimates_series_1", "estimates_series_2", "estimates_series_3"))



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

plot_inc <- function(df){

  #plot incidence (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3

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
}

plot_inc_hiv <- function(df){

  # plot TB/HIV incidence (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3

  print(qplot(year, inc_hiv_series1, data=df, geom="line", colour=I("blue")) +
        geom_ribbon(aes(year,
                        ymin=inc_hiv_lo_series1,
                        ymax=inc_hiv_hi_series1),
                    fill=I("blue"), alpha=0.2) +

        geom_line(aes(year, inc_hiv_series2), colour=I("red")) +
        geom_ribbon(aes(year,
                        ymin=inc_hiv_lo_series2,
                        ymax=inc_hiv_hi_series2),
                    fill=I("red"), alpha=0.2) +

        geom_line(aes(year, inc_hiv_series3), colour=I("green")) +
        geom_ribbon(aes(year,
                        ymin=inc_hiv_lo_series3,
                        ymax=inc_hiv_hi_series3),
                    fill=I("green"), alpha=0.2) +
        facet_wrap(~country, scales="free_y") +
        xlab("") +
        ylab("HIV-positive TB incidence rate per 100 000 population per year") +
        expand_limits(y=0) +
        theme_bw(base_size=8) +
        theme(legend.position="bottom"))
}

plot_mort <- function(df){

  # plot HIV-negative mortality (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3

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
        ylab("Mortality (excluding TB/HIV) rate per 100 000 population per year") +
        expand_limits(y=0) +
        theme_bw(base_size=8) +
        theme(legend.position="none"))
}

plot_mort_hiv <- function(df){

  # plot HIV-positive TB mortality (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3

  print(qplot(year, mort_hiv_series1, data=df, geom="line", colour=I("blue")) +
        geom_ribbon(aes(year,
                        ymin=mort_hiv_lo_series1,
                        ymax=mort_hiv_hi_series1),
                    fill=I("blue"), alpha=0.2) +

        geom_line(aes(year, mort_hiv_series2), colour=I("red")) +
        geom_ribbon(aes(year,
                        ymin=mort_hiv_lo_series2,
                        ymax=mort_hiv_hi_series2),
                    fill=I("red"), alpha=0.2) +

        geom_line(aes(year, mort_hiv_series3), colour=I("green")) +
        geom_ribbon(aes(year,
                        ymin=mort_hiv_lo_series3,
                        ymax=mort_hiv_hi_series3),
                    fill=I("green"), alpha=0.2) +
        facet_wrap(~country, scales="free_y") +
        xlab("") +
        ylab("HIV-positive TB mortality rate per 100 000 population per year") +
        expand_limits(y=0) +
        theme_bw(base_size=8) +
        theme(legend.position="none"))

}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")

block_size <- ifelse(g_whoregion == "SEA", 8, 16)

plot_blocks_to_pdf(changes, countries, paste0(outfolder, file_name_inc),     plot_function = plot_inc, block_size)
plot_blocks_to_pdf(changes, countries, paste0(outfolder, file_name_inc_hiv), plot_function = plot_inc_hiv, block_size)
plot_blocks_to_pdf(changes, countries, paste0(outfolder, file_name_mort),    plot_function = plot_mort, block_size)
plot_blocks_to_pdf(changes, countries, paste0(outfolder, file_name_mort_hiv),plot_function = plot_mort_hiv, block_size)







