# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Compare estimates of incidence from the 4 most recent global TB reports
# with USAID priority country targets
# # Hazim Timimi
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# file_name_*:        Names of the PDF output files
#
# The next two are set using set_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# usaid_targets_file: Full path to Excelfile containing USAID targets
# usaid_targets_sheet:Name of the worksheet in the Excel file which has the targets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


file_name_inc       <- paste0("inc_graphs_", Sys.Date(), ".pdf")
file_name_inc014    <- paste0("inc014_graphs_", Sys.Date(), ".pdf")


source("set_environment.r")  #particular to each person so this file is in the ignore list


# load packages ----
library(RODBC)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the USAID target  ----
#
# This is in wide format (iso2, target_code, 2018, 2019, 2020, 2021, 2022)
# where the year columns are the target for that year for a given
# country/target_code combination
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

targets <- read_excel(path = usaid_targets_file,
                      sheet = usaid_targets_sheet,
                      col_names = TRUE)

# Switch to long format
targets <- targets %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "year",
               names_ptypes = list(year = integer()),
               values_to = "target",
               values_drop_na = TRUE)
  #convert year field to integers


# Store the list of country codes separately for later use
iso2_list <- targets %>% distinct(iso2)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the estimates  ----
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

  sql <- paste0("SELECT iso2, year,
               e_inc_num, e_inc_num_lo, e_inc_num_hi
               FROM	epi_estimates_rawvalues_at_date(CAST('", limiting_date, "' AS DATE))")

  # Extract data from the database
  estimates <- sqlQuery(channel,sql)


  # rename variables to include version using plyr
  estimates <- estimates %>%
    rename_at(vars(starts_with("e_inc")),
              list(~ paste0(., "_", version))) %>%   # new syntax because funs() deprecated
    rename_at(vars(starts_with("e_inc")),
              list(~str_replace(., "e_inc_num", "inc")))

  return(estimates)
}


# Extract data from the database
ch <- odbcDriverConnect(connection_string)


estimates_series_1 <- get_historical_estimates(ch, "series1", series_1_date)
estimates_series_2 <- get_historical_estimates(ch, "series2", series_2_date)
estimates_series_3 <- get_historical_estimates(ch, "series3", series_3_date)
estimates_series_4 <- get_historical_estimates(ch, "series4", series_4_date)


# get master list of countries
countries <- sqlQuery(ch, "SELECT iso2, country FROM view_TME_master_report_country")

# get notifications
notifs <- sqlQuery(ch, "SELECT iso2, year, c_newinc FROM view_TME_master_notification WHERE year >= 2000")

close(ch)

# combine the three series into a single wider one called changes

changes <- merge(estimates_series_4, estimates_series_3, all.x=TRUE)
changes <- merge(changes, estimates_series_2, all.x=TRUE)
changes <- merge(changes, estimates_series_1, all.x=TRUE)

rm(list=ls(pattern = "^estimates_series"))

# Add the notifications
changes <- merge(changes, notifs, all.x = TRUE)

# Add the targets
changes_targets <- targets %>%
  filter(target_code== "tgt_newinc") %>%
  select(-target_code) %>%
  merge(changes, all = TRUE)

# Add country names
changes_targets <- changes_targets %>%
  inner_join(countries, by = "iso2")

# Create the country list for the plot_blocks_to_pdf() function
countries <- countries %>%
  mutate(iso2 = as.character(iso2)) %>%
  inner_join(iso2_list) %>%
  mutate(country = as.character(country)) %>%
  select(country)



# Simple rounding function that returns a string rounded to the nearest integer and
# uses a space as the thousands separator as per WHO standard.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rounder <- function(x) {

    ifelse(is.na(x), NA,
           formatC(round(x,0), big.mark=" ", format="d")
           )
}


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

        # add notifications as a black line
        geom_line(aes(year, c_newinc), colour=I("black")) +

        # add the targets as dots
        geom_point(aes(year, target), colour=I("black")) +

        # Use space separators for the y axis
        scale_y_continuous(labels = rounder) +

        facet_wrap(~country, scales="free_y") +
        xlab("") +
        ylab("Incidence, notifications and targets (number per year)") +
        expand_limits(y=0) +
        theme_bw(base_size=8) +
        theme(legend.position="bottom"))
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")


plot_blocks_to_pdf(changes_targets, countries, paste0(outfolder, file_name_inc), plot_function = plot_inc)


