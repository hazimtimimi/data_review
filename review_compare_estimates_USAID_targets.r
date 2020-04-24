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


file_name_inc     <- paste0("inc_graphs_", Sys.Date(), ".pdf")
file_name_inc014  <- paste0("inc014_graphs_", Sys.Date(), ".pdf")
file_name_inc_dr  <- paste0("inc_dr_graphs_", Sys.Date(), ".pdf")


source("set_environment.r")  #particular to each person so this file is in the ignore list


# load packages ----
library(RODBC)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# Get Function to plot multiple graphs to multi-page PDF -----
source("plot_blocks_to_pdf.r")



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
               #convert year field to integers
               names_ptypes = list(year = integer()),
               values_to = "target",
               values_drop_na = TRUE)



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


# Have to do something different for 0-14 incidence because that is only ever done for the
# latest year of data available. In dcyear 2018 changed from doing this in wide format in the
# main epi estimates view to long format allowing for more diaggregations. Therefore there are
# two functions here, depending on the year

get_wide_014_estimates <- function(channel, limiting_date, year){

  sql <- paste0("SELECT iso2, year,
                e_inc_num_014 AS best, e_inc_num_014_lo AS lo, e_inc_num_014_hi AS hi
                FROM epi_estimates_rawvalues_at_date(CAST('", limiting_date, "' AS DATE))
                WHERE year = ", year)

  estimates <- sqlQuery(channel,sql)

  return(estimates)
}

get_long_014_estimates <- function(channel, limiting_date, year){

 sql <- paste0("SELECT	iso2, year, best, lo, hi
                FROM	estimates.estimates_rawvalues_at_date(CAST('", limiting_date, "' AS DATE))
                WHERE	measure = 'inc' AND
            		unit = 'num' AND
            		age_group = '0-14' AND
            		sex = 'a' AND
            		risk_factor = 'all' AND
            		year = ", year)

  estimates <- sqlQuery(channel,sql)

  return(estimates)
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
# Extract data from the database ----
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ch <- odbcDriverConnect(connection_string)


estimates_series_1 <- get_historical_estimates(ch, "series1", series_1_date)
estimates_series_2 <- get_historical_estimates(ch, "series2", series_2_date)
estimates_series_3 <- get_historical_estimates(ch, "series3", series_3_date)
estimates_series_4 <- get_historical_estimates(ch, "series4", series_4_date)


# get master list of countries
countries <- sqlQuery(ch, "SELECT iso2, country FROM view_TME_master_report_country")

# get notifications
notifs <- sqlQuery(ch, "SELECT iso2, year, c_newinc
                   FROM view_TME_master_notification
                   WHERE year >= 2000")

# get the rr incidence estimates -- since we only produce estimates for the latest year
# in each report it is easy to extract the data as a timeseries, even though it really isn't a
# timeseries since estimates of the previous years are null and void
dr_estimates <- sqlQuery(ch, "SELECT iso2, year, e_inc_rr_num, e_inc_rr_num_lo, e_inc_rr_num_hi
                         FROM view_TME_estimates_drtb_rawvalues
                         WHERE year >= 2015")

# get the number of rr patients started on treatment
rr_tx <- sqlQuery(ch, "SELECT iso2, year,
                  CASE WHEN COALESCE(unconf_rrmdr_tx, conf_rrmdr_tx) IS NULL THEN NULL
			                 ELSE ISNULL(unconf_rrmdr_tx, 0) + ISNULL(conf_rrmdr_tx, 0)
		              END AS rrmdr_tx
		              FROM view_TME_master_notification
                  WHERE year >= 2015")

# Get the incidence estimate for age 0-14
estimates_014_series_1 <- get_wide_014_estimates(ch, series_1_date, series_1_startyear)
estimates_014_series_2 <- get_wide_014_estimates(ch, series_2_date, series_2_startyear)

estimates_014_series_3 <- get_long_014_estimates(ch, series_3_date, series_3_startyear)
estimates_014_series_4 <- get_long_014_estimates(ch, series_4_date, series_4_startyear)

# Get notifications in children aged 0-14
notifs_014 <- sqlQuery(ch, paste0("SELECT iso2, year, c_new_014
                             FROM view_TME_master_notification
                             WHERE year >= ", series_1_startyear))


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
country_list <- countries %>%
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


# Plot the incidence graphs to PDF -------
plot_blocks_to_pdf(changes_targets,
                   country_list,
                   paste0(outfolder, file_name_inc),
                   plot_function = plot_inc)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Now do the plots for RR/MDR -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Combine the estimates with numbers started on treatement
dr_changes <- merge(dr_estimates, rr_tx, all.x = TRUE)

# Add the USAID targets
dr_changes_targets <- targets %>%
  filter(target_code== "tgt_dr") %>%
  select(-target_code) %>%
  merge(dr_changes, all = TRUE)

# Add country names
dr_changes_targets <- dr_changes_targets %>%
  inner_join(countries, by = "iso2")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Define DR graph layout ----
#
# Plot RR incidence estimates as fishbones, number started on treatment as
# a black line and the USAID country targets as black points
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

plot_dr_inc <- function(df){

  print(

   ggplot(data = df, mapping = aes(x=year, y=e_inc_rr_num, ymin=0, colour = I("#009E73"))) +

     geom_pointrange(aes(x=year, ymin=e_inc_rr_num_lo, ymax=e_inc_rr_num_hi)) +

     # Add the numbers started on treatment as a black line
     geom_line(aes(x=year, y=rrmdr_tx, colour=I("black"))) +

     # add the targets as dots
     geom_point(aes(x=year, y=target, colour=I("black"))) +

      # Use space separators for the y axis
      scale_y_continuous(labels = rounder) +

      facet_wrap(~country, scales="free_y") +
      xlab("") +
      ylab("DR-TB Incidence, notifications and targets (number per year)") +
      expand_limits(y=0) +
      theme_bw(base_size=8) +
      theme(legend.position="bottom")

  )
}

# Plot the DR incidence graphs to PDF -------
plot_blocks_to_pdf(dr_changes_targets,
                   country_list,
                   paste0(outfolder, file_name_inc_dr),
                   plot_function = plot_dr_inc)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Now do the plots for incidence in children aged 0-14 -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Combine the estimates into one long table
kids_changes <- rbind(estimates_014_series_1, estimates_014_series_2, estimates_014_series_3, estimates_014_series_4)

rm(list=ls(pattern = "^estimates_014_series"))

# Add the notifications
kids_changes <- merge(kids_changes, notifs_014, all.x = TRUE)

# Add the targets
kids_changes_targets <- targets %>%
  filter(target_code== "tgt_014") %>%
  select(-target_code) %>%
  merge(kids_changes, all = TRUE)

# Add country names
kids_changes_targets <- kids_changes_targets %>%
  inner_join(countries, by = "iso2")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Define kids graph layout ----
#
# Plot 0-14 incidence estimates as fishbones, number of children notified as
# a black line and the USAID country targets as black points
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

plot_014_inc <- function(df){

  print(

   ggplot(data = df, mapping = aes(x=year, y=best, ymin=0, colour = I("#009E73"))) +

     geom_pointrange(aes(x=year, ymin=lo, ymax=hi)) +

     # Add the numbers started on treatment as a black line
     geom_line(aes(x=year, y=c_new_014, colour=I("black"))) +

     # add the targets as dots
     geom_point(aes(x=year, y=target, colour=I("black"))) +

      # Use space separators for the y axis
      scale_y_continuous(labels = rounder) +

      facet_wrap(~country, scales="free_y") +
      xlab("") +
      ylab("Incidence, notifications and targets for childrenn aged 0-14 (number per year)") +
      expand_limits(y=0) +
      theme_bw(base_size=8) +
      theme(legend.position="bottom")

  )
}

# Plot the incidence graphs to PDF -------
plot_blocks_to_pdf(kids_changes_targets,
                   country_list,
                   paste0(outfolder, file_name_inc014),
                   plot_function = plot_014_inc)


