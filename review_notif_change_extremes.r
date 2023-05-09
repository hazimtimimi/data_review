# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Find countries at the extremes of % change in notifications for the
# latest year compared to the previous year.
# Shown lists in console and as maps for new and relapse cases, TB/HIV cases
# and RR-TB (without FQR) cases
# Hazim Timimi, April 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# The next two are set using set_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


source("set_environment.r")  #particular to each person so this file is in the ignore list

# load packages ----
library(RODBC)
library(whomap)
library(dplyr)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
#
# I prefer to do this via SQL, but could be done of course with the pure views
# and some R jiggery pokery
#
# The query combines data from the master notification view with latest data
# reported as retreived from the dcf views (dcf = data collection form)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  sql <- "SELECT  iso2, year, c_newinc, conf_rr_nfqr, newrel_hivpos
	        FROM    dcf.latest_notification
          UNION ALL
          SELECT  iso2, year, c_newinc, conf_rr_nfqr, newrel_hivpos
          FROM	  view_TME_master_notification
        	WHERE	  year = (SELECT max(year - 1) from dcf.latest_notification)"


# Extract data from the database
channel <- odbcDriverConnect(connection_string)
recent_data <- sqlQuery(channel,sql)

# get list of countries
countries_ref <- sqlQuery(channel,
                          "SELECT iso2, iso3, country
                           FROM view_TME_master_report_country")

close(channel)

# Link iso3 and country name to recent_data
recent_data <- recent_data %>%
  inner_join(countries_ref, by = "iso2")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Calculate % change for a variable
# Define a minimum denominator size to exclude countries with small numbers
calculate_pct_change <- function(df, var_name, min_denominator) {

  output <- df %>%
    select(country, iso3, year, latest_val = !!sym(var_name)) %>%
    arrange(iso3, year) %>%
    mutate(previous_val = lag(latest_val)) %>%
    filter(year == max(year)) %>%
    mutate(change_pct = ifelse(!is.na(previous_val) & previous_val > min_denominator,
                               (latest_val - previous_val) * 100 / previous_val,
                               NA))
  return(output)
}



# identify the n extreme values either end of a vector -------
cut_extremes <- function(df, var_name, n) {

  output <- df %>%
    mutate(var = cut(!!sym(var_name),
                     # Use dplyr functions first() and nth() to identify break points
                     breaks = c(first(!!sym(var_name), order_by = !!sym(var_name), na_rm = TRUE),
                                nth(!!sym(var_name), n+1, order_by = !!sym(var_name), na_rm = TRUE),
                                0,
                                nth(!!sym(var_name), n, order_by = desc(!!sym(var_name)), na_rm = TRUE),
                                first(!!sym(var_name), order_by = desc(!!sym(var_name)), na_rm = TRUE)),
                     labels = c(paste('Top', n, 'declining'), 'Declining', 'Increasing', paste('Top', n, 'increasing')),
                     right = FALSE,
                     include.lowest = TRUE)) %>%
    arrange(!!sym(var_name))

  return(output)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the maps  -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Change in notifications (minimum of 1000 cases in previous year) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
newrel_change_extremes <- recent_data %>%
  calculate_pct_change("c_newinc", 1000) %>%
  select(country, iso3, newrel_change = change_pct) %>%
  # Assign the categories for the map - top 5 extremes
  cut_extremes("newrel_change", 5)

# Draw the map
newrel_change_extremes %>%
  whomap(colours = RColorBrewer::brewer.pal(4, "RdYlBu"),
         map.title = "Notification change extremes (numbers > 1000)",
         legend.title = "Notification change",
         na.col = "#FFFFFF",
         water.col = "#FFFFFF")

ggsave(paste0(outfolder, "notif_change_map_", Sys.Date(), ".pdf"), width = 12, height = 8)

# Display in console the top 10 and bottom 10 extremes (beyond the 5 shown in the map)
newrel_change_extremes %>% head(10) %>% print()
newrel_change_extremes %>% filter(!is.na(newrel_change)) %>% tail(10) %>% print()



# Change in RR-TB without FQR (minimum of 100 cases in previous year) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rr_nfqr_change_extremes <- recent_data %>%
  calculate_pct_change("conf_rr_nfqr", 100) %>%
  select(country, iso3, rr_nfqr_change = change_pct) %>%
  # Assign the categories for the map - top 5 extremes
  cut_extremes("rr_nfqr_change", 5)

# Draw the map
rr_nfqr_change_extremes %>%
  whomap(colours = RColorBrewer::brewer.pal(4, "RdYlBu"),
         map.title = "RR-TB (not FQR) detection change extremes (numbers > 100)",
         legend.title = "RR-TB detection change",
         na.col = "#FFFFFF",
         water.col = "#FFFFFF")

ggsave(paste0(outfolder, "rr_nfqr_change_map_", Sys.Date(), ".pdf"), width = 12, height = 8)

# Display in console the top 10 and bottom 10 extremes (beyond the 5 shown in the map)
rr_nfqr_change_extremes %>% head(10) %>% print()
rr_nfqr_change_extremes %>% filter(!is.na(rr_nfqr_change)) %>% tail(10) %>% print()


# Change in TB/HIV  (minimum of 1000 cases in previous year) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
tbhiv_change_extremes <- recent_data %>%
  calculate_pct_change("newrel_hivpos", 1000) %>%
  select(country, iso3, hivtest_pos_change = change_pct) %>%
  # Assign the categories for the map - top 5 extremes
  cut_extremes("hivtest_pos_change", 5)

# Draw the map
tbhiv_change_extremes %>%
  whomap(colours = RColorBrewer::brewer.pal(4, "RdYlBu"),
         map.title = "TB/HIV change extremes (numbers > 1000)",
         legend.title = "TB/HIV detection change",
         na.col = "#FFFFFF",
         water.col = "#FFFFFF")

ggsave(paste0(outfolder, "tbhiv_change_map_", Sys.Date(), ".pdf"), width = 12, height = 8)

# Display in console the top 10 and bottom 10 extremes (beyond the 5 shown in the map)
tbhiv_change_extremes %>% head(10) %>% print()
tbhiv_change_extremes %>% filter(!is.na(hivtest_pos_change)) %>% tail(10) %>% print()

