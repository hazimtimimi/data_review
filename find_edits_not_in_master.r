# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Find variables where the value in the data collection form view are different
# from the value in the master view
#
# This is especially useful for high-burden countries featured in tables in the global report
#	to make sure main report tables and the country profiles are in sync.
#
# Hazim Timimi, December 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# clear the decks
rm(list=ls())

# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# scriptsfolder:      Folder containing these scripts
#
# The next is set using set_environment.r
#
# connection_string:  ODBC connection string to the global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

scriptsfolder <- getSrcDirectory(function(x) {x})  # See http://stackoverflow.com/a/30306616
setwd(scriptsfolder)

source("set_environment.r")  #particular to each person so this file is in the ignore list

# load packages ----

library(RODBC)
library(dplyr)
library(tidyr)


# Functions ----

Null_to_minus_1 <- function(x){
  # Convert a null (NA) to -1 to make comparisons possible
  x <- ifelse(is.na(x),-1,x)
  return(x)
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Extract data from the database
channel <- odbcDriverConnect(connection_string)

# latest notifications reported in the dcf view (dcf = data collection form)
notifs_dcf <- sqlQuery(channel, "SELECT *
                                FROM dcf.latest_notification",
                       stringsAsFactors = FALSE)

# notifications already in the master view for the same year
notifs_master <- sqlQuery(channel,
                      paste("SELECT *
                            FROM view_TME_master_notification
                            WHERE year = (SELECT MAX(year) FROM dcf.latest_notification)"),
                       stringsAsFactors = FALSE)

close(channel)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Convert data to long format (Hadley Wickham's "tidy" format)  ----
# (called unpivoting in SQL-Server)
# Use iso2 in addition to variable name as row keys for DCF
# and keep country in row keys for master for ease of reading results
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

notifs_dcf_long <- notifs_dcf %>%
                    # restrict to the same variable names as in notifs_master
                    select(which(names(notifs_dcf) %in% names(notifs_master))) %>%
                    # drop country, year and remarks
                    select(-country, -year, -starts_with("remarks")) %>%
                    gather(key=var_name, value=value_dcf, -iso2) %>%
                    # convert nulls to -1
                    mutate(value_dcf=Null_to_minus_1(value_dcf))


notifs_master_long <- notifs_master %>%
                      # remove un-needed variables before "tidying"
                      # so just restrict to the same variable names as in notifs_dcf
                      select(which(names(notifs_master) %in% names(notifs_dcf))) %>%
                      gather(key=var_name, value=value_master, -iso2, -country, -year ) %>%
                      # convert to strings to make comparisons easier, including for NAs
                      mutate(value_master=Null_to_minus_1(value_master))


# Join the two and look for differences  ----
# - - - - - - - - - - -

notifs_diff <- notifs_master_long %>%
                inner_join(notifs_dcf_long) %>%
                filter( value_master != value_dcf) %>%
                arrange(country, var_name)


View(notifs_diff)
