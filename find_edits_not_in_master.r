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
# The next is set using set_environment.r
#
# connection_string:  ODBC connection string to the global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

channel <- odbcDriverConnect(connection_string)

# A. Latest data from the dcf views (dcf = data collection form)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# notifications
notifs_dcf <- sqlQuery(channel, "SELECT * FROM dcf.latest_notification",
                       stringsAsFactors = FALSE)


# treatment outcomes
outcomes_dcf <- sqlQuery(channel, "SELECT * FROM dcf.latest_outcomes",
                       stringsAsFactors = FALSE)

# mdr/xdr treatment outcomes
mdr_xdr_outcomes_dcf <- sqlQuery(channel, "SELECT * FROM dcf.latest_mdr_xdr_outcomes",
                       stringsAsFactors = FALSE)

# strategy
strategy_dcf <- sqlQuery(channel, "SELECT * FROM dcf.latest_strategy",
                       stringsAsFactors = FALSE)

# budget
budget_dcf <- sqlQuery(channel, "SELECT * FROM dcf.latest_budget",
                       stringsAsFactors = FALSE)

# expenditures and services
expenditure_dcf <- sqlQuery(channel, "SELECT * FROM dcf.latest_expenditure_services",
                       stringsAsFactors = FALSE)

# B. Older records already in the master views (match years to dcf years)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


notifs_master <- sqlQuery(channel,
                      paste("SELECT *
                            FROM view_TME_master_notification
                            WHERE year = (SELECT MAX(year) FROM dcf.latest_notification)"),
                       stringsAsFactors = FALSE)

notifs_hiv_tpt_cmplt_master <- sqlQuery(channel, "SELECT country, iso2, year,
                                      hiv_all_tpt_started, hiv_all_tpt_completed
                            FROM view_TME_master_notification
                            WHERE year = (SELECT MAX(year)-1 FROM dcf.latest_strategy)",
                            stringsAsFactors = FALSE)

# dr surveillance records are in the dcf notifications view!
dr_surveillance_master <- sqlQuery(channel,
                          paste("SELECT *
                                FROM view_TME_master_dr_surveillance
                                WHERE year = (SELECT MAX(year) FROM dcf.latest_notification) AND all_areas_covered=1"),
                           stringsAsFactors = FALSE)

outcomes_master <- sqlQuery(channel,
                      paste("SELECT *
                            FROM view_TME_master_outcomes
                            WHERE year = (SELECT MAX(year) FROM dcf.latest_outcomes)"),
                       stringsAsFactors = FALSE)

mdr_xdr_outcomes_master <- sqlQuery(channel,
                            paste("SELECT *
                                  FROM view_TME_master_outcomes
                                  WHERE year = (SELECT MAX(year) FROM dcf.latest_mdr_xdr_outcomes)"),
                             stringsAsFactors = FALSE)


strategy_master <- sqlQuery(channel, "SELECT *
                            FROM view_TME_master_strategy
                            WHERE year = (SELECT MAX(year) FROM dcf.latest_strategy)",
                       stringsAsFactors = FALSE)

strategy_tpt_cmplt_master <- sqlQuery(channel, "SELECT country, iso2, year,
                                      newinc_con_prevtx, newinc_con_prevtx_cmplt
                            FROM view_TME_master_strategy
                            WHERE year = (SELECT MAX(year)-1 FROM dcf.latest_strategy)",
                            stringsAsFactors = FALSE)

budget_master <- sqlQuery(channel, "SELECT *
                          FROM view_TME_master_budget_expenditure
                          WHERE year = (SELECT MAX(year) FROM dcf.latest_budget)",
                       stringsAsFactors = FALSE)

expenditure_master <- sqlQuery(channel, "SELECT *
                          FROM view_TME_master_budget_expenditure
                          WHERE year = (SELECT MAX(year) FROM dcf.latest_expenditure_services)",
                       stringsAsFactors = FALSE)

# And a one-off for 2021 data collection year
covid_unhlm_master <- sqlQuery(channel, "SELECT *
                          FROM view_TME_master_covid_unhlm
                          WHERE year = 2021",
                       stringsAsFactors = FALSE)

close(channel)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Convert data to long format (Hadley Wickham's "tidy" format)  ----
# (called unpivoting in SQL-Server)
# Use iso2 in addition to variable name as row keys for DCF
# and keep country in row keys for master for ease of reading results
# Wrap it all up in a function
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

compare_views <- function(dcf_view, master_view){


  dcf_view_long <- dcf_view %>%
                    # restrict to the same variable names as in master_view
                    select(which(names(dcf_view) %in% names(master_view))) %>%
                    # drop country, year and remarks
                    select(-country, -year, -starts_with("remarks")) %>%
                    gather(key=var_name, value=value_dcf, -iso2) %>%
                    # convert nulls to -1
                    mutate(value_dcf=Null_to_minus_1(value_dcf))

  master_view_long <- master_view %>%
                      # remove un-needed variables before "tidying"
                      # so just restrict to the same variable names as in dcf_view
                      select(which(names(master_view) %in% names(dcf_view))) %>%
                      gather(key=var_name, value=value_master, -iso2, -country, -year ) %>%
                      # convert nulls to -1
                      mutate(value_master=Null_to_minus_1(value_master))

  # Join the two long views and look for differences  ----
  # - - - - - - - - - - -

  views_diff <- master_view_long %>%
                inner_join(dcf_view_long) %>%
                filter( as.numeric(value_master) != as.numeric(value_dcf)) %>%
                arrange(country, var_name)

  return(views_diff)
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Save file to CSV, with date in filename  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
to_csv <- function(df, filename) {

  write.csv(df, file = paste0(outfolder, filename, Sys.Date(), ".csv"), row.names = FALSE)

}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Now do the comparisons, and write results to CSV   ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Notifications
# - - - - - - - -

# Exclude some of the auto-calculated age/sex variables as these are not auto-calculated in the DCF view
notifs_dcf <- notifs_dcf %>%
  select(-newrel_f514, -newrel_f014, -newrel_f1524, -newrel_m514, -newrel_m014, -newrel_m1524)

notifs_master <- notifs_master %>%
  select(-newrel_f514, -newrel_f014, -newrel_f1524, -newrel_m514, -newrel_m014, -newrel_m1524)


# Move the HIV TPT completion variables from the DCF into a separate data frame
notifs_hiv_tpt_cmplt_dcf <- notifs_dcf %>%
  select(country, iso2, year,
         hiv_all_tpt_started,
         hiv_all_tpt_completed) %>%
  mutate(year = year - 1) %>%
  # remove empty rows
  filter(!is.na(hiv_all_tpt_started) | !is.na(hiv_all_tpt_completed))

notifs_dcf <- notifs_dcf %>%
  select(-hiv_all_tpt_started, -hiv_all_tpt_completed)

notifs_master <- notifs_master %>%
  select(-hiv_all_tpt_started, -hiv_all_tpt_completed)


notifs_diff <- compare_views(dcf_view = notifs_dcf, master_view = notifs_master)
to_csv(notifs_diff, "notifs_diff")

notifs_hiv_tpt_cmplt_diff <- compare_views(dcf_view = notifs_hiv_tpt_cmplt_dcf, master_view = notifs_hiv_tpt_cmplt_master)
to_csv(notifs_hiv_tpt_cmplt_diff, "notifs_hiv_tpt_cmplt_diff")




# DR surveillance
# - - - - - - - -


# latest dr surveillance records are in the dcf notifications view!
dr_surveillance_diff <- compare_views(dcf_view = notifs_dcf, master_view = dr_surveillance_master)
to_csv(dr_surveillance_diff, "dr_surveillance_diff")





# Outcomes
# - - - - - - - -

# treatment success rates are not rounded in the outcomes dcf views, so round them now to match
# properly with master views

outcomes_dcf$c_new_tsr <- round(outcomes_dcf$c_new_tsr)
outcomes_dcf$c_ret_tsr <- round(outcomes_dcf$c_ret_tsr)
outcomes_dcf$c_tbhiv_tsr <- round(outcomes_dcf$c_tbhiv_tsr)

mdr_xdr_outcomes_dcf$c_mdr_tsr <- round(mdr_xdr_outcomes_dcf$c_mdr_tsr)
mdr_xdr_outcomes_dcf$c_xdr_tsr <- round(mdr_xdr_outcomes_dcf$c_xdr_tsr)


# Don't bother with the c_*_neval variables in the outcome tables as they are auto-calculated in master

outcomes_dcf <- select(outcomes_dcf, -contains("_neval"))
outcomes_master <- select(outcomes_master, -contains("_neval"))

mdr_xdr_outcomes_dcf <- select(mdr_xdr_outcomes_dcf, -contains("_neval"))
mdr_xdr_outcomes_master <- select(mdr_xdr_outcomes_master, -contains("_neval"))



outcomes_diff <- compare_views(dcf_view = outcomes_dcf, master_view = outcomes_master)
to_csv(outcomes_diff, "outcomes_diff")

mdr_xdr_outcomes_diff <- compare_views(dcf_view = mdr_xdr_outcomes_dcf, master_view = mdr_xdr_outcomes_master)
to_csv(mdr_xdr_outcomes_diff, "mdr_xdr_outcomes_diff")



# Strategy and TPT completion
# - - - - - - - - - - - - - -


# Move the TPT completion variables for ym2 to a separate DCF dataframe
strategy_tpt_cmplt_dcf <- strategy_dcf %>%
  select(country, iso2, year,
         newinc_con_prevtx_cmplt) %>%
  mutate(year = year - 1) %>%
  # remove empty rows
  filter(!is.na(newinc_con_prevtx_cmplt))


strategy_dcf <- strategy_dcf %>%
  select(-prevtx_cmplt_data_available, newinc_con_prevtx_cmplt)

strategy_master <- strategy_master %>%
  select(-newinc_con_prevtx_cmplt)



strategy_diff <- compare_views(dcf_view = strategy_dcf, master_view = strategy_master)
to_csv(strategy_diff, "strategy_diff")

strategy_tpt_cmplt_diff <- compare_views(dcf_view = strategy_tpt_cmplt_dcf, master_view = strategy_tpt_cmplt_master)
to_csv(strategy_tpt_cmplt_diff, "strategy_tpt_cmplt_diff")



# Finance
# - - - - - - - -


budget_diff <- compare_views(dcf_view = budget_dcf, master_view = budget_master)
# remove empty rows (master = -1 AND dcf = 0)
budget_diff <- budget_diff %>% filter(!(value_master == -1 & value_dcf == 0))
to_csv(budget_diff, "budget_diff")

expenditure_diff <- compare_views(dcf_view = expenditure_dcf, master_view = expenditure_master)
# remove empty rows (master = -1 AND dcf = 0)
expenditure_diff <- expenditure_diff %>% filter(!(value_master == -1 & value_dcf == 0))
to_csv(expenditure_diff, "expenditure_diff")



# MAF
# - - - - - - - -


# And for 2021
covid_unhlm_dcf <- strategy_dcf %>%
  select(country, iso2, year, annual_report_published, ms_review, ms_review_civil_soc) %>%
  mutate(year = year + 1)

covid_unhlm_diff <- compare_views(dcf_view = covid_unhlm_dcf, master_view = covid_unhlm_master)
to_csv(covid_unhlm_diff, "covid_unhlm_diff")

