# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at trends in male:female notification ratio
# using data from previous years and
# the latest data reported to us
# Hazim Timimi, June 2019
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
# The next two are set using set_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

scriptsfolder <- getSrcDirectory(function(x) {x})  # See http://stackoverflow.com/a/30306616

setwd(scriptsfolder)

source("set_environment.r")  #particular to each person so this file is in the ignore list

file_name     <- paste0(outfolder, "mf_graphs_", Sys.Date(), ".pdf")


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
# The query combines data from the master notification view with latest data
# reported as retreived from the dcf views (dcf = data collection form)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sql <- "WITH notifs AS
(
  SELECT  country, year, c_newinc,
  newrel_f014, newrel_f15plus, newrel_fu,
  newrel_m014, newrel_m15plus, newrel_mu
  FROM	view_TME_master_notification
  WHERE	year BETWEEN 2013 AND (SELECT max(year - 1) from dcf.latest_notification)
  UNION ALL
  SELECT  country, year, c_newinc,
  newrel_f014, newrel_f15plus, newrel_fu,
  newrel_m014, newrel_m15plus, newrel_mu
  FROM	dcf.latest_notification
  )
  SELECT	country, year, c_newinc,
  ROUND(
  CASE
  --check denominator is not null or zero
  WHEN (	ISNULL(newrel_f014,0)  +
  ISNULL(newrel_f15plus,0)  +
  ISNULL(newrel_fu,0)) = 0  THEN NULL

  --check numerator is not null
  WHEN COALESCE(newrel_m014,
  newrel_m15plus,
  newrel_mu) is null THEN NULL

  ELSE	(
  ISNULL(newrel_m014,0)  +
  ISNULL(newrel_m15plus,0)  +
  ISNULL(newrel_mu,0)
  )  * 1.00
  /
  (
  ISNULL(newrel_f014,0)  +
  ISNULL(newrel_f15plus,0)  +
  ISNULL(newrel_fu,0)
  )
  END
  ,2) AS mf_ratio
  FROM	notifs
  ORDER BY country, year;"


# Extract data from the database
channel <- odbcDriverConnect(connection_string)
data_to_plot <- sqlQuery(channel,sql)

close(channel)

# Find countries where the ratio has flipped either side of 1
# - - - - - - - - - - -

countries <-  data_to_plot %>%
              filter(year >= 2017 & !is.na(mf_ratio)) %>%
              arrange(country, year) %>%
              group_by(country) %>%
              mutate(mf_prev = lag(mf_ratio)) %>%
              ungroup() %>%
              filter((mf_prev > 1 & mf_ratio < 1) | (mf_prev < 1 & mf_ratio > 1) ) %>%
              # remove countries with notifications < 100
              filter(c_newinc >= 100) %>%
              select(country)


data_to_plot <- data_to_plot %>%
                filter(country %in% countries$country)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

graphs <- data_to_plot %>%
          ggplot(aes(x=year, y=mf_ratio, ymin=0)) +
          geom_line(colour=I("blue")) +
          facet_wrap(~country, scales="free_y") +
          xlab("year") + ylab("mf ratio") +
          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="bottom")



ggsave(filename=file_name,
       plot=graphs,
       width=11,
       height=7)

# clear the decks
rm(list=ls())





