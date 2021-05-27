# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at trends in male:female notification ratio
# using data from previous years and
# the latest data reported to us
# Hazim Timimi, June 2019
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# file_name:          Name of the PDF output file
#
# The next two are set using set_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


source("set_environment.r")  #particular to each person so this file is in the ignore list
source("set_plot_themes.r")


file_name     <- paste0(outfolder, "mf_graphs_", Sys.Date(), ".pdf")

# Notifications threshold (exclude countries below the threshold)
notif_threshold <- 1000


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
              filter(year >= 2018 & !is.na(mf_ratio)) %>%
              arrange(country, year) %>%
              group_by(country) %>%
              mutate(mf_prev = lag(mf_ratio)) %>%
              ungroup() %>%
              filter((mf_prev > 1 & mf_ratio < 1) | (mf_prev < 1 & mf_ratio > 1) ) %>%
              # remove countries with notifications less than the threshold
              filter(c_newinc >= notif_threshold) %>%
              select(country)


data_to_plot <- data_to_plot %>%
                filter(country %in% countries$country)

if(nrow(data_to_plot) == 0) stop("No countries with flipped mf ratio! Stopping here...")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

graphs <- data_to_plot %>%
          ggplot(aes(x=year, y=mf_ratio, ymin=0)) +
          geom_line(colour=I("blue")) +
          facet_wrap(~country,
                     scales="free_y",
                     # Use the labeller function to make sure long country names are wrapped in panel headers
                     labeller = label_wrap_gen(width = 24)) +
          xlab("year") + ylab("mf ratio") +
          expand_limits(y=0) +
          theme_gtbr_2021(base_size=8, axis_text_size = 6)



ggsave(filename=file_name,
       plot=graphs,
       width=11,
       height=7)






