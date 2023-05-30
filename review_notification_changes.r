# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at % and absolute change in case notification rate using data from previous years and
# the latest data reported to us
# Hazim Timimi, November 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# file_name_pcnt:     Name of the PDF output file for % changes
# file_name_delta:    Name of the PDF output file for changes in absolute numers
#
# start_year:         Starting year for the graphs
# minimum_notifs:     Minimum number of notifications reported in the data collection form
#                     for the coutnry to be included in the graphs
#
# The next two are set using set_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



source("set_environment.r")  #particular to each person so this file is in the ignore list
source("set_plot_themes.r")

file_name_pcnt <- paste0(outfolder, "notif_change_pcnt_", Sys.Date(), ".pdf")
file_name_delta <- paste0(outfolder, "notif_change_delta_", Sys.Date(), ".pdf")
file_name_cnr <- paste0(outfolder, "notif_cnr_", Sys.Date(), ".pdf")


# Define list of regions in SQL format if we don't want to plot all countries
# (If not keep it as an empty string)

region_filter <- ""  #WHERE g_whoregion IN ('AFR', 'EMR','SEA', 'WPR')"

start_year <- 2015
minimum_notifs <- 1000


# load packages ----
library(dplyr)
library(RODBC)
library(ggplot2)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Extract data from the database
channel <- odbcDriverConnect(connection_string)

# latest notifications reported in the dcf views (dcf = data collection form)
notifs_dcf <- sqlQuery(channel,
                      paste("SELECT	dcf.latest_notification.country, dcf.latest_notification.year, c_newinc,

                       CASE WHEN (ISNULL(e_pop_num, 0) > 0) AND (c_newinc IS NOT NULL  )
                              THEN 100000.00 * c_newinc / e_pop_num
                            ELSE NULL
                       END AS c_newinc_100k

                       FROM dcf.latest_notification

                       INNER JOIN view_TME_estimates_population ON
                          view_TME_estimates_population.iso2 = dcf.latest_notification.iso2 AND
                          view_TME_estimates_population.year = dcf.latest_notification.year",
                            region_filter),
                       stringsAsFactors = FALSE)

# notifications reported in previous years
notifs_historic <- sqlQuery(channel,
                      paste("SELECT	view_TME_master_notification.country, view_TME_master_notification.year, c_newinc,

                            CASE WHEN (ISNULL(e_pop_num, 0) > 0) AND (c_newinc IS NOT NULL  )
                                THEN 100000.00 * c_newinc / e_pop_num
                                ELSE NULL
                            END AS c_newinc_100k

                            FROM view_TME_master_notification

                            INNER JOIN view_TME_estimates_population ON
                                view_TME_estimates_population.iso2 = view_TME_master_notification.iso2 AND
                                view_TME_estimates_population.year = view_TME_master_notification.year
                            WHERE view_TME_master_notification.year BETWEEN ", start_year,
                            " AND (SELECT max(year - 1) from dcf.latest_notification)"),
                       stringsAsFactors = FALSE)

close(channel)


# Calculate % change in notifications  ----
# - - - - - - - - - - -

# Identify countries exceeding notification threshold to show in the output
countries <- notifs_dcf %>%
              filter(c_newinc >= minimum_notifs) %>%
              select(country) %>%
              arrange(country)


# Combine the dcf and historic notifications and do the maths
notifs <- union(notifs_historic, notifs_dcf) %>%
          arrange(country, year) %>%
          group_by(country) %>%
          mutate(c_newinc_100k_prev = lag(c_newinc_100k)) %>%
          ungroup() %>%
          mutate(c_newinc_100k_pcnt = ifelse(c_newinc_100k_prev == 0,
                                        NA,
                                        (c_newinc_100k - c_newinc_100k_prev) * 100 / c_newinc_100k_prev),
                 c_newinc_100k_delta = c_newinc_100k - c_newinc_100k_prev)

# capture final year in a variable
dcf_year <- first(notifs_dcf$year)


# Define graph layout ----
# - - - - - - - - - - -

plot_faceted_cnr <- function(df){

  # Blue line  = case notification rate
  # Use a log scale to see steady rates of change

  graphs <- qplot(year, c_newinc_100k, data=df, geom="line", colour=I("blue")) +

    facet_wrap(~country,
               scales="free_y",
    # Use the labeller function to make sure long country names are wrapped in panel headers
    labeller = label_wrap_gen(width = 25)) +

    scale_x_continuous(name="", breaks = c(start_year, start_year+2, start_year+4)) +

    ylab("Case notification rate (log scale)") +

    scale_y_log10() +

    expand_limits(y=0) +

    theme_gtbr_2021(base_size=8, axis_text_size = 6) +

    # Add a shaded ribbon showing glm mean and standard errors
    # But do this on all years prior to 2020 (COVID) to get a visual hint
    # as to whether the final points deviates recent trends
    # (although I'm not sure if this is sound methodologically ...)
    geom_smooth(data = filter(df, year < 2020),
                method="glm",
                colour = "black",
                size=0.5)


  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)

}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")


plot_blocks_to_pdf(notifs, countries, file_name_cnr, plot_function = plot_faceted_cnr)






