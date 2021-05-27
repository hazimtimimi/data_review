# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Compare provisional and final notifications
# Hazim Timimi, May 2021
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

# Define list of regions in SQL format if we don't want to plot all countries
# (If not keep it as an empty string)

region_filter <- "AND iso2 IN (SELECT iso2 FROM view_TME_master_report_country
                              WHERE g_whoregion IN ('AFR', 'EMR','SEA', 'WPR'))"

# region_filter <- ""

report_year <- 2021

file_name     <- paste0(outfolder, "prov_notifs_graphs_", Sys.Date(), ".pdf")



# load packages ----
library(RODBC)
library(ggplot2)
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

sql <- paste("SELECT country, year, c_newinc FROM dcf.latest_notification
              UNION ALL
              SELECT country, year, c_newinc FROM view_TME_master_notification
              WHERE year BETWEEN",
              report_year-5,
              "AND",
              report_year-1,
              "ORDER BY country, year")


# Extract data from the database
channel <- odbcDriverConnect(connection_string)
data_to_plot <- sqlQuery(channel,sql)

# get provisional notifications for countries that have reported for the whole year
prov_notifs <- sqlQuery(channel,
                        paste("SELECT country, year,
                              CASE report_frequency
                                WHEN 70 THEN
                              m_01 + m_02 + m_03 + m_04 + m_05 + m_06 +
                              m_07 + m_08 + m_09 + m_10 + m_11 + m_12
                                ELSE q_1 + q_2 + q_3 + q_4
                              END AS c_newinc_prov
                              FROM dcf.latest_provisional_c_newinc",
                              "WHERE COALESCE(m_12, q_4) IS NOT NULL",
                              region_filter,
                              "AND year = ",
                              report_year-1,
                              "ORDER BY country"))

close(channel)


# Merge the two datasets
data_to_plot <- data_to_plot %>%
  left_join(prov_notifs, by = c("country", "year"))

# List of countries to plot
countries <- select(prov_notifs, country)


# Simple rounding function that returns a string rounded to the nearest integer and
# uses a space as the thousands separator as per WHO standard.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rounder <- function(x) {

    ifelse(is.na(x), NA,
           formatC(round(x,0), big.mark=" ", format="d")
           )
}

# Define graph layout ----
# - - - - - - - - - - -

plot_faceted <- function(df){

  # Blue line  = New and relapse cases
  # Green dot  = provisional notifications

  graphs <- qplot(year, c_newinc, data=df, geom="line", colour=I("blue")) +

            geom_point(aes(year, c_newinc_prov), colour=I("green"), size=2 ) +

            # Use space separators for the y axis
            scale_y_continuous(name = "New and relapse annual cases (blue) and provisional monthly/quarterly (green) (number)",
                               labels = rounder) +

            scale_x_continuous(name="", breaks = c(2016, 2018, 2020)) +

            facet_wrap(~country,
                       scales="free_y",
                       # Use the labeller function to make sure long country names are wrapped in panel headers
                       labeller = label_wrap_gen(width = 22)) +

            expand_limits(y=0) +

            theme_gtbr_2021(base_size=8, axis_text_size = 6)


  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)

}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")


plot_blocks_to_pdf(data_to_plot, countries, file_name)


