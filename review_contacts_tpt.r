# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at trends in number of contacts provided with TB preventive therapy
# Hazim Timimi, May 2020
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


# Define list of regions in SQL format if we don't want to plot all countries
# region_filter <- "WHERE g_whoregion IN ('AFR', 'EMR','SEA', 'WPR')"

region_filter <- ""

source("set_environment.r")  #particular to each person so this file is in the ignore list
source("set_plot_themes.r")

file_name     <- paste0(outfolder, "contacts_tpt_graphs_", Sys.Date(), ".pdf")


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

sql <- "WITH completion AS
(
SELECT	iso2, year - 1 AS year, newinc_con_prevtx_cmplt
FROM	dcf.latest_contacts_tpt
UNION ALL
SELECT iso2, year, newinc_con_prevtx_cmplt
FROM view_TME_master_contacts_tpt
WHERE year BETWEEN 2019 AND (SELECT MAX(year - 2) FROM dcf.latest_contacts_tpt)
)

SELECT	country, year, newinc_con_prevtx, newinc_con04_prevtx, NULL AS newinc_con_prevtx_cmplt
FROM	dcf.latest_contacts_tpt
UNION ALL
SELECT country, view_TME_master_contacts_tpt.year, newinc_con_prevtx, newinc_con04_prevtx, completion.newinc_con_prevtx_cmplt
FROM view_TME_master_contacts_tpt
	LEFT OUTER JOIN completion ON
		view_TME_master_contacts_tpt.iso2 = completion.iso2 AND
		view_TME_master_contacts_tpt.year = completion.year
WHERE view_TME_master_contacts_tpt.year BETWEEN 2015 AND (SELECT MAX(year - 1) FROM dcf.latest_contacts_tpt)
ORDER BY country,year;"

# Extract data from the database
channel <- odbcDriverConnect(connection_string)
data_to_plot <- sqlQuery(channel,sql)

# get list of countries
countries <- sqlQuery(channel, paste("SELECT country FROM dcf.latest_contacts_tpt",
                                     "WHERE COALESCE(newinc_con_prevtx, newinc_con04_prevtx) IS NOT NULL",
                                     region_filter,
                                     "ORDER BY country"))

close(channel)


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

  # Blue line  = Contacts started on TPT
  # Green dots = Contacts aged 0-4 started on TPT


graphs <- qplot(year, newinc_con_prevtx, data=df, geom="point", colour=I("blue")) +
          geom_line(aes(year, newinc_con_prevtx), colour=I("blue"), size = 1.25) +

          geom_line(aes(year, newinc_con04_prevtx), colour=I("green")) +
          geom_point(aes(year, newinc_con04_prevtx), colour=I("green"), size = 1) +


  geom_line(aes(year, newinc_con_prevtx_cmplt), colour=I("red")) +
  geom_point(aes(year, newinc_con_prevtx_cmplt), colour=I("red"), size = 1) +

          # Use space separators for the y axis
          scale_y_continuous(name = "Contacts started on TPT (blue) [aged 0-4 (green)] and completing TPT (red)  (number)",
                             labels = rounder) +

          scale_x_continuous(name="", breaks = c(2015, 2018, 2021)) +

          facet_wrap(~country, scales="free_y",
                     # Use the labeller function to make sure long country names are wrapped in panel headers
                     labeller = label_wrap_gen(width = 25)) +

          expand_limits(y=0) +
          theme_gtbr_2021(base_size=6) +
          # Add a gray line over the x-axis so that all graphs have a line at the bottom
          annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, colour = "#BCBCBC")
          theme(legend.position="none")

  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")


plot_blocks_to_pdf(data_to_plot, countries, file_name, plot_function = plot_faceted)






