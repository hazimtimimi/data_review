# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at trends in provision of palliative care to patients whose MDR
# treatment failed
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


source("set_environment.r")  #particular to each person so this file is in the ignore list

file_name     <- paste0(outfolder, "palliative_graphs_", Sys.Date(), ".pdf")


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

sql <- "WITH palliative AS (
SELECT iso2, year-2 AS year, mdrxdr_fail_morphine
FROM	view_TME_master_strategy
UNION ALL
SELECT iso2, year-2 AS year, mdrxdr_fail_morphine
FROM	dcf.latest_strategy
),
mxdr_fail AS (
SELECT  iso2, country, year, ISNULL(mdr_fail,0) + ISNULL(xdr_fail,0) AS dr_fail
FROM	view_TME_master_outcomes
WHERE	year BETWEEN 2015 AND (SELECT MAX(year) - 1 FROM dcf.latest_mdr_xdr_outcomes)
UNION ALL
SELECT  iso2, country, year, ISNULL(mdr_fail,0) + ISNULL(xdr_fail,0) AS dr_fail
FROM	dcf.latest_mdr_xdr_outcomes
)

SELECT	mxdr_fail.country, mxdr_fail.year, dr_fail,mdrxdr_fail_morphine
FROM	mxdr_fail
			LEFT OUTER JOIN palliative ON
				mxdr_fail.iso2 = palliative.iso2 AND
				mxdr_fail.year = palliative.year
WHERE	mxdr_fail.iso2 IN (SELECT iso2 FROM dcf.latest_mdr_xdr_outcomes WHERE ISNULL(mdr_fail,0) + ISNULL(xdr_fail,0) > 0)
ORDER BY country, year;"


# Extract data from the database
channel <- odbcDriverConnect(connection_string)

data_to_plot <- sqlQuery(channel,sql, stringsAsFactors = FALSE)

# get list of countries
countries <- sqlQuery(channel,
                      "SELECT country FROM dcf.latest_mdr_xdr_outcomes WHERE ISNULL(mdr_fail,0) + ISNULL(xdr_fail,0) > 0 ORDER BY country",
                      stringsAsFactors = FALSE)

close(channel)




# Define graph layout ----
# - - - - - - - - - - -

plot_faceted <- function(df){

  # Blue dots  = Patients failed second-line treatment
  # Green dots = Number of patients who failed second line treatment who received oral morphine


graphs <- qplot(year, dr_fail, data=df, geom="point", colour=I("blue")) +
          geom_point(aes(year, mdrxdr_fail_morphine), colour=I("green")) +

          # Use space separators for the y axis
          scale_y_continuous(name = "Patients failed second-line treatment (blue) and who received oral morphine (green) (number)") +

          scale_x_continuous(name="", breaks = c(2015, 2016, 2017)) +

          facet_wrap(~country, scales="free_y") +


          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="bottom")

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




