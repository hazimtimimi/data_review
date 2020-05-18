# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at trends in number of notifications among workers at health care facilities
# Hazim Timimi, June 2017
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

file_name     <- paste0(outfolder, "hcw_graphs_", Sys.Date(), ".pdf")
file_name_rate     <- paste0(outfolder, "hcw_rate_graphs_", Sys.Date(), ".pdf")


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

sql <- "SELECT country, iso2, year, hcw_tb_infected, hcw_tot FROM dcf.latest_strategy
                WHERE COALESCE(hcw_tb_infected, hcw_tot) IS NOT NULL
                UNION ALL
                SELECT country, iso2, year, hcw_tb_infected, hcw_tot FROM view_TME_master_strategy
                WHERE year BETWEEN 2010 AND (SELECT MAX(year - 1) FROM dcf.latest_strategy) AND
  					    iso2 IN (SELECT iso2 FROM dcf.latest_strategy WHERE COALESCE(hcw_tb_infected, hcw_tot) IS NOT NULL)
				        ORDER BY country,year"

sql_notif_rate <- "WITH notifs AS (
SELECT	iso2, year, c_newinc
FROM	dcf.latest_notification
UNION ALL
SELECT	iso2, year, c_newinc
FROM	view_TME_master_notification
WHERE	year BETWEEN 2010 AND (SELECT MAX(year - 1) FROM dcf.latest_strategy)
)
SELECT notifs.iso2, notifs.year,
		CASE WHEN (ISNULL(e_pop_num, 0) > 0) AND (c_newinc IS NOT NULL  )
				THEN 100000.00 * c_newinc / e_pop_num
				ELSE NULL
		END AS c_newinc_100k
FROM	notifs
			INNER JOIN view_TME_estimates_population ON
				notifs.iso2 = view_TME_estimates_population.iso2 AND
				notifs.year = view_TME_estimates_population.year
WHERE notifs.iso2 IN (SELECT iso2 FROM dcf.latest_strategy WHERE COALESCE(hcw_tb_infected, hcw_tot) IS NOT NULL)"

# Extract data from the database
channel <- odbcDriverConnect(connection_string)

data_to_plot <- sqlQuery(channel,sql, stringsAsFactors = FALSE)

notif_rates <- sqlQuery(channel,sql_notif_rate, stringsAsFactors = FALSE)
# get list of countries
countries <- sqlQuery(channel,
                      "SELECT country FROM dcf.latest_strategy WHERE COALESCE(hcw_tb_infected, hcw_tot) IS NOT NULL ORDER BY country",
                      stringsAsFactors = FALSE)

close(channel)


# Calculate percent of cases among foreigners where possible
data_to_plot <- data_to_plot %>%
  mutate(hcw_rate = ifelse(hcw_tot > 0, hcw_tb_infected * 1e5 / hcw_tot, NA)) %>%
  # add the generate TB notification rate for comparison
  inner_join(notif_rates, by = c("iso2", "year"))



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

  # Blue dots  = workers at health care facilities
  # Green dots = Cases among workers at health care facilities


graphs <- qplot(year, hcw_tot, data=df, geom="point", colour=I("blue")) +
          geom_point(aes(year, hcw_tb_infected), colour=I("green")) +

          # Use space separators for the y axis
          scale_y_continuous(name = "Workers at healthcare facilities (blue) and TB cases among  healthcare workers (green) (number per year)",
                             labels = rounder) +

          scale_x_continuous(name="", breaks = c(2010, 2014, 2018)) +

          facet_wrap(~country, scales="free_y") +


          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="bottom")

  # note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(graphs)

}



plot_faceted_rate <- function(df){

  # Blue line  = % cases among foreigners

graphs <- qplot(year, hcw_rate, data=df, geom="line", colour=I("blue")) +
          geom_point(aes(year, hcw_rate), colour=I("blue")) +

          geom_line(aes(year, c_newinc_100k), colour = I("green")) +

          # Use space separators for the y axis
          scale_y_continuous(name = "TB notification rate per 100 000 health care workers (blue) and population (green)") +

          scale_x_continuous(name="", breaks = c(2010, 2014, 2018)) +

          facet_wrap(~country, scales="free_y") +

          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="bottom")
  print(graphs)

}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")


plot_blocks_to_pdf(data_to_plot, countries, file_name, plot_function = plot_faceted)

plot_blocks_to_pdf(data_to_plot, countries, file_name_rate, plot_function = plot_faceted_rate)





