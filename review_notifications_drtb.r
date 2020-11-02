# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at trends for drug-resistant TB using data from previous years and
# the latest data reported to us
# Hazim Timimi, November 2020
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

file_name     <- paste0(outfolder, "notifs_drtb_graphs_", Sys.Date(), ".pdf")



# load packages ----
library(RODBC)
library(ggplot2)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
#
# I prefer to do this via SQL, but could be done of course with the pure views
# and some R jiggery pokey
#
# The query combines data from the master notification view with latest data
# reported as retreived from the dcf views (dcf = data collection form)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sql <- "SELECT country, year, conf_rrmdr, conf_mdr, conf_rrmdr_tx FROM dcf.latest_notification
                UNION ALL
                SELECT country, year, conf_rrmdr, conf_mdr,

                /* next statement is possible because variables are mutually exclusive -- switch happened in 2015 dcyear */
                CASE WHEN COALESCE(conf_mdr_tx, conf_rrmdr_tx) is null THEN NULL
                     ELSE ISNULL(conf_mdr_tx,0) + ISNULL(conf_rrmdr_tx,0)
                END AS conf_rrmdr_tx
                FROM view_TME_master_notification
                WHERE year BETWEEN 2000 AND (SELECT max(year - 1) from dcf.latest_notification)
				        ORDER BY country,year"

# Extract data from the database
channel <- odbcDriverConnect(connection_string)
data_to_plot <- sqlQuery(channel,sql)

# get list of countries
countries <- sqlQuery(channel, "SELECT country FROM dcf.latest_notification WHERE COALESCE(conf_rrmdr, conf_mdr, conf_rrmdr_tx) IS NOT NULL ORDER BY country")

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

  # Blue line  = RR-TB cases detected
  # Green line = MDR-TB cases detected
  # Grey line = RR-TB cases started on treatment

  graphs <- qplot(year, conf_rrmdr, data=df, geom="line", colour=I("blue")) +
            geom_line(aes(year, conf_mdr), colour=I("green"), alpha=0.5) +
            geom_line(aes(year, conf_rrmdr_tx), colour=I("grey"), alpha=0.5, size = 1.2) +

            # Use space separators for the y axis
            scale_y_continuous(name = "All RR-TB cases (blue), MDR-TB cases (green), started on treatment (grey) (number)",
                               labels = rounder) +

            scale_x_continuous(name="", breaks = c(2000, 2005, 2010, 2015, 2019)) +

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


plot_blocks_to_pdf(data_to_plot, countries, file_name)


