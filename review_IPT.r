# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Look at IPT trends using data from previous years and
# the latest data reported to UNAIDS (via GARPR)
# Hazim Timimi, May 2015
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
# (If not keep it as an empty string)
# region_filter <- "AND g_whoregion IN ('AFR', 'EMR','SEA', 'WPR')"

region_filter <- ""

source("set_environment.r")  #particular to each person so this file is in the ignore list
source("set_plot_themes.r")


file_name     <- paste0(outfolder, "hiv_tpt_graphs_", Sys.Date(), ".pdf")


# load packages ----
library(RODBC)
library(ggplot2)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
#
# I prefer to do this via SQL, but could be done of course with the pure views
# and some R jiggery pokery
#
# The query combines data from the master notification view with latest data
# reported as retreived from the dcf views (dcf = data collection form)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sql <- "SELECT country, year, hiv_new_tpt AS hiv_ipt, hiv_new AS hiv_reg_new FROM dcf.latest_notification
                WHERE hiv_new_tpt is not null
                UNION ALL
                SELECT country, year, hiv_ipt, hiv_reg_new FROM view_TME_master_notification
                WHERE year BETWEEN 2010 AND (SELECT max(year - 1) from dcf.latest_notification) AND
  					    iso2 IN (SELECT iso2 from dcf.latest_notification WHERE ISNULL(hiv_ipt,0) > 0)
				        ORDER BY country,year"


# Extract data from the database
channel <- odbcDriverConnect(connection_string)
data_to_plot <- sqlQuery(channel,sql)

# get list of countries
countries <- sqlQuery(channel, paste("SELECT country FROM dcf.latest_TBHIV_for_aggregates",
                                     "WHERE ISNULL(hiv_new_tpt,0) > 0",
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


graphs <- qplot(year, hiv_reg_new, data=df, geom="line", colour=I("green")) +
            geom_line(aes(year, hiv_ipt), colour=I("blue")) +

          # Use space separators for the y axis
          scale_y_continuous(name = "PLHIV newly enrolled in care (green), given TPT (blue) (number)",
                             labels = rounder) +

          scale_x_continuous(name="", breaks = c(2010, 2014, 2018)) +

          facet_wrap(~country, scales="free_y",
                     # Use the labeller function to make sure long country names are wrapped in panel headers
                     labeller = label_wrap_gen(width = 25)) +

          expand_limits(y=0) +
            theme_gtbr_2021(base_size=6) +
            # Add a gray line over the x-axis so that all graphs have a line at the bottom
            annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, colour = "#BCBCBC")
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

plot_blocks_to_pdf(data_to_plot, countries, file_name, block_size = 12)


