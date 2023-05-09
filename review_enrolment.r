# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Compare diagnosis and enrolment on treatment numbers with total notifications
# Hazim Timimi, May 2023
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
# region_filter <- "AND iso2 IN (SELECT iso2 FROM view_TME_master_report_country
#                               WHERE g_whoregion IN ('AFR', 'EMR','SEA', 'WPR'))"

region_filter <- ""


file_name     <- paste0(outfolder, "enrolment_graphs_", Sys.Date(), ".pdf")



# load packages ----
library(RODBC)
library(ggplot2)
library(dplyr)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
#
# Find countries with at least 10 notified cases and where the difference
# between notified cases and the total diagnosed (DS-TB and RR-TB) is at
# least 5% of notifications
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sql <- "SELECT country, c_notified, nrr, nrr_tx, conf_rr_nfqr, conf_rr_nfqr_tx, unconf_rr_nfqr_tx,
		    ABS(ISNULL(nrr, 0) + ISNULL(conf_rr_nfqr,0) - c_notified) * 100.00 / c_notified AS diff_pct
        FROM dcf.latest_notification
        WHERE ISNULL(c_notified, 0) > 10 AND
              ABS(ISNULL(nrr, 0) + ISNULL(conf_rr_nfqr,0) - c_notified) * 100.00 / c_notified > 5"

# Extract data from the database
channel <- odbcDriverConnect(connection_string)
notifs_enrolments <- sqlQuery(channel,sql)

close(channel)

# Store list of countries
countries <- notifs_enrolments %>% select(country) %>% arrange(toupper(country))

# Flip the data to plot to long format
data_to_plot <- notifs_enrolments %>%
  tidyr::pivot_longer(c_notified:unconf_rr_nfqr_tx)


# Simple rounding function that returns a string rounded to the nearest integer and
# uses a space as the thousands separator as per WHO standard.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rounder <- function(x) {

  ifelse(is.na(x), NA,
         ifelse(x %% 1 != 0, "",
                formatC(round(x,0), big.mark=" ", format="d")
         ))
}



# Define graph layout ----
# - - - - - - - - - - -

plot_faceted <- function(df){


  graphs <- df %>%
    ggplot() +

    geom_col(aes(x=name,y = value, fill=name))  +

    # Use space separators for the y axis
    scale_y_continuous(name = "Number of people",
                       labels = rounder) +

    scale_x_discrete(name="",
                     limits=c("Tot" = "c_notified",
                              "ds" = "nrr",
                              "dstx" = "nrr_tx",
                              "dr" = "conf_rr_nfqr",
                              "drtx" = "conf_rr_nfqr_tx",
                              "u" = "unconf_rr_nfqr_tx"),
                     labels=c("Tot",
                              "ds",
                              "dstx",
                              "dr",
                              "drtx",
                              "u")) +

    scale_fill_manual(values = c("c_notified" = "darkgreen",
                                 "nrr" = "darkblue",
                                 "nrr_tx" = "blue",
                                 "conf_rr_nfqr" = "darkred",
                                 "conf_rr_nfqr_tx" = "red",
                                 "unconf_rr_nfqr_tx" = "red")) +

    facet_wrap(~country, scales="free_y",
               # Use the labeller function to make sure long country names are wrapped in panel headers
               labeller = label_wrap_gen(width = 25)) +

    expand_limits(y=0) +
    theme_gtbr_2021(base_size=6) +
    theme(legend.position = "none")


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


