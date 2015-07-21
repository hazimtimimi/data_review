# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Compare estimates of incidence, prevalence and mortality from three separate
# Global TB reports
# Hazim Timimi, December 2014, based on original code from Babis Sismanidis
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
# The next two are set using get_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

scriptsfolder <- getSrcDirectory(function(x) {x})  # See http://stackoverflow.com/a/30306616

file_name     <- paste0("estimates_graphs_", Sys.Date(), ".pdf")

setwd(scriptsfolder)

source("get_environment.r")  #particular to each person so this file is in the ignore list


# load packages ----
library(RODBC)
library(ggplot2)
library(plyr)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
#
# I prefer to do this via SQL, but could be done of course with the pure views
# and some R jiggery pokey
#
# Extract historical estimates for a given report/date
# and associated variable names with the report year
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

get_historical_estimates <- function(channel,version,limiting_date){

  sql <- paste("SELECT country, iso3, year,
                  e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
              		e_prev_100k, e_prev_100k_lo, e_prev_100k_hi,
              		e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi
                  FROM	epi_estimates_at_date(CAST('", limiting_date, "' AS DATE))", sep="")

  # Extract data from the database
  historical <- sqlQuery(channel,sql)


  # rename variables to include version using plyr
  historical <- rename(historical, c("e_inc_100k" = paste("inc",version, sep=""),
                                     "e_inc_100k_lo" = paste("inclo",version, sep=""),
                                     "e_inc_100k_hi" = paste("inchi",version, sep=""),
                                     "e_prev_100k" = paste("prev",version, sep=""),
                                     "e_prev_100k_lo" = paste("prevlo",version, sep=""),
                                     "e_prev_100k_hi" = paste("prevhi",version, sep=""),
                                     "e_mort_exc_tbhiv_100k" = paste("mort",version, sep=""),
                                     "e_mort_exc_tbhiv_100k_lo" = paste("mortlo",version, sep=""),
                                     "e_mort_exc_tbhiv_100k_hi" = paste("morthi",version, sep="")))


  return(historical)
}


# Extract data from the database
ch <- odbcDriverConnect(connection_string)


est2012 <- get_historical_estimates(ch, "series1","2012-10-17")    #2012 report
est2013 <- get_historical_estimates(ch, "series2","2013-12-31")    #2013 report
est2014 <- get_historical_estimates(ch, "series3","2014-11-30")    #2014 report

# get list of countries
countries <- sqlQuery(ch, "SELECT country FROM view_TME_master_report_country ORDER BY country")

close(ch)

# combine the datasets into one

changes <- merge(est2014, est2013, all.x=TRUE)
changes <- merge(changes, est2012, all.x=TRUE)

rm(list=c("est2012", "est2013", "est2014"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Define graph layout ----
#
# Plot 3 sets of graphs (incidence, prevalence, mortality)
# Each graph shows three series for the same indicator, each from a separate global report.
# Series will overlap, hence make them transparent -- I've used alpha=0.2
#
# Assumes the input dataframe is wide, with the three data series labelled series1, series2 and series3
# and indicators called inc, prev and mort, with uncertainty intervals called lo and hi
# therefore incloseries2= low bound incidence from series2
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

plot_incprevmort <- function(df){


  #plot incidence (faceted)
  #note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)
  print(qplot(year, incseries1, data=df, geom='line', colour=I('blue')) +
    geom_ribbon(aes(year, ymin=incloseries1, ymax=inchiseries1), fill=I('blue'), alpha=0.2) +
    geom_line(aes(year, incseries2), colour=I('red')) +
    geom_ribbon(aes(year, ymin=incloseries2, ymax=inchiseries2),
                fill=I('red'), alpha=0.2) +
    geom_line(aes(year, incseries3), colour=I('green')) +
    geom_ribbon(aes(year, ymin=incloseries3, ymax=inchiseries3),
                fill=I('green'), alpha=0.2) +
    facet_wrap(~country, scales='free_y') +
    xlab('') + ylab('Incidence rate per 100,000 population/year') +
    expand_limits(y=0) +
    theme_bw(base_size=8) +
    theme(legend.position="bottom"))

  #plot prevalence (faceted)
  print(qplot(year, prevseries1, data=df, geom='line', colour=I('blue')) +
    geom_ribbon(aes(year, ymin=prevloseries1, ymax=prevhiseries1), fill=I('blue'), alpha=0.2) +
    geom_line(aes(year, prevseries2), colour=I('red')) +
    geom_ribbon(aes(year, ymin=prevloseries2, ymax=prevhiseries2),
                fill=I('red'), alpha=0.2) +
    geom_line(aes(year, prevseries3), colour=I('green')) +
    geom_ribbon(aes(year, ymin=prevloseries3, ymax=prevhiseries3),
                fill=I('green'), alpha=0.2) +
    facet_wrap(~country, scales='free_y') +
    xlab('') + ylab('Prevalence rate per 100,000 population') +
    expand_limits(y=0) +
    theme_bw(base_size=8) +
    theme(legend.position='none'))

  #plot mortality (faceted)
  print(qplot(year, mortseries1, data=df, geom='line', colour=I('blue')) +
    geom_ribbon(aes(year, ymin=mortloseries1, ymax=morthiseries1), fill=I('blue'), alpha=0.2) +
    geom_line(aes(year, mortseries2), colour=I('red')) +
    geom_ribbon(aes(year, ymin=mortloseries2, ymax=morthiseries2),
                fill=I('red'), alpha=0.2) +
    geom_line(aes(year, mortseries3), colour=I('green')) +
    geom_ribbon(aes(year, ymin=mortloseries3, ymax=morthiseries3),
                fill=I('green'), alpha=0.2) +
    facet_wrap(~country, scales='free_y') +
    xlab('') + ylab('Mortality rate per 100,000 population/year') +
    expand_limits(y=0) +
    theme_bw(base_size=8) +
    theme(legend.position='none'))

}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")

setwd(outfolder)

plot_blocks_to_pdf(changes, countries, file_name, plot_function = plot_incprevmort)

# clear the decks
rm(list=ls())





