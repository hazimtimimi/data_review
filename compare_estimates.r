# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Compare estimates of incidence and mortality from three separate
# Global TB reports
# Hazim Timimi, December 2014, based on original code from Babis Sismanidis
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Set up the running environment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This depends on the person, location, machine used etc.and populates the following:
#
# file_name_*:        Names of the PDF output files
#
# The next variables are set using set_environment.r
#
# outfolder:          Folder containing output subfolders for tables and figures
# connection_string:  ODBC connection string to the global TB database
# series_1_date:      Date at which estimates for series 1 were valid
# series_2_date:      Date at which estimates for series 2 were valid
# series_3_date:      Date at which estimates for series 3 were valid
#
# g_whoregion:        WHO regional code to filter charts
#                     (if not specified then charts are produced for all countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


source("set_environment.r")  #particular to each person so this file is in the ignore list


file_name_inc      <- paste0(g_whoregion, "incidence_graphs_", Sys.Date(), ".pdf")
file_name_inc_h  <- paste0(g_whoregion, "incidence_hiv_graphs_", Sys.Date(), ".pdf")
file_name_mort_nh     <- paste0(g_whoregion, "mortality_excl_hiv_graphs_", Sys.Date(), ".pdf")
file_name_mort_h <- paste0(g_whoregion, "mortality_hiv_graphs_", Sys.Date(), ".pdf")
file_name_rr_new <- paste0(g_whoregion, "rr_in_new_graphs_", Sys.Date(), ".pdf")
file_name_rr_ret <- paste0(g_whoregion, "rr_in_ret_graphs_", Sys.Date(), ".pdf")
file_name_inc_rr <- paste0(g_whoregion, "inc_rr_graphs_", Sys.Date(), ".pdf")


# load packages ----
library(RODBC)
library(ggplot2)
library(dplyr)
library(stringr)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
#
# I prefer to do this via SQL, but could be done of course with the pure views
# and some R jiggery pokey
#
# Extract historical estimates for a given report/date
# and rename them using the pattern variablename_version
#
# therefore inc_lo_series1 is the low bound incidence for version called series1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

get_historical_estimates <- function(channel,version,limiting_date){

  # Make sure a valid limiting date was supplied. If not set it to 1900-01-01
  limiting_date <- tryCatch(as.Date(limiting_date), error = function(e) return('1900-01-01'))

  # Set the limiting date for RR estimates, remembering that there are
  # no timeseries before 2022-09-30

  rr_limiting_date <- ifelse(limiting_date >= as.Date('2022-09-30'),
                             as.character(limiting_date),
                             '1900-01-01')



  sql <- "
        SELECT country, inc_mort.iso3, inc_mort.year,
               e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
               e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi,
               e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
               e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo, e_mort_tbhiv_100k_hi,
			         e_rr_prop_new, e_rr_prop_new_lo, e_rr_prop_new_hi,
               e_rr_prop_ret, e_rr_prop_ret_lo, e_rr_prop_ret_hi,
               e_inc_rr_num, e_inc_rr_num_lo, e_inc_rr_num_hi

        FROM  epi_estimates_rawvalues_at_date(CAST('@limiting_date' AS DATE)) AS inc_mort

               LEFT OUTER JOIN
              (SELECT iso3, year,
               e_rr_prop_new, e_rr_prop_new_lo, e_rr_prop_new_hi,
               e_rr_prop_ret, e_rr_prop_ret_lo, e_rr_prop_ret_hi,
               e_inc_rr_num, e_inc_rr_num_lo, e_inc_rr_num_hi
               FROM	dr_estimates_rawvalues_at_date(CAST('@rr_limiting_date' AS DATE))) AS dr
                  ON inc_mort.iso3 = dr.iso3 AND
                     inc_mort.year = dr.year;"

  sql <- gsub('@limiting_date', as.character(limiting_date), sql, ignore.case = TRUE)
  sql <- gsub('@rr_limiting_date', rr_limiting_date, sql, ignore.case = TRUE)

  # Extract data from the database
  estimates <- sqlQuery(channel,sql)


  # rename variables -- shorten and add version using dplyr
  estimates <- estimates |>

    rename_with( ~ paste0(str_replace(.x, "e_inc_100k" , "inc"),"_" , version, recycle0 = TRUE),
                 .cols = starts_with("e_inc_100k")) |>

    rename_with( ~ paste0(str_replace(.x, "e_inc_tbhiv_100k" , "inc_h"),"_" , version, recycle0 = TRUE),
                 .cols = starts_with("e_inc_tbhiv_100k")) |>

    rename_with( ~ paste0(str_replace(.x, "e_mort_exc_tbhiv_100k" , "mort_nh"),"_" , version, recycle0 = TRUE),
                 .cols = starts_with("e_mort_exc_tbhiv_100k")) |>

    rename_with( ~ paste0(str_replace(.x, "e_mort_tbhiv_100k" , "mort_h"),"_" , version, recycle0 = TRUE),
                 .cols = starts_with("e_mort_tbhiv_100k")) |>

    rename_with( ~ paste0(str_replace(.x, "e_rr_prop_new" , "rr_new"),"_" , version, recycle0 = TRUE),
                 .cols = starts_with("e_rr_prop_new")) |>

    rename_with( ~ paste0(str_replace(.x, "e_rr_prop_ret" , "rr_ret"),"_" , version, recycle0 = TRUE),
                 .cols = starts_with("e_rr_prop_ret")) |>

    rename_with( ~ paste0(str_replace(.x, "e_inc_rr_num" , "inc_rr"),"_" , version, recycle0 = TRUE),
                 .cols = starts_with("e_inc_rr_num"))

  return(estimates)
}


# Extract data from the database
ch <- odbcDriverConnect(connection_string)

estimates_series_1 <- get_historical_estimates(ch, "series1", series_1_date)
estimates_series_2 <- get_historical_estimates(ch, "series2", series_2_date)
estimates_series_3 <- get_historical_estimates(ch, "series3", series_3_date)

# get list of countries
country_sql <- ifelse(g_whoregion == "",
                    "SELECT country FROM view_TME_master_report_country ORDER BY country",
                    paste0("SELECT country FROM view_TME_master_report_country WHERE g_whoregion = '", g_whoregion, "' ORDER BY country"))

countries <- sqlQuery(ch, country_sql)

# get list of HBCs
hbc_sql <- ifelse(g_whoregion == "",
                      "SELECT country FROM view_country_group_membership WHERE group_type='g_hb_tb' AND group_name = '1' ORDER BY country",
                      paste0("SELECT country FROM view_country_group_membership WHERE group_type='g_hb_tb' AND group_name = '1' AND g_whoregion = '", g_whoregion, "' ORDER BY country"))

hbc  <- sqlQuery(ch, hbc_sql)

# get list of USAID countries
usaid_countries <- sqlQuery(ch, "
                           SELECT country FROM view_TME_master_report_country
                          WHERE iso2 in ('AF','BD','KH','CD','ET','IN','ID','KE','KG','MW','MZ','MM',
                                          'NG','PK','PH','ZA','TJ','UG','UA','TZ','UZ','VN','ZM','ZW')
                          ORDER BY country")

close(ch)

# combine the three series into a single wider one called estimates_changes

estimates_changes <- merge(estimates_series_2, estimates_series_3, all=TRUE)
estimates_changes <- merge(estimates_changes, estimates_series_1, all=TRUE)

rm(list=c("estimates_series_1", "estimates_series_2", "estimates_series_3"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Define graph layout ----
#
# Plot graphs (incidence, TB/HIV incidence, mortality excl HIV, TB/HIV mortality,
# rr in new, rr in ret, RR incidence)
#
# Each graph shows three series for the same indicator, each from a separate global report.
# Series will overlap, hence make them transparent -- I've used alpha=0.2
#
# Assumes the input dataframe is wide, with the three data series labelled series1, series2 and series3
# and indicators called inc, inc_h, mort_nh,mort_h, rr_new, rr_ret, inc_rr with uncertainty intervals
# called lo and hi
#
# variable format is variable_series
#
# therefore inc_lo_series2 is the low bound incidence from series2
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

plot_inc <- function(df){

  #plot incidence (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3

  #note that inside a function the print() command is needed to paint to the canvass
  #(see http://stackoverflow.com/questions/19288101/r-pdf-usage-inside-a-function)


  print(qplot(year, inc_series1, data=df, geom="line", colour=I("blue")) +
        geom_ribbon(aes(year,
                        ymin=inc_lo_series1,
                        ymax=inc_hi_series1),
                    fill=I("blue"), alpha=0.2) +

        geom_line(aes(year, inc_series2), colour=I("red")) +
        geom_ribbon(aes(year,
                        ymin=inc_lo_series2,
                        ymax=inc_hi_series2),
                    fill=I("red"), alpha=0.2) +

        geom_line(aes(year, inc_series3), colour=I("green")) +
        geom_ribbon(aes(year,
                        ymin=inc_lo_series3,
                        ymax=inc_hi_series3),
                    fill=I("green"), alpha=0.2) +
        facet_wrap(~country, scales="free_y") +
        xlab("") +
        ylab(paste0("Incidence (rate/100k pop/yr)",
                    "; blue=", series_1_desc,
                    "; red=", series_2_desc,
                    "; green=", series_3_desc)) +
        expand_limits(y=0) +
        theme_bw(base_size=8) +
        theme(legend.position="bottom"))
}

plot_inc_h <- function(df){

  # plot TB/HIV incidence (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3

  print(qplot(year, inc_h_series1, data=df, geom="line", colour=I("blue")) +
        geom_ribbon(aes(year,
                        ymin=inc_h_lo_series1,
                        ymax=inc_h_hi_series1),
                    fill=I("blue"), alpha=0.2) +

        geom_line(aes(year, inc_h_series2), colour=I("red")) +
        geom_ribbon(aes(year,
                        ymin=inc_h_lo_series2,
                        ymax=inc_h_hi_series2),
                    fill=I("red"), alpha=0.2) +

        geom_line(aes(year, inc_h_series3), colour=I("green")) +
        geom_ribbon(aes(year,
                        ymin=inc_h_lo_series3,
                        ymax=inc_h_hi_series3),
                    fill=I("green"), alpha=0.2) +
        facet_wrap(~country, scales="free_y") +
        xlab("") +
        ylab(paste0("TB/HIV incidence (rate/100k pop/yr)",
                    "; blue=", series_1_desc,
                    "; red=", series_2_desc,
                    "; green=", series_3_desc)) +
        expand_limits(y=0) +
        theme_bw(base_size=8) +
        theme(legend.position="bottom"))
}

plot_mort_nh <- function(df){

  # plot HIV-negative mortality (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3

  print(qplot(year, mort_nh_series1, data=df, geom="line", colour=I("blue")) +
        geom_ribbon(aes(year,
                        ymin=mort_nh_lo_series1,
                        ymax=mort_nh_hi_series1),
                    fill=I("blue"), alpha=0.2) +

        geom_line(aes(year, mort_nh_series2), colour=I("red")) +
        geom_ribbon(aes(year,
                        ymin=mort_nh_lo_series2,
                        ymax=mort_nh_hi_series2),
                    fill=I("red"), alpha=0.2) +

        geom_line(aes(year, mort_nh_series3), colour=I("green")) +
        geom_ribbon(aes(year,
                        ymin=mort_nh_lo_series3,
                        ymax=mort_nh_hi_series3),
                    fill=I("green"), alpha=0.2) +
        facet_wrap(~country, scales="free_y") +
        xlab("") +
        ylab(paste0("Mortality, excluding TB/HIV (rate/100k pop/yr)",
                    "; blue=", series_1_desc,
                    "; red=", series_2_desc,
                    "; green=", series_3_desc)) +
        expand_limits(y=0) +
        theme_bw(base_size=8) +
        theme(legend.position="none"))
}

plot_mort_h <- function(df){

  # plot HIV-positive TB mortality (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3

  print(qplot(year, mort_h_series1, data=df, geom="line", colour=I("blue")) +
        geom_ribbon(aes(year,
                        ymin=mort_h_lo_series1,
                        ymax=mort_h_hi_series1),
                    fill=I("blue"), alpha=0.2) +

        geom_line(aes(year, mort_h_series2), colour=I("red")) +
        geom_ribbon(aes(year,
                        ymin=mort_h_lo_series2,
                        ymax=mort_h_hi_series2),
                    fill=I("red"), alpha=0.2) +

        geom_line(aes(year, mort_h_series3), colour=I("green")) +
        geom_ribbon(aes(year,
                        ymin=mort_h_lo_series3,
                        ymax=mort_h_hi_series3),
                    fill=I("green"), alpha=0.2) +
        facet_wrap(~country, scales="free_y") +
        xlab("") +
        ylab(paste0("TB/HIV mortality (rate/100k pop/yr)",
                    "; blue=", series_1_desc,
                    "; red=", series_2_desc,
                    "; green=", series_3_desc)) +
        expand_limits(y=0) +
        theme_bw(base_size=8) +
        theme(legend.position="none"))

}

plot_rr_new <- function(df){

  # plot prevalence of RR in new pulm bac-confirmed cases (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3

  print(qplot(year, rr_new_series1, data=df, geom="line", colour=I("blue")) +
          geom_ribbon(aes(year,
                          ymin=rr_new_lo_series1,
                          ymax=rr_new_hi_series1),
                      fill=I("blue"), alpha=0.2) +

          geom_line(aes(year, rr_new_series2), colour=I("red")) +
          geom_ribbon(aes(year,
                          ymin=rr_new_lo_series2,
                          ymax=rr_new_hi_series2),
                      fill=I("red"), alpha=0.2) +

          geom_line(aes(year, rr_new_series3), colour=I("green")) +
          geom_ribbon(aes(year,
                          ymin=rr_new_lo_series3,
                          ymax=rr_new_hi_series3),
                      fill=I("green"), alpha=0.2) +
          facet_wrap(~country, scales="free_y") +
          xlab("") +
          ylab(paste0("Proportion of RR among new pulm bac-confirmed cases",
                      "; blue=", series_1_desc,
                      "; red=", series_2_desc,
                      "; green=", series_3_desc)) +
          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="none"))

}


plot_rr_ret <- function(df){

  # plot prevalence of RR in previously treated pulm bac-confirmed cases (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3

  print(qplot(year, rr_ret_series1, data=df, geom="line", colour=I("blue")) +
          geom_ribbon(aes(year,
                          ymin=rr_ret_lo_series1,
                          ymax=rr_ret_hi_series1),
                      fill=I("blue"), alpha=0.2) +

          geom_line(aes(year, rr_ret_series2), colour=I("red")) +
          geom_ribbon(aes(year,
                          ymin=rr_ret_lo_series2,
                          ymax=rr_ret_hi_series2),
                      fill=I("red"), alpha=0.2) +

          geom_line(aes(year, rr_ret_series3), colour=I("green")) +
          geom_ribbon(aes(year,
                          ymin=rr_ret_lo_series3,
                          ymax=rr_ret_hi_series3),
                      fill=I("green"), alpha=0.2) +
          facet_wrap(~country, scales="free_y") +
          xlab("") +
          ylab(paste0("Proportion of RR among previously treated pulm bac-confirmed cases",
                      "; blue=", series_1_desc,
                      "; red=", series_2_desc,
                      "; green=", series_3_desc)) +
          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="none"))

}


plot_inc_rr <- function(df){

  # plot incidence of RR as absoluet numbers (faceted)

  # Blue bands  = series 1
  # Red bands   = series 2
  # Green bands = series 3

  print(qplot(year, inc_rr_series1, data=df, geom="line", colour=I("blue")) +
          geom_ribbon(aes(year,
                          ymin=inc_rr_lo_series1,
                          ymax=inc_rr_hi_series1),
                      fill=I("blue"), alpha=0.2) +

          geom_line(aes(year, inc_rr_series2), colour=I("red")) +
          geom_ribbon(aes(year,
                          ymin=inc_rr_lo_series2,
                          ymax=inc_rr_hi_series2),
                      fill=I("red"), alpha=0.2) +

          geom_line(aes(year, inc_rr_series3), colour=I("green")) +
          geom_ribbon(aes(year,
                          ymin=inc_rr_lo_series3,
                          ymax=inc_rr_hi_series3),
                      fill=I("green"), alpha=0.2) +
          facet_wrap(~country, scales="free_y") +
          xlab("") +
          ylab(paste0("Incidence of RR-TB (number)",
                      "; blue=", series_1_desc,
                      "; red=", series_2_desc,
                      "; green=", series_3_desc)) +
          expand_limits(y=0) +
          theme_bw(base_size=8) +
          theme(legend.position="none"))

}



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot the graphs to PDF -------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get Function to plot multiple graphs to multi-page PDF
source("plot_blocks_to_pdf.r")

block_size <- ifelse(g_whoregion == "SEA", 8, 16)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2010),
                   countries,
                   paste0(outfolder, file_name_inc),
                   plot_function = plot_inc,
                   block_size)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2010),
                   countries,
                   paste0(outfolder, file_name_inc_h),
                   plot_function = plot_inc_h,
                   block_size)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2010),
                   countries,
                   paste0(outfolder, file_name_mort_nh),
                   plot_function = plot_mort_nh,
                   block_size)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2010),
                   countries,
                   paste0(outfolder, file_name_mort_h),
                   plot_function = plot_mort_h,
                   block_size)

# RR timeseries start at 2015!
plot_blocks_to_pdf(estimates_changes |> filter(year >= 2015),
                   countries,
                   paste0(outfolder, file_name_rr_new),
                   plot_function = plot_rr_new,
                   block_size)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2015),
                   countries,
                   paste0(outfolder, file_name_rr_ret),
                   plot_function = plot_rr_ret,
                   block_size)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2015),
                   countries,
                   paste0(outfolder, file_name_inc_rr),
                   plot_function = plot_inc_rr,
                   block_size)



# Optional bit to produce the same charts restricted to the HBCs
plot_blocks_to_pdf(estimates_changes |> filter(year >= 2010),
                   hbc,
                   paste0(outfolder, 'hbc_', file_name_inc),
                   plot_function = plot_inc,
                   block_size)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2010),
                   hbc,
                   paste0(outfolder, 'hbc_', file_name_inc_h),
                   plot_function = plot_inc_h,
                   block_size)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2010),
                   hbc,
                   paste0(outfolder, 'hbc_', file_name_mort_nh),
                   plot_function = plot_mort_nh,
                   block_size)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2010),
                   hbc,
                   paste0(outfolder, 'hbc_', file_name_mort_h),
                   plot_function = plot_mort_h,
                   block_size)

# RR timeseries start at 2015!
plot_blocks_to_pdf(estimates_changes |> filter(year >= 2015),
                   hbc,
                   paste0(outfolder, 'hbc_', file_name_rr_new),
                   plot_function = plot_rr_new,
                   block_size)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2015),
                   hbc,
                   paste0(outfolder, 'hbc_', file_name_rr_ret),
                   plot_function = plot_rr_ret,
                   block_size)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2015),
                   hbc,
                   paste0(outfolder, 'hbc_', file_name_inc_rr),
                   plot_function = plot_inc_rr,
                   block_size)


# Optional bit to produce the charts restricted to the USAID countries priority countries
plot_blocks_to_pdf(estimates_changes,
                   usaid_countries,
                   paste0(outfolder, 'USAID_', file_name_inc),
                   plot_function = plot_inc,
                   block_size = 8)

plot_blocks_to_pdf(estimates_changes,
                   usaid_countries,
                   paste0(outfolder, 'USAID_', file_name_mort_nh),
                   plot_function = plot_mort_nh,
                   block_size = 8)

# RR timeseries start at 2015!
plot_blocks_to_pdf(estimates_changes |> filter(year >= 2015),
                   usaid_countries,
                   paste0(outfolder, 'USAID_', file_name_rr_new),
                   plot_function = plot_rr_new,
                   block_size = 8)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2015),
                   usaid_countries,
                   paste0(outfolder, 'USAID_', file_name_rr_ret),
                   plot_function = plot_rr_ret,
                   block_size = 8)

plot_blocks_to_pdf(estimates_changes |> filter(year >= 2015),
                   usaid_countries,
                   paste0(outfolder, 'USAID_', file_name_inc_rr),
                   plot_function = plot_inc_rr,
                   block_size = 8)

