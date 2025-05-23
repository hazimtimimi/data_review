# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Compare data from previous years used to estimate RR-TB prevalence with
# the data reported in the most recent year
#
#
# Hazim Timimi, May 2024, based on Stata code from Anna Dean
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source("set_environment.r")  #particular to each person so this file is in the ignore list
source("set_plot_themes.r")

# load packages ----
library(RODBC)
library(ggplot2)
library(dplyr)

file_name_new     <- paste0(outfolder, "drs_new_graphs_", Sys.Date(), ".pdf")
file_name_ret     <- paste0(outfolder, "drs_ret_graphs_", Sys.Date(), ".pdf")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the data  ----
#
# I prefer to do this via SQL, but could be done of course with the pure views
# and some R jiggery pokery
#
# The query combines data from the master notification view with latest data
# reported as retreived from the dcf views
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sql_new <- "
  SELECT	country, iso3, g_whoregion, year_new, source_new, surv_quality_new AS surv_quality, NULL AS pulm_labconf_new, r_rlt_new, rr_new,
  		CASE WHEN source_new = 'Survey' THEN rr_new_pcnt
  			 WHEN source_new = 'Surveillance' AND r_rlt_new > 0 THEN rr_new * 100.0 / r_rlt_new
  			 ELSE NULL
  		END AS rr_new_pcnt
  FROM	view_DRS_for_estimation_new
  WHERE	all_areas_covered_new=1 AND
  		year_new BETWEEN 2015 AND (SELECT MAX(year - 1) FROM dcf.latest_notification) AND
  		ISNULL(r_rlt_new,0) > 0

  UNION ALL

  /* Latest records used for estimation of RR prevalence among new cases */
  SELECT	country,iso3, g_whoregion, year_new, 'Surveillance' AS source_new, surv_quality, pulm_labconf_new, r_rlt_new ,rr_new,
  		CASE WHEN r_rlt_new > 0 THEN rr_new * 100.0 / r_rlt_new
  			 ELSE NULL
  		END AS rr_new_pcnt
  FROM	dcf.latest_dr_surveillance_for_estimation
  WHERE	ISNULL(r_rlt_new,0) > 0

  ORDER BY country, year_new;"

sql_ret <- stringr::str_replace_all(sql_new, "_new", "_ret")

sql_wrd <- "
  SELECT	dcf.latest_strategy.country, dcf.latest_strategy.iso2, dcf.latest_strategy.year, m_wrd_tests_positive,
  		(ISNULL(r_rlt_new, 0) + ISNULL(r_rlt_ret, 0) + ISNULL(r_rlt_unk, 0)) AS r_rlt
  FROM	dcf.latest_strategy
		INNER JOIN dcf.latest_notification ON
				dcf.latest_strategy.iso2 = dcf.latest_notification.iso2

  WHERE	 ISNULL(m_wrd_tests_positive, 0) > 0 AND
  		(ISNULL(r_rlt_new, 0) + ISNULL(r_rlt_ret, 0) + ISNULL(r_rlt_unk, 0)) > 0 AND
		/* Restrict to records that qualify for use in estimation */
		dcf.latest_strategy.iso2 IN (SELECT iso2 FROM dcf.latest_dr_surveillance_for_estimation)

  ORDER BY dcf.latest_strategy.country;"


# Extract data from the database
channel <- odbcDriverConnect(connection_string)
rr_new <- sqlQuery(channel,sql_new)
rr_ret <- sqlQuery(channel,sql_ret)
wrd <- sqlQuery(channel,sql_wrd)

close(channel)

# Store list of countries for plotting
countries_new <- rr_new |>  select(country) |>  arrange(toupper(country)) |> unique()
countries_ret <- rr_ret |>  select(country) |>  arrange(toupper(country)) |> unique()


# Define graph layout ----
# - - - - - - - - - - -

plot_faceted <- function(df){

  case_type = head(df$case_type, 1)

  graphs <- df %>%
    ggplot() +

    geom_point(aes(year, rr_pcnt, colour=source_est), size=1 ) +

    scale_y_continuous(name = paste("Rif-resistance among", case_type, "(%) (Surveys in blue)")) +

    scale_x_continuous(name="", breaks = seq(2015, max(df$year), by=2)) +

    scale_colour_manual(values = c("Survey" = "blue",
                                   "Surveillance" = "green")) +

    facet_wrap(~country,
               scales="free_y",
               # Use the labeller function to make sure long country names are wrapped in panel headers
               labeller = label_wrap_gen(width = 22)) +

    expand_limits(y=0) +

    theme_gtbr_2021(base_size=8, axis_text_size = 6) +
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

# Plot new cases trend
rr_new |>
  select(country, year = year_new, rr_pcnt = rr_new_pcnt, source_est = source_new) |>
  mutate(case_type = "new cases") |>
  plot_blocks_to_pdf(countries_new, file_name_new)


# Plot previously treated cases trend
rr_ret |>
  select(country, year = year_ret, rr_pcnt = rr_ret_pcnt, source_est = source_ret) |>
  mutate(case_type = "previously treated cases") |>
  plot_blocks_to_pdf(countries_ret, file_name_ret)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get listing of countries where no surveillance record for latest year,
# but did have surveillance record for previous year
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

latest_year <- max(rr_new$year_new)

rr_new_not_latest_year <- rr_new |>
  filter(year_new == latest_year - 1 & source_new == "Surveillance") |>
  left_join(filter(rr_new, year_new == latest_year & source_new == "Surveillance" ), by = "iso3") |>
  filter(is.na(country.y)) |>
  select(country.x, year_new.x, source_new.x, surv_quality.x)

rr_ret_not_latest_year <- rr_ret |>
  filter(year_ret == latest_year - 1 & source_ret == "Surveillance") |>
  left_join(filter(rr_ret, year_ret == latest_year & source_ret == "Surveillance" ), by = "iso3") |>
  filter(is.na(country.y)) |>
  select(country.x, year_ret.x, source_ret.x, surv_quality.x)

# Get listing of countries where no surveillance record for previous year,
# but did have surveillance record for the most recent year

rr_new_just_latest_year <- rr_new |>
  filter(year_new == latest_year - 1 & source_new == "Surveillance") |>
  right_join(filter(rr_new, year_new == latest_year & source_new == "Surveillance" ), by = "iso3") |>
  filter(is.na(country.x)) |>
  select(country.y, year_new.y, source_new.y, surv_quality.y)

rr_ret_just_latest_year <- rr_ret |>
  filter(year_ret == latest_year - 1 & source_ret == "Surveillance") |>
  right_join(filter(rr_ret, year_ret == latest_year & source_ret == "Surveillance" ), by = "iso3") |>
  filter(is.na(country.x)) |>
  select(country.y, year_ret.y, source_ret.y, surv_quality.y)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Compare testing for RR with numbers positive by WRDs as reporting in the
# lab section
#
# Anna said:
# in countries relying on Xpert, we would expert similar numbers
# number pos for TB by rapid test should not be greater than number with DST because the rapid test would also provide a RIF result
# (however, in practice, there can be issues in disaggregating data by treatment history and so DST cannot be fully reported due to lack of treatment history)
# number with DST result can be higher than pos for TB by rapid test if culture and DST are performed commonly
#
# Hazim: Another reason for disparity is that WRD results are by test rather than by patients.
# Hazim: ANother reason given by some countries is that there may be many "trace" results from WRDs that trigger
# additional testing on the same patient
#
# Look for 10% difference or more
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rr_rlt_v_wrd <- wrd |>
  mutate(diff_pcnt = round((m_wrd_tests_positive - r_rlt) * 100 / r_rlt)) |>
  filter(abs(diff_pcnt) > 10)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get listing of countries where testing rate was 100%
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rr_rlt_100_pcnt_new <- rr_new |>
  filter(year_new == latest_year & r_rlt_new == pulm_labconf_new)


rr_rlt_100_pcnt_ret <- rr_ret |>
  filter(year_ret == latest_year & r_rlt_ret == pulm_labconf_ret)

