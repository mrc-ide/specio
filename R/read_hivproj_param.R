read_hivproj_param <- function(pjnz_path) {
  dp_data <- get_dp_data(pjnz_path)
  spectrum_vers <- get_spectrum_version(dp_data)
  tags <- get_dp_tags(spectrum_vers)

  version <- get_dp_property(tags$version, dp_data)
  valid_date <- get_dp_notes(tags$valid_date, dp_data)
  valid_version <- get_dp_property(tags$valid_version, dp_data)

  ## state space dimensions
  NG <- 2
  AG <- 17
  DS <- 7
  TS <- 3

  ## Projection parameters
  yr_start <- get_dp_property(tags$yr_start, dp_data)
  yr_end <- get_dp_property(tags$yr_end, dp_data)
  proj_years <- seq.int(yr_start, yr_end)

  ## scalar params
  relinfectART <- 1.0 - get_dp_property(tags$relinfectART, dp_data)


}
