read_hivproj_param <- function(pjnz_path) {
  dp_data <- get_dp_data(pjnz_path)
  spectrum_vers <- get_spectrum_version(dp_data)

  version <- get_property_data("version", dp_data)
  valid_date <- get_property_data("valid_date", dp_data)
  valid_version <- get_property_data("valid_version", dp_data)

  ## state space dimensions
  NG <- 2
  AG <- 17
  DS <- 7
  TS <- 3

  ## Projection parameters
  yr_start <- get_property_data("yr_start", dp_data)
  yr_end <- get_property_data("yr_end", dp_data)
  proj_years <- seq.int(yr_start, yr_end)

  ## scalar params
  relinfectART <- 1.0 - get_property_data("relinfectART", dp_data)


}

#' Get data for a property from full dp_data via its tag.
#'
#' Gets the list of tags for a particular property, identifies which one
#' should be used and then invokes the configured function for that tag.
#'
#' @param property The property to get data for.
#' @param dp_data The full dp dataset.
#'
#' @return The parsed data
#' @keywords internal
#'
get_property_data <- function(property, dp_data) {
  tags <- get_property_tags(property)
  tag <- get_tag(tags$tags, dp_data)
  tag_data <- get_raw_tag_data(tag, dp_data)
  tags$func(tag_data)
}
