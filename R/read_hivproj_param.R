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

  ## Demographic inputs
  total_population <- get_property_data("total_population", dp_data, proj_years)
  sx <- get_property_data("survey_rate", dp_data, proj_years)
  fertility_rate <- get_property_data("fertility_rate", dp_data, proj_years)
  births_proportion <- get_property_data("asfd", dp_data, proj_years)
  age_specific_fertility_rate <-
    calc_age_specific_fertility_rate(fertility_rate, births_proportion)
  sex_ratio_at_birth <-
    get_property_data("sex_ratio_at_birth", dp_data, proj_years)
  births <- get_property_data("births", dp_data, proj_years)
  total_net_migr <- get_property_data("total_net_migr", dp_data, proj_years)
  net_migr_age_dist <-
    get_property_data("net_migr_age_dist", dp_data, proj_years)
  net_migration <- calc_net_migration(total_net_migr, net_migr_age_dist)

  ## scalar params
  relinfectART <- 1.0 - get_property_data("relinfectART", dp_data)

  output <- list(
    valid_date = valid_date,
    yr_start = yr_start,
    yr_end = yr_end,
    totpop = total_population,
    Sx = sx,
    asfr = age_specific_fertility_rate,
    srb = sex_ratio_at_birth,
    births = births,
    netmigr = net_migration
  )


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
get_property_data <- function(property, dp_data, ...) {
  tags <- get_property_tags(property)
  tag <- get_tag(tags, dp_data)
  tag_data <- get_raw_tag_data(tag, dp_data)
  tags[[tag]]$func(tag, tag_data, tags[[tag]], ...)
}

#' Calculate ASFR from TFR and fertility distribution.
#'
#' Splits each 5 year age group into 5, distributing proportion of births (asfd)
#' evenly between the single ages. Then multiply each age group by the
#' corresponding fertility rate for that that age group and year.
#'
#' @param fertility_rate vector of annual fertility rate values.
#' @param births_proportion array of proportion of births by 5 year age group
#' 15-49.
#'
#' @return array of age-specific fertility rate by single-year of age 15-49.
#'
#' @keywords internal
calc_age_specific_fertility_rate <- function(fertility_rate, births_proportion){
  asfr <- apply(births_proportion / 5, 2, rep, each = 5)
  asfr <- sweep(asfr, 2, fertility_rate, "*")
  dimnames(asfr) <- list(age = 15:49,
                         year = dimnames(births_proportion)[["year"]])
  asfr
}

calc_net_migration <- function(total_net_migr, net_migr_age_dist){

  net_migr <- sweep(net_migr_age_dist, 2:3, total_net_migr, "*")
  ## Disaggregate 5 year age groups into singe-year age groups.
  ## Except for last age group which represents 80+ range.
  net_migr_by_age <- apply(net_migr[-dim(net_migr)["agegr"], , ], 2:3,
                           beers::beers_sub_ordinary)

  ## Add 80+ age group back
  migr_data <- list()
  for (year in dimnames(net_migr_by_age)[["year"]]) {
    migr_data[[year]] <- rbind(net_migr_by_age[, , year],
                               net_migr[dim(net_migr)["agegr"], , year])
  }

  dn <- get_specpop_dimnames(colnames(total_net_migr))
  migr_data <- array(unlist(migr_data), lengths(dn), dn)
  migr_data
}
