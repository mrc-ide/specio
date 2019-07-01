#' Get projection metadata from PJN file data
#'
#' @param pjn_data Data frame of PJN file data
#'
#' @keywords internal
get_pjn_metadata <- function(pjn_data) {
  properties <- list()
  country <- get_pjn_country(pjn_data)
  properties$country <- country$country
  properties$iso3 <- country$iso3
  properties$iso_numeric <- country$iso_numeric
  properties$region <- get_pjn_region(pjn_data)
  properties$region_code <- get_pjn_region_code(pjn_data)
  properties$spectrum_version <-
    pjn_data[which(pjn_data[, "Tag"] == "<Projection General>") + 4, "Data"]
  properties$projection_name <-
    pjn_data[which(pjn_data[, "Tag"] == "<Projection Name>") + 2, "Data"]
  properties
}

#' Get country name and code from parsed PJN
#'
#' @keywords internal
get_pjn_country <- function(pjn) {
  cc <- as.integer(
    pjn[which(pjn[, "Tag"] == "<Projection Parameters>") + 2, "Data"]
  )
  spectrum5_countrylist <- readRDS(
    system.file("spectrum5_countrylist.rds", package = "specio")
  )
  idx <- which(spectrum5_countrylist$Code == cc)
  list(
    country = spectrum5_countrylist$Country[idx],
    iso3 = spectrum5_countrylist$iso3[idx],
    iso_numeric = cc
  )
}

#' Get subnational region from parsed PJN
#'
#' @keywords internal
get_pjn_region <- function(pjn) {
  region <- pjn[which(
    pjn[, "Tag"] == "<Projection Parameters - Subnational Region Name2>") + 2,
    "Data"]
  if (region == "") {
    return(NULL)
  } else {
    return(region)
  }
}

#' Get subnational region code from parsed PJN
#'
#' @keywords internal
get_pjn_region_code <- function(pjn) {
  idx <- which(pjn[, "Tag"] == "<Projection Parameters - Subnational Region Name2>") + 3
  region_code <- pjn[idx, "Data"]

  if (region_code == "") {
    return(NULL)
  }

  as.integer(region_code)
}