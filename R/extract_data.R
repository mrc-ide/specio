#' Extract population data from PJNZ file
#'
#' Get population data separated by year, sex and age. Final age is open ended
#' age group 80+.
#'
#' @param pjnz_path Path to PJNZ file to extract the data from.
#' @param long_format If TRUE then data is returned in long format.
#'
#' @return 3D array of population data separated by year, sex and age.
#' @export
#'
#' @examples
#' pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")
#' population <- get_total_pop(pjnz_path)
#' pop_long <- get_total_pop(pjnz_path, TRUE)
#'
get_total_pop <- function(pjnz_path, long_format = FALSE) {
  get_data_from_pjnz(pjnz_path, "total_population", long_format, "total_pop")
}

#' Extract people living with HIV (PLHIV) from PJNZ file
#'
#' Get HIV population data separated by year, sex and age. Final age is open
#' ended age group 80+.
#'
#' @param pjnz_path Path to PJNZ file to extract the data from.
#' @param long_format If TRUE then data is returned in long format.
#'
#' @return 3D array of HIV population data separated by year, sex and age.
#' @export
#'
#' @examples
#' pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")
#' hiv_population <- get_hiv_pop(pjnz_path)
#' hiv_pop_long <- get_hiv_pop(pjnz_path, TRUE)
#'
get_hiv_pop <- function(pjnz_path, long_format = FALSE) {
  get_data_from_pjnz(pjnz_path, "hiv_pop", long_format, "hiv_pop")
}

#' Extract ART population data from PJNZ file
#'
#' Get ART population data separated by year, sex and age. Final age is open
#' ended age group 80+.
#'
#' @param pjnz_path Path to PJNZ file to extract the data from.
#' @param long_format If TRUE then data is returned in long format.
#'
#' @return 3D array of ART population data separated by year, sex and age.
#' @export
#'
#' @examples
#' pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")
#' art_population <- get_art_pop(pjnz_path)
#' art_pop_long <- get_art_pop(pjnz_path, TRUE)
#'
get_art_pop <- function(pjnz_path, long_format = FALSE) {
  get_data_from_pjnz(pjnz_path, "art_pop", long_format, "art_pop")
}

#' Get data from PJNZ file for a single property
#'
#' Extract the data for a single property from the PJNZ file. This accesses the
#' data from the DP file contained within the PJNZ zip.
#'
#' @param pjnz_path Path to PJNZ file to get data from.
#' @param property The name of the property to return data for.
#' @param long_format If TRUE data is converted to a long format.
#' @param long_format_label If long_format is TRUE then this is used as column
#' name for the long format data.
#'
#' @return Data extracted from PJNZ.
#'
#' @keywords internal
get_data_from_pjnz <- function(pjnz_path, property, long_format,
                            long_format_label) {
  dp_data <- get_dp_data(pjnz_path)
  data <- get_property_data(property, dp_data, get_projection_years(dp_data))
  if (long_format) {
    data <- utils::type.convert(
      as.data.frame.table(data, responseName = long_format_label))
  }
  data
}
