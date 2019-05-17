#' Extract population data from PJNZ file
#'
#' Get population data separated by year, sex and age. Final age is open ended
#' age group 80+.
#'
#' @param pjnz_path Path to PJNZ file to extract the data from.
#'
#' @return 3D array of population data separated by year, sex and age.
#' @export
#'
#' @examples
#' pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")
#' population <- extract_population(pjnz_path)
#'
extract_population <- function(pjnz_path) {
  dp_data <- get_dp_data(pjnz_path)
  get_property_data("total_population", dp_data, get_projection_years(dp_data))
}

#' Extract people living with HIV (PLHIV) from PJNZ file
#'
#' Get HIV population data separated by year, sex and age. Final age is open
#' ended age group 80+.
#'
#' @param pjnz_path Path to PJNZ file to extract the data from.
#'
#' @return 3D array of HIV population data separated by year, sex and age.
#' @export
#'
#' @examples
#' pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")
#' hiv_population <- extract_hiv_population(pjnz_path)
#'
extract_hiv_population <- function(pjnz_path) {
  dp_data <- get_dp_data(pjnz_path)
  get_property_data("hiv_pop", dp_data, get_projection_years(dp_data))
}
