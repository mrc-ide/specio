#' Get CD4 stage and age category dimensions.
#'
#' @param proj_years Vector of years the projection is for.
#'
#' @keywords internal
dimensions_cd4 <- function(proj_years = NULL) {
  list(
    cd4stage = seq_len(cfg$params$DS),
    agecat = dimesions_cd4_agecat()
  )
}

#' Get dimensions for adult CD4 progress data.
#'
#' Stages of infection and age categories.
#'
#' @param proj_years Vector of years the projection is for.
#'
#' @keywords internal
dimensions_cd4_progress <- function(proj_years) {
  list(
    cd4stage = seq_len(cfg$params$DS - 1),
    agecat = dimesions_cd4_agecat()
  )
}

dimesions_cd4_agecat <- function() {
  c("15-24", "25-34", "35-44", "45+")
}

#' Get artdur and year dimensions.
#'
#' @param proj_years Vector of years the projection is for.
#'
#' @keywords internal
dimensions_art <- function(proj_years) {
  list(
    artdur = c("ART0MOS", "ART6MOS", "ART1YR"),
    year = proj_years
  )
}

#' Get age group, sex and year dimensions.
#'
#' @param proj_years Vector of years the projection is for.
#'
#' @keywords internal
dimensions_agegr_sex_year <- function(proj_years) {
  list(agegr = get_agegr_labels(),
       sex = c("male", "female"),
       year = proj_years)
}

dimensions_sex_year <- function(proj_years) {
  list(sex = c("male", "female"),
       year = proj_years)
}

#' Get single age between 0 and 80, sex and year dimensions.
#'
#' @param proj_years Vector of years the projection is for.
#'
#' @keywords internal
dimensions_age_sex_year <- function(proj_years) {
  list(age = 0:80,
       sex = c("male", "female"),
       year = proj_years)
}

#' Get age groups between 15 and 49 and year dimensions.
#'
#' @param proj_years Vector of years the projection is for.
#'
#' @keywords internal
dimensions_agegr_year <- function(proj_years) {
  list(agegr = get_agegr_labels(between_15_49_only = TRUE),
       year = proj_years)
}

#' Get age groups between 15 and 49 and year dimensions alternatives.
#'
#' Used for different Spectrum version than `dimensions_agegr_year`.
#'
#' @param proj_years Vector of years the projection is for.
#'
#' @keywords internal
dimensions_agegr_year_alternative <- function(proj_years) {
  list(agegr = c("15-17", "18-19", "20-24", "25-29", "30-34", "35-49"),
       year = proj_years)
}

#' Get dimensions for age 14 hiv population tag.
#'
#' ART stage, CD4 category, sex and year.
#'
#' @param proj_years Vector of years the projection is for.
#'
#' @keywords internal
dimensions_age_14_hiv_population <- function(proj_years) {
  list(
    artstage = c("PERINAT", "BF0MOS", "BF6MOS", "BF1YR", "ART0MOS",
                 "ART6MOS", "ART1YR"),
    cd4cat = c("CD4_1000", "CD4_750", "CD4_500", "CD4_350", "CD4_200", "CD4_0"),
    sex = c("male", "female"),
    year = proj_years
  )
}
