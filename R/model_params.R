#' Get model parameters.
#'
#' \describe{
#'   \item{NG}{Number of sexes (male, female)}
#'   \item{AG}{Number of age groups (5 year groups from 0-4, ..., 75-79, 80+)}
#'   \item{DS}{Number of disease stages (CD4 count stages: >500, 350-499,
#'   250-349, 200-249, 100-199, 50-99, <5)}
#'   \item{TS}{Number of treatment stages (<6 months, 6-12 months, >1 year)}
#'   \item{fAG}{Number of age groups for fertility (15-19, 20-24, ..., 45-49)}
#'   \item{PAED_DS}{Number of disease stages for paediatric HIV infection}
#' }
#'
#' @return List of model params.
#'
#' @keywords internal
get_model_params <- function() {
  list(
    NG = 2,
    AG = 17,
    DS = 7,
    TS = 3,
    fAG = 7,
    PAED_DS = 6
  )
}
