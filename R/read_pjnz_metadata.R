#' Extract projection metadata from PJNZ
#'
#' @param pjnz_path path to PJNZ file
#'
#' @return List consisting of:
#'   * country name,
#'   * Country ISO Alpha-3 code,
#'   * Country ISO numeric code (3-digit)
#'   * Spectrum subnational region
#'   * Spectrum subnational region code
#'   * Spectrum version
#'   * Projection name
#'
#' @details
#' Returns metadata stored in the .PJN file with .PJNZ archive.
#'
#' @export
read_pjnz_metadata <- function(pjnz_path) {
  pjn_data <- get_pjn_data(pjnz_path)
  get_pjn_metadata(pjn_data)
}
