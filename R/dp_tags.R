#' Get tags for a specific Spectrum version.
#'
#' @param version Spectrum version to get tags for.
#'
#' @return List of properties mapped to their tag in Spectrum DP file.
#' @export
#'
#' @keywords internal
get_dp_tags <- function(version) {
  if (version == "Spectrum2016") {
    names <- list(
      version = "VersionNum MV",
      valid_date = "ValidDate MV",
      valid_version = "ValidVers MV"
    )
  } else if (version == "Spectrum2017") {
    names <- list(
      version = "VersionNum MV2",
      valid_date = "ValidDate MV",
      valid_version = "ValidVers MV"
    )
  } else {
    stop(sprintf(
      "Can't get tag names for Spectrum version %s, version not supported.",
      version
    ))
  }
}
