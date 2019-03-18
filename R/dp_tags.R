#' Get tags mapping for a property.
#'
#' This gets list of tags which may be used to store data for the property
#' within the DP file. Also specifies for each of these properties the function
#' which should be used to extract the data from full set of DP data. Plus any
#' other metadata related to property required for accessing.
#'
#' @param property Property to get the tags for.
#' @param proj_years Years of the projection.
#'
#' @return List of possible tags used to refer to the property, in order of
#' which they should be used. Also returns the function which should be used
#' to parse the data for the property from the full set of DP data.
#'
#' @keywords internal
get_property_tags <- function(property) {
  if (is.null(tags$config[[property]])) {
    stop(sprintf(
      "Can't get the tag names for property %s. Property missing from mapping.",
      property
    ))
  }
  tags$config[[property]]
}

