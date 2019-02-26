#' Get tags mapping for a property.
#'
#' This gets list of tags which may be used to store data for the property
#' within the DP file. Also specifies for each of these properties the function
#' which should be used to extract the data from full set of DP data.
#'
#' @param property Property to get the tags for.
#'
#' @return List of possible tags used to refer to the property, in order of
#' which they should be used. Also returns the function which should be used
#' to parse the data for the property from the full set of DP data.
#'
#' @keywords internal
get_property_tags <- function(property) {
  mapping <- switch(
    property,
    version       = tag(c("VersionNum MV2", "VersionNum MV1"),
                        get_dp_property),
    valid_date    = tag(c("ValidDate MV"),
                        get_dp_notes),
    valid_version = tag(c("ValidVers MV"),
                        get_dp_property),
    yr_start      = tag(c("FirstYear MV2", "FirstYear MV"),
                        get_dp_property),
    yr_end        = tag(c("FinalYear MV2", "FinalYear MV"),
                        get_dp_property),
    relinfectART  = tag("AdultInfectReduc MV",
                        get_dp_property),
    stop(sprintf(
      "Can't get the tag names for property %s. Property missing from mapping.",
      property
    ))
  )
  mapping
}


tag <- function(tags, func) {
  list(tags = tags, func = func)
}

