#' Get tags mapping for a property.
#'
#' This gets list of tags which may be used to store data for the property
#' within the DP file. Also specifies for each of these properties the function
#' which should be used to extract the data from full set of DP data. Plus any
#' other metadata related to property required for accessing.
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
    version = list(
      "VersionNum MV2" = list(
        func = get_tag_data
      ),
      "VersionNum MV1" = list(
        func = get_tag_data
      )
    ),
    valid_date = list(
      "ValidDate MV" = list(
        func = get_tag_notes
      )
    ),
    valid_version = list(
      "ValidVers MV" = list(
        func = get_tag_data
      )
    ),
    yr_start = list(
      "FirstYear MV2" = list(
        func = get_tag_data
      ),
      "FirstYear MV" = list(
        func = get_tag_data
      )
    ),
    yr_end = list(
      "FinalYear MV2" = list(
        func = get_tag_data
      ),
      "FinalYear MV" = list(
        func = get_tag_data
      )
    ),
    relinfectART = list(
      "AdultInfectReduc MV" = list(
        func = get_tag_data
      )
    ),
    total_population = list(
      "BigPop MV3" = list(
        func = get_total_population,
        rows = 2:163
      ),
      "BigPop MV2" = list(
        func = get_total_population,
        ## BigPop MV2 contains pop data split by sex: Male, Female, then
        ## subgrouped on region Total, Urban, Rural then one row for each age
        ## group 0 - 80. We only want to get the total data.
        rows = c(2 + 0:80, 245 + 0:80)
      ),
      "BigPop MV" = list(
        func = get_total_population,
        rows = 2:163
      ),
      "BigPop3" = list(
        func = get_total_population,
        rows = 1:162
      )
    ),
    fert_rat = list(
      "HIVFTR MV4" = list(
        func = get_fert_rat
      ),
      "HIVFTR MV3" = list(
        func = get_fert_rat
      ),
      "HIVFTR MV2" = list(
        func = get_fert_rat
      ),
      "HIVFTR MV" = list(
        func = get_fert_rat
      )
    ),
    stop(sprintf(
      "Can't get the tag names for property %s. Property missing from mapping.",
      property
    ))
  )
  mapping
}

