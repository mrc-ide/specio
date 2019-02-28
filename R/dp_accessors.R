#' Get a tag from the DP file.
#'
#' Parses data for a particular property.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param metadata Metadata params made available by tag configuration.
#' @param names Names for the returned data.
#'
#' @return The tidied tag data.
#'
#' @keywords internal
get_tag_data <- function(tag, tag_data, metadata, names = NULL) {
  if (row_contains_data(tag_data, 1)) {
    data <- get_data(tag_data)
  } else {
    desc_indices <- get_description_indices(1, nrow(tag_data), tag_data)
    data <- list()
    for (iter in seq_along(desc_indices)) {
      desc <- tag_data[desc_indices[iter], "Description"]
      ## Get data up to next block or if this is the last block get all
      ## remaining data
      end <- desc_indices[iter + 1] - 1
      if (is.na(end)) {
        end <- nrow(tag_data)
      }
      data[[desc]] <- get_data(
        tag_data[seq.int(desc_indices[iter], end), ]
      )
    }
  }
  if (!is.null(names)) {
    names(data) <- names
  }
  data
}

#' Get the notes column for a particular property from DP data.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param ... Other params made available calling function.
#'
#' @return The notes.
#'
#' @keywords internal
get_tag_notes <- function(tag, tag_data, ...) {
  tag_data[, "Notes"]
}


#' Get total population property.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param metadata Metadata params made available by tag configuration.
#' @param proj_years Years the projection is for.
#'
#' @keywords internal
#'
#' Get array data for a particular property from DP data.
#'
#' @param tag_data Filtered tag data.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param metadata Metadata params made available by tag configuration. Must
#' contain rows, and a func to define array dimensions and names.
#' @param proj_years Years the projection is for.
#'
#' @keywords internal
get_array_data <- function(tag, tag_data, metadata, proj_years) {
  if (is.null(metadata$rows) || is.null(metadata$dimensions)) {
    stop(sprintfr("Can't get array data for tag %s. Configuration is
                  incomplete. Must specify rows and dimensions at minimum.
                  rows are null: %s, dimension function is null: %s.",
                  tag, is.null(metadata$rows), is.null(metadata$dimensions)))
  }

  if (is.null(metadata$cols)) {
    ## Default to getting "Data" column up to "Data" column + projection years
    start_index <- get_data_start_column(tag_data)
    metadata$cols <- seq.int(start_index, start_index + length(proj_years) - 1)
  }
  data <- tag_data[metadata$rows, metadata$cols]

  if (!is.null(metadata$type) && metadata$type == "numeric") {
    data <- vapply(data, as.numeric, numeric(nrow(data)))
  }
  if (!is.null(metadata$convert_percent) && metadata$convert_percent) {
    data <- data / 100
  }

  dimensions <- metadata$dimensions(proj_years)
  data <- array(data, lengths(dimensions), dimensions)
  data
}

#' Get fertility ratio property.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param ... Other params made available by tag configuration.
#'
#' @keywords internal
get_fert_rat <- function(tag, tag_data, ...) {

}

#' Get the dimensions and names for specpop data.
#'
#' @param proj_years Vector of years the projection is for.
#'
#' @keywords internal
get_specpop_dimnames <- function(proj_years) {
  list(age = 0:80,
       sex = c("male", "female"),
       year = proj_years)
}

#' Get the dimensions and names for agegr data.
#'
#' @param proj_years Vector of years the projection is for.
#'
#' @keywords internal
get_agegr_dimnames <- function(proj_years) {
  list(agegr = get_agegr_labels()[4:10],
       year = proj_years)
}

#' Get the dimensions and names for agegr, sex and proj years data.
#'
#' @param proj_years Vector of years the projection is for.
#'
#' @keywords internal
get_agegr_and_sex_dimnames <- function(proj_years) {
  list(agegr = get_agegr_labels(),
       sex = c("male", "female"),
       year = proj_years)
}

get_sex_dimnames <- function(proj_years) {
  list(sex = c("male", "female"), year = proj_years)
}

#' Get the age group labels.
#'
#' @param proj_years Vector of years the projection is for.
#'
#' @keywords internal
get_agegr_labels <- function() {
  c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
    "70-74", "75-79", "80+")
}

