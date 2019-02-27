#' Get a tag from the DP file.
#'
#' Parses data for a particular property.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param ... Other params made available calling function.
#'
#' @return The tidied tag data.
#'
#' @keywords internal
get_tag_data <- function(tag, tag_data, ...) {
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

#' Get the notes column for a particular property from DP data.
#'
#' @param tag_data Filtered tag data.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param ... Other params made available calling function.
#'
#' @keywords internal
get_array_data <- function(tag, tag_data, rows, cols) {
  ## Ignore the row containing the value tag
  tag_data[rows, cols]
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

#' Get total population property.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param metadata Metadata params made available by tag configuration.
#' @param ... Other params made available calling function.
#'
#' @keywords internal
get_total_population <- function(tag, tag_data, metadata, proj_years) {
  cols <- seq.int(get_data_start_column(tag_data), length(proj_years))
  data <- get_array_data(tag, tag_data, metadata$rows, cols)
  data <- sapply(data, as.numeric)

  dn <- get_specpop_dimnames(proj_years)
  data <- array(data, lengths(dn), dn)
  data
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
