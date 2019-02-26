#' Get a tag from the DP file.
#'
#' Parses data for a particular property.
#'
#' @param tag_data Data to be tided.
#'
#' @return The tidied tag data.
#'
#' @keywords internal
get_tag_data <- function(tag_data) {
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
#' @param tag_data Filteres tag data.
#'
#' @return The notes.
#'
#' @keywords internal
get_tag_notes <- function(tag_data) {
  tag_data[, "Notes"]
}

#' Get fertility ratio property
#'
#' @return The tidied property data.
#'
#' @keywords internal
get_fert_rat <- function(tag, dp_data) {

}
