#' Get a property from the DP file.
#'
#' Finds a property via a matching "Tag" and extracts the data contained in
#' the tag.
#'
#' @param property Property to get.
#' @param dp_data Full set of untidied dp data.
#'
#' @return The tidied property data.
#'
#' @keywords internal
get_dp_property <- function(property, dp_data) {
  range <- get_data_indices(property, dp_data)

  if (row_contains_data(range$start, dp_data)) {
    data <- get_data(dp_data[seq.int(range$start, range$end - 1), ])
  } else {
    desc_indices <- get_description_indices(range$start, range$end, dp_data)
    data_blocks <- c(desc_indices, range$end)
    data <- list()
    for (iter in seq_along(desc_indices)) {
      desc <- dp_data[data_blocks[iter], "Description"]
      data[[desc]] <- get_data(
        dp_data[seq.int(data_blocks[iter], data_blocks[iter + 1] - 1), ]
      )
    }
  }
  data
}

#' Get the start and end indices of a particular property within full dataset.
#'
#' End index is inclusive of row containing <End> tag.
#'
#' @param property Property to get.
#' @param dp_data Full set of untidied dp data.
#'
#' @return List contianing the start and end indices of the data.
#'
#' @keywords internal
get_data_indices <- function(property, dp_data) {
  start_row <- which(dp_data[, "Tag"] == paste0("<", property, ">"))
  if (length(start_row) != 1) {
    stop(
      sprintf("Can't find exactly 1 property matching Tag %s, found %i.",
              property, length(start_row))
    )
  }
  end_row <- next_index("<End>", start_row, "Tag", dp_data)

  data_start <- next_index("<Value>", start_row, "Description", dp_data)
  list(start = data_start, end = end_row)
}

#' Get the notes column for a particular property from DP data.
#'
#' @param property Property to get notes for.
#' @param dp_data Full set of untidied dp data.
#'
#' @return The notes.
#'
#' @keywords internal
get_dp_notes <- function(property, dp_data) {
  range <- get_data_indices(property, dp_data)
  ## Use end - 1 to ignore row containing <End> tag.
  dp_data[seq.int(range$start, range$end - 1), "Notes"]
}

#' Get the next index of a tag.
#'
#' Get the row index of the next occurance of a tag in a specified column
#' starting from a specified row.
#'
#' @param tag Tag to look for
#' @param start_index Index to start looking from.
#' @param column Column to look for the tag in.
#' @param dp_data The dp data to look in.
#'
#' @keywords internal
next_index <- function(tag, start_index, column, dp_data) {
  occurances <- which(dp_data[, column] == tag)
  occurances <- occurances[occurances >= start_index]
  if (length(occurances) == 0) {
    stop(sprintf(
      "Can't find next occurance of tag %s starting from index %i.",
      tag, start_index
    ))
  }
  occurances[1]
}

#' Get the indices for which there is a non empty field in the description col.
#'
#' Gets all indices of not empty description fields, excluding <Value> tag.
#' Use these to identify separate blocks of data within the dp data. Get these
#' between two indices
#'
#' @param start_index Index to start looking from.
#' @param end_index Index to look up to.
#' @param dp_data The full set of dp data.
#'
#' @keywords internal
get_description_indices <- function(start_index, end_index, dp_data) {
  desc_indices <- which(!is.na_or_empty(dp_data[, "Description"]) &
    dp_data[, "Description"] != "<Value>")
  desc_indices[start_index <= desc_indices & desc_indices < end_index]
}

#' Get the data from the 'Data' column and onwards if present.
#'
#' Gets the data from the 'Data' column and onwards. If no data is found
#' returns null. Also tries to set rownames as the 'Notes' where these are
#' present. This expects to receive only the relevant rows, this does the work
#' to filter the relevant columns only.
#'
#' @param dp_data The subsetted dp data to extract data from.
#'
#' @keywords internal
get_data <- function(dp_data) {
  data_start_column <- which(colnames(dp_data) == "Data")[1]
  data_end_column <- get_last_non_na_column(dp_data)
  if (data_end_column < data_start_column) {
    data <- NULL
  } else {
    data <- dp_data[, seq.int(data_start_column, data_end_column)]
    data <- convert_type(data)
    has_rows_and_rownames <- !is.null(nrow(data)) && nrow(data) >= 0 &&
      (all(!is.na(dp_data[, "Notes"]) & !(dp_data[, "Notes"] == "")))
    if (has_rows_and_rownames) {
      rownames(data) <- dp_data[, "Notes"]
    }
  }
  data
}


#' Convert filtered data to correct type.
#'
#' Now data has been filtered to only data for a specific property we can
#' ensure that the data type is set to numeric where possible.
#'
#' @param data
#'
#' @return The converted data
#'
#' @keywords internal
convert_type <- function(data) {
  if (typeof(data) == "character") {
    data <- utils::type.convert(data, as.is = TRUE)
  } else if (class(data) == "data.frame") {
    vapply(data, function(x) {
      if (typeof(x) == "character") {
        utils::type.convert(x)
      } else {
        x
      }
    }, numeric(nrow(data)))
  } else {
    data
  }
}



#' Check whether a row contains data.
#'
#' A row is considered to contain data if the fields from the 'Data' column
#' to the final column are not all NA values.
#'
#' @param index The index of the row to check.
#' @param dp_data The dp_data to check.
#'
#' @keywords internal
row_contains_data <- function(index, dp_data) {
  data_start_column <- which(colnames(dp_data) == "Data")[1]
  row_data <- dp_data[index, seq.int(data_start_column, ncol(dp_data))]
  !all(is.na_or_empty(row_data))
}

#' Get the column index of the last non-NA non-empty column.
#'
#' This locates the last non-NA non-empty column i.e. the last column which
#' contains data from a subset of the dp_data. Used to remove spurious NAs or
#' empty strings (depending on column type) appended to the end of the data due
#' to using a csv to store data.
#'
#' @param dp_data The subset of dp_data to check.
#'
#' @keywords internal
get_last_non_na_column <- function(dp_data) {
  index <- ncol(dp_data)
  while (all(is.na_or_empty(dp_data[, index])) && index > 0) {
    index <- index - 1
  }
  index
}

is.na_or_empty <- function(x) {
  is.na(x) | x == ""
}

#' Get the version of Spectrum the dp data is for.
#'
#' Find the version of the Spectrum which created the dp data by inspecting a
#' tag.
#'
#' @param dp_data The dp_data to get the version for.
#'
#' @keywords internal
get_spectrum_version <- function(dp_data) {
  ## Infer the version based on the first tag which is in 1st column of 2nd row
  test_tag <- dp_data[2, 1]
  if (test_tag == "<FirstYear MV>") {
    dp_vers <- "Spectrum2016"
  } else if (test_tag == "<FirstYear MV2>") {
    dp_vers <- "Spectrum2017"
  } else {
    stop("Spectrum DP file version not recognised. Only Spectrum versions from 2016 onwards are supported.")
  }
  dp_vers
}
