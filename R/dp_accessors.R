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


#' Get array data for a particular property from DP data.
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

#' Get vector data for a particular property from DP data.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param metadata Metadata params made available by tag configuration. Must
#' contain cols to get the data for.
#' @param proj_years Years the projection is for.
#'
#' @keywords internal
get_vector_data <- function(tag, tag_data, metadata, proj_years) {
  if (is.null(metadata$cols)) {
    stop(sprintfr("Can't get vector data for tag %s. Configuration is
                  incomplete. Must specify cols at a minimum.", tag))
  }
  if (nrow(tag_data) != 1) {
    stop(sprintfr("Can't get vector data for tag %s. Must only be 1 row
                  of data for this tag to get as a vector. Got %i rows.",
                  tag, nrow(tag_data)))
  }
  data <- tag_data[, metadata$cols]
  if (!is.null(metadata$type) && metadata$type == "numeric") {
    data <- as.numeric(data)
  }
  data
}

#' Get array data for a cd4 property from DP data.
#'
#' Require some special handling because each row contains data for an age
#' group and a cd4 stage.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param metadata Metadata params made available by tag configuration. Must
#' contain rows, and a func to define array dimensions and names.
#' @param proj_years Years the projection is for.
#'
#' @keywords internal
get_cd4_array_data <- function(tag, tag_data, metadata, proj_years) {
  if (is.null(metadata$rows$male) || is.null(metadata$rows$female)) {
    stop(sprintfr("Can't get CD4 array data for tag %s. Must specify rows
                  for both male and female data.", tag))
  }

  ## One row for each sex and each row contains data for each age group and
  ## cd4 stage. 15-24 stage 1, 15-24 stage 2, ..., 45+ stage 7. So handle
  ## male and female separately and join into 3D array afterwards.
  male_metadata <- metadata
  male_metadata$rows <- metadata$rows$male
  male_data <- get_array_data(tag, tag_data, male_metadata, proj_years)

  female_metadata <- metadata
  female_metadata$rows <- metadata$rows$female
  female_data <- get_array_data(tag, tag_data, female_metadata, proj_years)

  dimensions <- metadata$dimensions(proj_years)
  dimensions$sex <- c("male", "female")
  array(c(male_data, female_data), lengths(dimensions), dimensions)
}

#' Get ART mortality rate data from DP data.
#'
#' Requires some special handling as we need the data separated by
#' time on ART but this isn't always provided by Spectrum.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param metadata Metadata params made available by tag configuration. Must
#' contain rows.
#' @param proj_years Years the projection is for.
#'
#' @keywords internal
get_art_mortality_rates <- function(tag, tag_data, metadata, proj_years) {
  if (is.null(metadata$rows)) {
    stop(sprintfr("Can't get art mortality rates using tag %s, rows
                  must be specified by configuration.", tag))
  }
  if (is.null(metadata$cols)) {
    ## Default to getting "Data" column up to "Data" column + projection years
    start_index <- get_data_start_column(tag_data)
    metadata$cols <- seq.int(start_index, start_index + length(proj_years) - 1)
  }
  mortality_data <- tag_data[metadata$rows, metadata$cols]
  array_data <- list(value = 1.0,
                     dimensions = dimensions_art)
  mortality_rates <- get_default_array(array_data, proj_years)

  if (nrow(mortality_data) == 1) {
    mortality_rates["ART0MOS", ] <- as.numeric(mortality_data[1, ])
    mortality_rates["ART6MOS", ] <- as.numeric(mortality_data[1, ])
    mortality_rates["ART1YR", ] <- as.numeric(mortality_data[1, ])
  } else if (nrow(mortality_data) == 2) {
    mortality_rates["ART0MOS", ] <- as.numeric(mortality_data[1, ])
    mortality_rates["ART6MOS", ] <- as.numeric(mortality_data[1, ])
    mortality_rates["ART1YR", ] <- as.numeric(mortality_data[2, ])
  } else {
    stop(sprintfr("Can't handle %s with %i rows. Either config is
                  misconfigured or this is a new case which needs handling.",
                  tag, nrow(mortality_data)))
  }
  mortality_rates
}

#' Get women on ART data.
#'
#' Handles case when the data only has a single value for women on ART. We
#' need 1 data point for each age group so just repeat the single value.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param metadata Metadata params made available by tag configuration.
#' @param agegr_labels Age group labels.
#'
#' @keywords internal
get_women_on_art <- function(tag, tag_data, metadata, agegr_labels) {
  women_on_art <- rep(get_tag_data(tag, tag_data, NULL), cfg$params$fAG)
  names(women_on_art) <- agegr_labels
  women_on_art
}

#' Get ART eligibility population data.
#'
#' @param tag The name of the tag getting data for.
#' @param tag_data Tag data to be parsed.
#' @param metadata Metadata params made available by tag configuration.
#' @param agegr_labels Age group labels.
#'
#' @keywords internal
get_eligibility_pop_data <- function(tag, tag_data, metadata, proj_years) {
  if (is.null(metadata$rows) || is.null(metadata$cols)) {
    stop(sprintfr("Can't get eligibility pop data via tag %s. Configuration is
                  incomplete. Must specify rows and cols at minimum.
                  rows are null: %s, cols are null: %s.",
                  tag, is.null(metadata$rows), is.null(metadata$cols)))
  }

  data <- tag_data[metadata$rows, metadata$cols]
  data <- stats::setNames(data, c("description", "pop", "elig", "percent", "year"))
  data$pop <- c("PW", "TBHIV", "DC", "FSW", "MSM", "IDU", "OTHER")
  data$elig <- as.logical(as.integer(data$elig))
  data$percent <- as.numeric(data$percent)/100
  data$year <- as.integer(data$year)
  data$idx <- match(as.integer(data$year), proj_years)
  data
}

#' Get default array data
#'
#' Defaults to creating a data frame of a single value of dimensions specified
#' by metadata.
#'
#' @param metadata List containing value for array and dimensions of array.
#' @param proj_years Years the projection is for.
#'
#' @keywords internal
get_default_array <- function(metadata, proj_years) {
  if (is.null(metadata$dimensions) || is.null(metadata$value)) {
    stop(sprintfr("Must supply default dimensions function and value to create
         default array. Dimensions is null: %s  and value is null: %s",
         is.null(metadata$dimensions), is.null(metadata$value)))
  }
  dims <- metadata$dimensions(proj_years)
  array(eval(parse(text = metadata$value)), lengths(dims), dims)
}


#' Get the age group labels.
#'
#' @param between_15_49_only Whether to get age ranges between 15 and 49 only.
#'
#' @keywords internal
get_agegr_labels <- function(between_15_49_only = FALSE) {
  age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                  "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                  "70-74", "75-79", "80+")
  if (between_15_49_only) {
    age_groups <- age_groups[4:10]
  }
  age_groups
}

