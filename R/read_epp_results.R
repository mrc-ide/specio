#' Read best fitting HIV incidence and prevalence data from SPT file.
#'
#' Locates SPT file within the PJNZ file and reads out the best fitting
#' incidence and prevalence data into memory.
#'
#' Reads best fitting prevalence and incidence and population data for all
#' regions. The outputs one data frame for each region in the SPT file.
#'
#' @param pjnz_path Path to the PJNZ file.
#'
#' @return List of data frames containing the best fit prevalence and incidence
#' data.
#' @export
#'
#' @examples
#' pjnz_path <- system.file("testdata", "Botswana2018.PJNZ", package="specio")
#' read_spt(pjnz_path)
#'
read_spt <- function(pjnz_path){
  spt_file <- readlines_from_path(pjnz_path, "SPT")

  ## Data for a particular region starts with "==" delimiter
  region_row_breaks <- which(spt_file == "==")
  ## Incidence and prevalence data ends with "=" delimiter
  incid_prev_row_breaks <- which(spt_file == "=")
  no_of_years <- incid_prev_row_breaks[2] - region_row_breaks[1] - 3
  regions_str <- stats::na.omit(spt_file[region_row_breaks + 1])

  ## Expect regions to have identifier e.g. Botswana_ 2017_2\Urban:URBAN,NO,50.0
  ## National region is not labelled, add manually
  regions <- rep("National", length(regions_str))
  re <- ".+\\\\([A-Za-z ]+):.+"
  i <- grepl(re, regions_str)
  regions[i] <- sub(re, "\\1", regions_str[i])


  ## Ignore first break as the "National" data is repeated
  spt_data <- lapply(incid_prev_row_breaks[-1],
                     extract_incidence_prevalence,
                     spt_data = spt_file,
                     no_of_years = no_of_years)

  names(spt_data) <- regions
  return(spt_data)
}

#' Read EPP uncertainty results from SPU file.
#'
#' @param pjnz_path Path to the PJNZ file.
#'
#' @return List containing uncertainty results for prevalence and incidence
#' @export
#'
read_spu <- function(pjnz_path) {
  spu <- readlines_from_path(pjnz_path, "SPU")

  ## Series are delimitted by "=="
  series_breaks <- which(spu == "==")
  ## 2 "extra" rows per data set
  no_of_years <- series_breaks[2] - series_breaks[1] - 2
  resample_counts <- vnapply(spu[series_breaks[-length(series_breaks)] + 1],
                             get_resample_count)

  ## First block contains median prevalence and incidence, exclude this
  spu_data <- lapply(series_breaks[-1],
                     extract_incidence_prevalence,
                     spt_data = spu,
                     no_of_years = no_of_years)
  ## Repeat each spu data block number of times as specified by its count
  spu_data <- spu_data[rep(seq_along(resample_counts), resample_counts)]

  return(list("incid" = get_incid_prev_results(spu_data, "incid"),
              "prev" = get_incid_prev_results(spu_data, "prev")))
}


#' Pull out the count from a string e.g. "COUNT 1.0"
#'
#' @keywords internal
#'
get_resample_count <- function(count) {
  as.numeric(strsplit(count, " ")[[1]][2])
}

#' Get incidence or prevalence as a matrix from the list of matrices
#' representing the uncertainty results data.
#'
#' @param series_data The extracted series data as a list of matrices, one list
#' entry for each resample.
#' @param property The property to get from the series data. Either "incid" for
#' incidence or "prev" for prevalence.
#'
#' @keywords internal
#'
get_incid_prev_results <- function(results, property) {
  property_data <- lapply(results, function(x) x[, property, drop = FALSE])
  prop <- matrix(unlist(property_data), nrow = nrow(property_data[[1]]))
  rownames(prop) <- row.names(property_data[[1]])
  prop
}

#' Extract incidence and prevalence for single region.
#'
#' Gets the incidence, prevalence and population data (if available) for a
#' single region, e.g. National, Urban or Rural.
#'
#' @param break_index The index of the line break in the spt_data where data
#' about this region ends.
#' @param spt_data The complete spt data read from a file.
#' @param no_of_years The number of years in the data set, equivalent to the
#' number of rows we return.
#'
#' @return A data frame, one row for each year showing the HIV incidence,
#' prevalence and population (if available).
#'
#' @keywords internal
#'
extract_incidence_prevalence <- function(break_index, spt_data, no_of_years) {
  dat <- spt_data[seq(break_index-no_of_years, break_index-1)]
  region_data <- utils::read.table(text = dat, sep = ",", row.names = 1)
  region_data[,1:2] <- region_data[,1:2]/100
  names(region_data) <- c("prev", "incid", "pop")[seq_along(region_data)]
  return(region_data)
}

