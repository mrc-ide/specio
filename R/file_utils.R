#' Get filename from extension.
#'
#' Helper method to get the full filename within a zip matching extension. This
#' expects there to be only one file in the zip with the extension.
#'
#' @param file_extension The file extension to look for.
#' @param path_to_zip The path to the zip file to look in.
#'
#' @return The name of the located file.
#'
#' @keywords internal
#'
get_filename_from_extension <- function(file_extension, path_to_zip) {
  filename <- grep(paste0("\\.", file_extension, "$"),
                   utils::unzip(path_to_zip, list = TRUE)$Name,
                   value = TRUE
  )
  if (length(filename) != 1) {
    stop(sprintf(
      "Only one file of type %s must exist at path %s, found %d.",
      file_extension, path_to_zip, length(filename)
    ))
  }
  return(filename)
}

#' Get EPP workset from xml file.
#'
#' Gets all properties of the EPP workset from xml file inside zip at specified
#' path.
#'
#' @param pjnz_path Path to zip containing workset xml.
#'
#' @return The workset as a list.
#'
get_eppxml_workset <- function(pjnz_path) {
  xmlfile <- get_filename_from_extension("xml", pjnz_path)
  con <- unz(pjnz_path, xmlfile)
  epp_xml <- xml2::read_xml(con)
  properties <- xml2::xml_children(xml2::xml_children(epp_xml))
  names(properties) <- xml2::xml_attr(properties, "property")
  stopifnot(length(properties) > 0)
  return(properties)
}

readlines_from_path <- function(pjnz_path, extension) {
  file <- get_filename_from_extension(extension, pjnz_path)
  con <- unz(pjnz_path, file)
  on.exit(close(con))
  lines <- readLines(con)
}

#' Get csv data from file containing within PJNZ.
#'
#' @param pjnz_path Path to PJNZ zip file.
#' @param extension The file extension to look for.
#'
#' @return The data from file read as a csv.
#'
get_pjnz_csv_data <- function(pjnz_path, extension) {
  file <- get_filename_from_extension(extension, pjnz_path)
  csv <- utils::read.csv(unz(pjnz_path, file), as.is = TRUE)
}
