#' Load best fitting HIV incidence and prevalence data from SPT file.
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
#' read_bf_incidence_and_prevalence(pjnz_path)
#'
read_bf_incidence_and_prevalence <- function(pjnz_path){
  spt_filename <- get_filename_from_extension("SPT", pjnz_path)
  con <- unz(pjnz_path, spt_filename)
  spt <- scan(con, "character", sep="\n")
  close(con)

  # Data for a particular region starts with "==" delimiter
  region_row_breaks <- which(spt == "==")
  # Incidence and prevalence data ends with "=" delimiter
  incid_prev_row_breaks <- which(spt == "=")
  no_of_years <- incid_prev_row_breaks[2] - region_row_breaks[1] - 3
  regions <- stats::na.omit(spt[region_row_breaks + 1])
  # Expect regions to have identifier e.g. Botswana_ 2017_2\Urban:URBAN,NO,50.0
  regions <- stringr::str_match(regions, "\\\\([\\w\\s]+):")[,2]
  # National region is not labelled, add manually
  regions[is.na(regions)] <- "National"

  # Ignore first break as the "National" data is repeated
  incidence_and_prevalence <- lapply(incid_prev_row_breaks[-1],
                                     extract_incidence_prevalence,
                                     spt_data = spt,
                                     no_of_years = no_of_years)

  names(incidence_and_prevalence) <- regions
  return(incidence_and_prevalence)
}

#' Read EPP fitting surveillance data
#'
#' Reads the HIV prevalence from sentinel surveillance and household
#' surveillance data from the EPP .xml file within a PJNZ.
#'
#' EPP projection sets are identified in the .xml file by searching the XML tree
#' for tag "object", and then selecting objects with "class" attribute equal to
#' "epp2011.core.sets.ProjectionSet".
#'
#' @param pjnz_path Path to Spectrum PJNZ file.
#' @return List of data frames containing prevalence and incidence data.
#'
#' @export
#'
#' @examples
#' pjnz_path <- system.file("testdata", "Botswana2018.PJNZ", package="specio")
#' read_epp_prevalence(pjnz_path)
#'
read_epp_prevalence <- function(pjnz_path) {

  properties <- get_eppxml_workset(pjnz_path)
  if(is.null(properties)) {
    return(NULL)
  }

  epp_data <- list()
  attr(epp_data, "country") <- xml2::xml_text(properties[["worksetCountry"]])
  attr(epp_data, "country_code") <- xml2::xml_integer(
    properties[["countryCode"]])

  # ANC/HSS input mode appears only defined in second set if updated, so
  # define global version. Defaults to "HSS mode", which is no ANC-RT data.
  input_mode <- "HSS"

  projection_sets <- xml2::xml_find_all(
    properties, "//object[@class='epp2011.core.sets.ProjectionSet']")

  for (projection_set in projection_sets) {
    projset_id <- as.integer(gsub("[^0-9]", "", xml2::xml_attr(projection_set,
                                                               "id")))
    epp_set <- xml2::xml_children(projection_set)
    names(epp_set) <- xml2::xml_attr(epp_set, "property")
    epp_region <- xml2::xml_text(epp_set[["name"]])

    if(length(epp_set[["dataInputMode"]]) && length(
      xml2::xml_find_first(epp_set[["dataInputMode"]], ".//string"))) {
      input_mode <- xml2::xml_text(
        xml2::xml_find_first(epp_set[["dataInputMode"]], ".//string"))
    }

    anc_data <- get_anc_data(epp_set, input_mode)

    hh_survey_data <- get_hh_survey_data(epp_set)

    epp_data[[epp_region]] <- list(
      country = attr(epp_data, "country"),
      region = epp_region,
      projset_id = projset_id,
      anc.used = anc_data$used,
      anc.prev = anc_data$prevalence,
      anc.n = anc_data$sample_size,
      ancrtsite.prev = anc_data$rt_prevalence,
      ancrtsite.n = anc_data$rt_sample_size,
      ancrtcens = anc_data$rt_cens,
      hhs = hh_survey_data)
  }
  class(epp_data) <- "eppd"
  return(epp_data)
}

#' Extract ANC related data from ProjectionSet.
#'
#' Helper function which handles extracting ANC data from the xml_nodeset
#' representing ProjectionSet java object.
#'
#' @param projection_set An xml_nodeset representing a ProjectionSet java
#' object.
#' @param input_mode
#'
#' @keywords internal
get_anc_data <- function(projection_set, input_mode) {
  anc = list()
  if (exists("siteNames", projection_set)) {
    anc$site_names <- get_array_property_data(projection_set, "siteNames")
    anc$used <- get_array_property_data(projection_set, "siteSelected")

    anc$prevalence <- get_array_property_data(projection_set,
                                              "survData",
                                              anc$site_names)
    # TODO Why?
    anc$prevalence <- anc$prevalence / 100
    anc$sample_size <-  get_array_property_data(projection_set,
                                                "survSampleSizes",
                                                 anc$site_names)

    # ANC-RT site level
    if(length(projection_set[["PMTCTData"]]) && input_mode == "ANC") {
      anc$rt_prevalence <- get_array_property_data(projection_set,
                                                   "PMTCTData",
                                                   anc$site_names)
      anc$rt_prevalence <- anc$rt_prevalence / 100
      anc$rt_sample_size <- get_array_property_data(
        projection_set,
        "PMTCTSiteSampleSizes",
        anc$site_names)
    }

  }

  if (length(projection_set[["censusPMTCTSurvData"]]) && input_mode == "ANC") {
    census_prevalence <- get_array_property_data(projection_set,
                                                 "censusPMTCTSurvData")
    names(census_prevalence) <- 1985+0:(length(census_prevalence)-1)
    census_prevalence[census_prevalence == -1] <- NA
    census_prevalence <- census_prevalence / 100

    census_sample_size <- get_array_property_data(
      projection_set, "censusPMTCTSampleSizes")
    census_sample_size[census_sample_size == -1] <- NA

    anc$rt_cens <- data.frame(year = as.integer(names(census_prevalence)),
                              prev = census_prevalence,
                              n = census_sample_size)
    anc$rt_cens <- anc$rt_cens[which(
      !is.na(anc$rt_cens$prev) | !is.na(anc$rt_cens$n)), ]
  }
  return(anc)
}

#' Extract HH survey data from ProjectionSet.
#'
#' Helper function which handles extracting ANC data from the xml_nodeset
#' representing ProjectionSet java object.
#'
#' @param projection_set An xml_nodeset representing a ProjectionSet java
#' object.
#'
#' @keywords internal
get_hh_survey_data <- function(projection_set) {
  hh_survey <- data.frame()
  if ("surveyYears" %in% names(projection_set)) {
    hh_survey <- data.frame(
      year = get_array_property_data(projection_set, "surveyYears"),
      prev = get_array_property_data(projection_set, "surveyHIV") / 100,
      se = get_array_property_data(projection_set, "surveyStandardError") / 100,
      n = NA,
      used = get_array_property_data(projection_set, "surveyIsUsed")
    )

    if ("inputInc" %in% names(projection_set)) {
      hh_survey$incid <- get_array_property_data(projection_set,
                                                 "inputInc") / 100
      hh_survey$incid_se <- get_array_property_data(projection_set,
                                                    "inputIncSE") / 100
      hh_survey$prev_incid_corr <- get_array_property_data(projection_set,
                                                  "inputIncPrevCorr")
      hh_survey$incid_cohort <- get_array_property_data(projection_set,
                                                        "incIsCohort")
    } else {
      hh_survey$incid <- -1
      hh_survey$incid_se <- NA
      hh_survey$prev_incid_corr <- NA
      hh_survey$incid_cohort <- NA
    }

    hh_survey <- hh_survey[which(hh_survey$prev > 0 | hh_survey$used |
                                    hh_survey$se != 0.01), ]

    # TODO Why?
    if (nrow(hh_survey)) {
      hh_survey[hh_survey$incid < 0, c("incid",
                                       "incid_se",
                                       "prev_incid_corr",
                                       "incid_cohort")] <- NA
    }

  } else {
    surveys <- xml2::xml_children(xml2::xml_children(
      projection_set[["surveyData"]]))
    hh_survey <- lapply(surveys, parse_survey)
    hh_survey <- as.data.frame(do.call(rbind, hh_survey))
    hh_survey <- as.data.frame(lapply(hh_survey, utils::type.convert))

    hh_survey$prev <- hh_survey$prev / 100
    hh_survey$se <- hh_survey$se / 100
    hh_survey$incid <- hh_survey$incid / 100
    hh_survey$incid_se <- hh_survey$incid_se / 100
  }
  return(hh_survey)
}

# Consider refactoring when we have test case which covers this
parse_survey <- function(survey) {
  ns <- xml2::xml_children(xml2::xml_child(survey))
  attrs <- lapply(ns, xml2::xml_attrs)

  val <- lapply(
   lapply(ns[sapply(attrs,  "%in%", x = "getField")], xml2::xml_children),
   xml2::xml_text)

  v2 <- lapply(ns[!sapply(attrs,  "%in%", x = "getField")], xml2::xml_children)
  v2 <- lapply(v2, lapply, xml2::xml_children)
  v2 <- lapply(v2, lapply, xml2::xml_text)

  val <- c(val, unlist(v2, FALSE))
  val <- stats::setNames(sapply(val, "[", 2), sapply(val, "[", 1))

  cols <- c("name" = "name",
            "year" = "year",
            "used",
            "n",
            "surveyHIV" = "prev",
            "surveyStandardError" = "se",
            "incidence" = "incid",
            "standardError" = "incid_se",
            "prev_incid_corr",
            "incidence_cohort",
            "usingIncidenceData" = "incid_used")

  names(val) <- cols[names(val)]
  val[setdiff(cols, names(val))] <- NA
  val["used"] <- TRUE
  val[cols]
}

#' Get data for a property from the xml_nodeset.
#'
#' Will get array data from one xml_node from an xml_nodeset. If node
#' represents a vector then a vector is returned. If node repsents a matrix then
#' parse data and return matrix.
#'
#' @param nodeset The xml nodeset containing the property.
#' @param property The property to get data for.
#' @param site_names Optional list of site names to use for matrix row names.
#'
#' @return Parsed property data.
#' @keywords internal
#'
get_array_property_data <- function(nodeset, property, site_names = NULL) {
  property_data <- xml2::xml_find_first(nodeset[[property]], "array")
  parsed_data <- NA
  if(xml2::xml_attr(property_data, "class") %in% c("[D", "[I")) {
    parsed_data <- parse_matrix(property_data)
    dimnames(parsed_data) <- list(site = site_names,
                                  year = 1985+0:(ncol(parsed_data)-1))
    # TODO: Why?
    parsed_data[parsed_data == -1] <- NA
  } else {
    parsed_data <- parse_array(property_data)
  }
  return(parsed_data)
}


#' Parse xml node containing array to vector.
#'
#' Parses XML representation of java array from EPP .xml file.
#'
#' @param xml_node `xml_node` object representing an array.
#' @return A vector representing the array.
#'
#' @keywords internal
#'
parse_array <- function(xml_node) {
  convert_element <- switch(xml2::xml_attr(xml_node, "class"),
                            int = xml2::xml_integer,
                            double = xml2::xml_double,
                            boolean =
                              function(x) as.logical(xml2::xml_text(x)),
                            java.lang.String = xml2::xml_text)
  arr <- convert_element(xml2::xml_children(xml_node))
  # Array may contain fewer elements than its length property specifies. Ensure
  # return vector has the correct length. i.e. the length specified by the
  # property
  if (length(arr) < as.integer(xml2::xml_attr(xml_node, "length"))) {
    length(arr) <- as.integer(xml2::xml_attr(xml_node, "length"))
  }
  return(arr)
}

#' Parse xml node representing matrix to an R matrix.
#'
#' Parses XML representation of java array of arrays from EPP .xml file.
#'
#' @param xml_node `xml_node` object representing a matrix.
#' @return Matrix
#'
#' @keywords internal
#'
parse_matrix <- function(xml_node) {
  if(!xml2::xml_attr(xml_node, "class") %in% c("[D", "[I")) {
    stop("Can't parse matrix on node not of class '[D' or '[I'.")
  }
  rows <- xml2::xml_children(xml_node)
  idx <- as.integer(xml2::xml_attr(rows, "index")) + 1L
  parsed_matrix <- sapply(xml2::xml_find_first(rows, "array"), parse_array)
  t(parsed_matrix)
}

#' Get EPP workset from xml file.
#'
#' Gets all properties of the EPP workset from xml file inside zip at specified
#' path.
#'
#' @param pjnz_path Path to zip containing workset xml.
#'
#' @return The workset as a list.
#' @export
#'
get_eppxml_workset <- function(pjnz_path) {
  xmlfile <- get_filename_from_extension("xml", pjnz_path)
  con <- unz(pjnz_path, xmlfile)
  epp_xml <- xml2::read_xml(con)
  properties <- xml2::xml_children(xml2::xml_children(epp_xml))
  names(properties) <- xml2::xml_attr(properties, "property")
  return(properties)
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
  dat <- spt_data[(break_index-no_of_years):(break_index-1)]
  region_data <-
    data.frame(t(sapply(strsplit(dat, ","), as.numeric)), row.names=1)
  region_data[,1:2] <- region_data[,1:2]/100
  names(region_data) <- c("prev", "incid", "pop")[1:ncol(region_data)]
  return(region_data)
}

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
                   utils::unzip(path_to_zip, list=TRUE)$Name,
                   value=TRUE)
  if (length(filename) != 1) {
    stop(sprintf("Only one file of type %s must exist at path %s, found %d.",
                 file_extension, path_to_zip, length(filename)))
  }
  return(filename)
}
