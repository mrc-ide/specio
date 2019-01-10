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
#' read_spt(pjnz_path)
#'
read_spt <- function(pjnz_path){
  spt_filename <- get_filename_from_extension("SPT", pjnz_path)
  con <- unz(pjnz_path, spt_filename)
  spt_file <- scan(con, "character", sep="\n")
  close(con)

  ## Data for a particular region starts with "==" delimiter
  region_row_breaks <- which(spt_file == "==")
  ## Incidence and prevalence data ends with "=" delimiter
  incid_prev_row_breaks <- which(spt_file == "=")
  no_of_years <- incid_prev_row_breaks[2] - region_row_breaks[1] - 3
  regions <- stats::na.omit(spt_file[region_row_breaks + 1])
  ## Expect regions to have identifier e.g. Botswana_ 2017_2\Urban:URBAN,NO,50.0
  regions <- stringr::str_match(regions, "\\\\([\\w\\s]+):")[,2]
  ## National region is not labelled, add manually
  regions[is.na(regions)] <- "National"

  ## Ignore first break as the "National" data is repeated
  spt_data <- lapply(incid_prev_row_breaks[-1],
                                     extract_incidence_prevalence,
                                     spt_data = spt_file,
                                     no_of_years = no_of_years)

  names(spt_data) <- regions
  return(spt_data)
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
#' read_epp_data(pjnz_path)
#'
read_epp_data <- function(pjnz_path) {

  workset <- get_eppxml_workset(pjnz_path)
  if(is.null(workset)) {
    return(NULL)
  }

  epp_data <- list()
  attr(epp_data, "country") <- get_property(workset, "worksetCountry")
  attr(epp_data, "country_code") <- get_property(workset, "countryCode")

  ## ANC/HSS input mode appears only defined in second ProjectionSet if
  ## different from first ProjectionSet, so define global version.
  ## Spectrum defaults to "HSS mode", which is no ANC-RT data.
  input_mode <- "HSS"

  projection_sets <- xml2::xml_find_all(
    workset, ".//object[@class='epp2011.core.sets.ProjectionSet']")

  for (projection_set in projection_sets) {
    projset_id <- as.integer(gsub("[^0-9]", "", xml2::xml_attr(projection_set,
                                                               "id")))
    epp_set <- xml2::xml_children(projection_set)
    names(epp_set) <- xml2::xml_attr(epp_set, "property")
    epp_region <- get_property(epp_set, "name")

    ## Only update input mode if specified in the ProjectionSet, otherwise use
    ## the previous value.
    if(length(epp_set[["dataInputMode"]]) && length(
      xml2::xml_find_first(epp_set[["dataInputMode"]], ".//string"))) {
      input_mode <- get_property(epp_set, "dataInputMode")
    }

    anc_data <- get_anc_data(epp_set, input_mode)
    census_data <- get_census_data(epp_set, input_mode)
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
      ancrtcens = census_data,
      hhs = hh_survey_data)
  }
  class(epp_data) <- "eppd"
  return(epp_data)
}

#' Read EPP subpopulation configuration.
#'
#' Reads the subpopulation configuration and population sizes from the EPP .xml
#' file within a PJNZ.
#'
#' EPP projection sets are identified in the .xml file by searching the XML tree
#' for tag "object", and then selecting objects with "class" attribute equal to
#' "epp2011.core.sets.ProjectionSet".
#'
#' @param pjnz_path file path to Spectrum PJNZ file.
#'
#' @export
read_epp_subpops <- function(pjnz_path) {
  workset <- get_eppxml_workset(pjnz_path)
  if(is.null(workset)) {
    return(NULL)
  }

  epp_pops <- list()
  attr(epp_pops, "country") <- get_property(workset, "worksetCountry")
  attr(epp_pops, "country_code") <- get_property(workset, "countryCode")

  epidemic_type <- tolower(get_property(workset, "epidemicType"))
  attr(epp_pops, "epidemicType") <- epidemic_type

  start_year <- get_property(workset, "worksetStartYear")
  end_year <- get_property(workset, "worksetEndYear")
  pop_base_year <- get_property(workset, "worksetPopBaseYear")

  epp_pops$total <-  data.frame(
    year      = start_year:end_year,
    pop15to49 = get_property(workset, "pop15to49"),
    pop15     = get_property(workset, "pop15"),
    pop50     = get_property(workset, "pop50"),
    netmigr   = get_property(workset, "netMigration"))

  epp_pops$subpops <- list()

  projection_sets <- xml2::xml_find_all(
    workset, ".//object[@class='epp2011.core.sets.ProjectionSet']")

  for(projection_set in projection_sets) {
    projset_id <- as.integer(gsub("[^0-9]", "",
                                  xml2::xml_attr(projection_set, "id")))
    epp_set <- xml2::xml_children(projection_set)
    names(epp_set) <- xml2::xml_attr(epp_set, "property")

    epp_region <- get_property(epp_set, "name")

    subpop_data <- data.frame(
      year = start_year:end_year,
      pop15to49 = get_property(epp_set, "pop15to49"),
      pop15     = get_property(epp_set, "pop15"),
      pop50     = get_property(epp_set, "pop50"),
      netmigr   = get_property(epp_set, "netMigration"))
    attr(subpop_data, "projset_id") <- projset_id
    attr(subpop_data, "epidemic.start") <- as.integer(get_property(epp_set,
                                                                   "priorT0vr"))

    if (epidemic_type == "concentrated") {
      subpop <- get_property(epp_set, "specSubPop")
      names(subpop) <- c("low_risk", "msm", "msw", "fsw", "clients",
                         "idu", "prisoners", "transgender", "anc")

      if (length(epp_set[["percentageMale"]])) {
        percent_male <- get_property(epp_set, "percentageMale") / 100
      } else {
        percent_male <- 0.0
      }

      turnover <- as.logical(length(epp_set[["turnedOver"]])) &&
        get_property(epp_set, "turnedOver")

      if(turnover) {
        duration <- get_property(epp_set, "duration")
        assign_id <- xml2::xml_attr(
          xml2::xml_find_first(epp_set[["groupToAssignTo"]], ".//object"), "id")
        assign_id <- as.integer(gsub("[^0-9]", "", assign_id))
        assignment_type <- switch(
          get_property(epp_set, "assignmentMethod"),
          ASSIGN_REPLACE_PREVALENCE = "replace",
          ASSIGN_ADD_PREVALENCE = "add")
      } else {
        duration <- NA
        assign_id <- NA
        assignment_type <- NA
      }

      attr(subpop_data, "subpop") <-  names(which(subpop))
      attr(subpop_data, "percent_male") <- percent_male
      attr(subpop_data, "turnover") <- turnover
      attr(subpop_data, "duration") <- duration
      attr(subpop_data, "assign_id") <- assign_id
      attr(subpop_data, "assignmentType") <- assignment_type
    }

    epp_pops$subpops[[epp_region]] <- subpop_data
  }

  projset_ids <- sapply(epp_pops$subpops, attr, "projset_id")
  assign_ids <- sapply(epp_pops$subpops, attr, "assign_id")
  assign_name <- names(projset_ids)[match(assign_ids, projset_ids)]

  epp_pops$subpops <- Map("attr<-", epp_pops$subpops, "assign_name", assign_name)

  class(epp_pops) <- "eppsubp"

  return(epp_pops)
}

#' Extract and prepare ANC related data from ProjectionSet.
#'
#' Helper function which handles extracting and tidying ANC data from the
#' xml_nodeset representing ProjectionSet java object.
#'
#' @param projection_set An xml_nodeset representing a ProjectionSet java
#' object.
#' @param input_mode
#'
#' @keywords internal
get_anc_data <- function(projection_set, input_mode) {
  anc = list()
  if (exists("siteNames", projection_set)) {
    anc$site_names <- get_property(projection_set, "siteNames")
    anc$used <- get_property(projection_set, "siteSelected")

    anc$prevalence <- get_property(projection_set, "survData")
    anc$prevalence <- tidy_data_for_epp(anc$prevalence,
                                        is_percentage = TRUE,
                                        add_names = TRUE,
                                        site_names = anc$site_names)

    anc$sample_size <-  get_property(projection_set, "survSampleSizes")
    anc$sample_size <- tidy_data_for_epp(anc$sample_size,
                                           add_names = TRUE,
                                           site_names = anc$site_names)

    ## ANC-RT site level
    if(length(projection_set[["PMTCTData"]]) && input_mode == "ANC") {
      anc$rt_prevalence <- get_property(projection_set, "PMTCTData")
      anc$rt_prevalence <- tidy_data_for_epp(anc$rt_prevalence,
                                             is_percentage = TRUE,
                                             add_names = TRUE,
                                             site_names = anc$site_names)

      anc$rt_sample_size <- get_property(projection_set, "PMTCTSiteSampleSizes")
      anc$rt_sample_size <- tidy_data_for_epp(anc$rt_sample_size,
                                              add_names = TRUE,
                                              site_names = anc$site_names)
    }
  }
  return(anc)
}

#' Extract and prepare census data from ProjectionSet.
#'
#' Helper function which handles extracting and tidying census data from the
#' xml_nodeset representing ProjectionSet java object.
#'
#' @param projection_set An xml_nodeset representing a ProjectionSet java
#' object.
#'
#' @keywords internal
get_census_data <- function(projection_set, input_mode) {
  if (length(projection_set[["censusPMTCTSurvData"]]) && input_mode == "ANC") {
    census_prevalence <- get_property(projection_set, "censusPMTCTSurvData")
    census_prevalence <- tidy_data_for_epp(census_prevalence, TRUE, TRUE)

    census_sample_size <- get_property(projection_set, "censusPMTCTSampleSizes")
    census_sample_size <- tidy_data_for_epp(census_sample_size)

    census_data <- data.frame(year = as.integer(names(census_prevalence)),
                              prev = census_prevalence,
                              n = census_sample_size)
    census_data <- census_data[which(
      !is.na(census_data$prev) | !is.na(census_data$n)), ]
  }
}

#' Tidy data for EPP.
#'
#' Helper to tidy data and add labels for use in EPP.
#'
#' This will convert any '-1' in the data to an NA as the Java representation
#' of the data uses -1 to indicate no data.
#' If is_percentage is true it will divide the data by 100 as Java
#' representation of data is in percentage but EPP needs it as a probability.
#' If add_names is true it will add labels to the matrix or vector. If a vector
#' then it will use years for the names. If a matrix then it will add year
#' labels to the columns and site labels specified by site_names to the rows.
#'
#' @param dat Data to be tidied.
#' @param is_percentage True if data represents a percentage.
#' @param add_names True if names should be added to the data.
#' @param site_names List of site names to be used for the data.
#'
#' @return The tidied and labelled data.
#'
#' @keywords internal
tidy_data_for_epp <- function(dat, is_percentage = FALSE, add_names = FALSE,
                              site_names = NA) {
  dat[dat == -1] <- NA
  if (is_percentage & is.numeric(dat)) {
    dat <- dat/100
  }
  if (add_names) {
    if (length(site_names) & is.matrix(dat)) {
      dimnames(dat) <- list(site = site_names, year = 1985+0:(ncol(dat)-1))
    } else if (is.vector(dat)) {
      names(dat) <- 1985+0:(length(dat)-1)
    }
  }
  return(dat)
}

#' Extract and prepare HH survey data from ProjectionSet.
#'
#' Helper function which handles extracting and tidying HH survey data from
#' the xml_nodeset representing ProjectionSet java object.
#'
#' @param projection_set An xml_nodeset representing a ProjectionSet java
#' object.
#'
#' @keywords internal
get_hh_survey_data <- function(projection_set) {
  hh_survey <- data.frame()
  if ("surveyYears" %in% names(projection_set)) {
    hh_survey <- data.frame(
      year = get_property(projection_set, "surveyYears"),
      prev = get_property(projection_set, "surveyHIV") / 100,
      se = get_property(projection_set, "surveyStandardError") / 100,
      n = NA,
      used = get_property(projection_set, "surveyIsUsed")
    )

    if ("inputInc" %in% names(projection_set)) {
      hh_survey$incid <- get_property(projection_set, "inputInc") / 100
      hh_survey$incid_se <- get_property(projection_set, "inputIncSE") / 100
      hh_survey$prev_incid_corr <- get_property(projection_set,
                                                "inputIncPrevCorr")
      hh_survey$incid_cohort <- get_property(projection_set, "incIsCohort")
    } else {
      hh_survey$incid <- -1
      hh_survey$incid_se <- NA
      hh_survey$prev_incid_corr <- NA
      hh_survey$incid_cohort <- NA
    }

    hh_survey <- hh_survey[which(hh_survey$prev > 0 | hh_survey$used |
                                    hh_survey$se != 0.01), ]

    ## TODO Why?
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

## Consider refactoring when we have test case which covers this
## Used for Spectrum EPP data structure for HH survey data impl in 2019 version
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
