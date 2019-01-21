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

  epp_data <- list()
  attr(epp_data, "country") <- get_property(workset, "worksetCountry")
  attr(epp_data, "country_code") <- get_property(workset, "countryCode")

  ## ANC/HSS input mode appears only defined in second ProjectionSet if
  ## different from first ProjectionSet, so define global version.
  ## Spectrum defaults to "HSS mode", which is no ANC-RT data.
  input_mode <- "HSS"

  region_nodeset <- xml2::xml_find_all(
    workset, ".//object[@class='epp2011.core.sets.ProjectionSet']")

  for (region_node in region_nodeset) {
    projset_id <- as.integer(gsub("[^0-9]", "",
                                  xml2::xml_attr(region_node, "id")))
    region_properties <- xml2::xml_children(region_node)
    names(region_properties) <- xml2::xml_attr(region_properties, "property")
    epp_region <- get_property(region_properties, "name")

    ## Only update input mode if specified in the ProjectionSet, otherwise use
    ## the previous value.
    input_mode_changed <- length(region_properties[["dataInputMode"]]) &&
      length(xml2::xml_find_first(region_properties[["dataInputMode"]],
                                  ".//string"))
    if (input_mode_changed) {
      input_mode <- get_property(region_properties, "dataInputMode")
    }

    anc_data <- get_anc_data(region_properties, input_mode)
    census_data <- get_census_data(region_properties, input_mode)
    hh_survey_data <- get_hh_survey_data(region_properties)

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

  region_datasets <- xml2::xml_find_all(
    workset, ".//object[@class='epp2011.core.sets.ProjectionSet']")

  for(region_data in region_datasets) {
    projset_id <- as.integer(gsub("[^0-9]", "",
                                  xml2::xml_attr(region_data, "id")))
    region_properties <- xml2::xml_children(region_data)
    names(region_properties) <- xml2::xml_attr(region_properties, "property")

    epp_region <- get_property(region_properties, "name")

    subpop_data <- data.frame(
      year = start_year:end_year,
      pop15to49 = get_property(region_properties, "pop15to49"),
      pop15     = get_property(region_properties, "pop15"),
      pop50     = get_property(region_properties, "pop50"),
      netmigr   = get_property(region_properties, "netMigration"))
    attr(subpop_data, "projset_id") <- projset_id
    attr(subpop_data, "epidemic.start") <- as.integer(
      get_property(region_properties, "priorT0vr"))

    if (epidemic_type == "concentrated") {
      concentrated_data <- get_concentrated_epidemic_data(region_properties)
      attr(subpop_data, "subpop") <-  names(which(concentrated_data$subpop))
      attr(subpop_data, "percent_male") <- concentrated_data$percent_male
      attr(subpop_data, "turnover") <- concentrated_data$turnover
      attr(subpop_data, "duration") <- concentrated_data$duration
      attr(subpop_data, "assign_id") <- concentrated_data$assign_id
      attr(subpop_data, "assignmentType") <- concentrated_data$assignment_type
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

#' Get concentrated epidemic data from EPP data set.
#'
#' Gets all data related to a concentrated epidemic from EPP nodeset.
#'
#' @param epp_set xmlnodeset containing concetrated epidemic data.
#'
#' @return List containing concentrated epidemic data.
#'
#' @keywords internal
get_concentrated_epidemic_data <- function(epp_set) {
  epidemic_data <- list()
  epidemic_data$subpop <- get_property(epp_set, "specSubPop")
  names(epidemic_data$subpop) <- c("low_risk", "msm", "msw", "fsw", "clients",
                     "idu", "prisoners", "transgender", "anc")

  if (length(epp_set[["percentageMale"]])) {
    epidemic_data$percent_male <- get_property(epp_set, "percentageMale") / 100
  } else {
    epidemic_data$percent_male <- 0.0
  }

  epidemic_data$turnover <- as.logical(length(epp_set[["turnedOver"]])) &&
    get_property(epp_set, "turnedOver")

  if(epidemic_data$turnover) {
    epidemic_data$duration <- get_property(epp_set, "duration")
    epidemic_data$assign_id <- xml2::xml_attr(
      xml2::xml_find_first(epp_set[["groupToAssignTo"]], ".//object"), "id")
    epidemic_data$assign_id <- as.integer(gsub("[^0-9]", "",
                                               epidemic_data$assign_id))
    epidemic_data$assignment_type <- switch(
      get_property(epp_set, "assignmentMethod"),
      ASSIGN_REPLACE_PREVALENCE = "replace",
      ASSIGN_ADD_PREVALENCE = "add")
  } else {
    epidemic_data$duration <- NA
    epidemic_data$assign_id <- NA
    epidemic_data$assignment_type <- NA
  }
  return(epidemic_data)
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
  if (length(projection_set[["censusPMTCTSurvData"]]) == 0 || input_mode != "ANC") {
    return(NULL)
  }
  census_prevalence <- get_property(projection_set, "censusPMTCTSurvData")
  census_prevalence <- tidy_data_for_epp(census_prevalence, TRUE, TRUE)

  census_sample_size <- get_property(projection_set, "censusPMTCTSampleSizes")
  census_sample_size <- tidy_data_for_epp(census_sample_size)

  census_data <- data.frame(year = as.integer(names(census_prevalence)),
                            prev = census_prevalence,
                            n = census_sample_size)
  census_data <- census_data[which(
    !is.na(census_data$prev) | !is.na(census_data$n)), ]
  census_data
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
                              site_names = NULL) {
  dat[dat == -1] <- NA
  if (is_percentage && is.numeric(dat)) {
    dat <- dat/100
  }
  if (add_names) {
    ## Tidy years
    if (!is.null(site_names) && is.matrix(dat)) {
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
    ## HH survey data stored in new structure for 2019, read data from updated
    ## structure.
    surveys <- xml2::xml_children(xml2::xml_children(
      projection_set[["surveyData"]]))
    survey_data <- lapply(surveys, parse_survey)
    hh_survey <- data.frame(do.call(rbind.data.frame, survey_data))
    v_percent <- c("prev", "se", "incid", "incid_se")
    hh_survey[v_percent] <- hh_survey[v_percent] / 100
    rownames(hh_survey) <- NULL
  }
  hh_survey
}

#' Parse survey data into expected return format.
#'
#' Parses survey data as stored in 2019 version of spectrum.
#'
#' @param nodeset for a survey
#'
#' @return parsed survey data
#'
#' @keywords internal
parse_survey <- function(survey) {
  fields <- get_fields(survey)
  return_names <- c("year" = "year",
                    "surveyHIV" = "prev",
                    "surveyStandardError" = "se",
                    "n" = "n",
                    "used" = "used",
                    "incidence" = "incid",
                    "standardError" = "incid_se",
                    "prev_incid_corr" = "prev_incid_corr",
                    "incidence_cohort" = "incid_cohort")
  return_values <- data.frame(
    year = NA_integer_,
    prev = NA_real_,
    se = NA_real_,
    n = NA,
    used = TRUE,
    incid = NA_real_,
    incid_se = NA_real_,
    prev_incid_corr = NA,
    incid_cohort = NA)
  names(fields) <- return_names[match(names(fields), names(return_names))]
  survey_data <- lapply(names(return_values),
                   function(x) fields[[x]][[1]] %||% return_values[[x]])
  names(survey_data) <- names(return_values)
  survey_data
}

