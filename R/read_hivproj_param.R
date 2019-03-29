read_hivproj_param <- function(pjnz_path) {
  dp_data <- get_dp_data(pjnz_path)
  pjn_data <- get_pjn_data(pjnz_path)

  pjn_metadata <- get_pjn_metadata(pjn_data)

  version <- get_property_data("version", dp_data)
  valid_date <- get_property_data("valid_date", dp_data)
  valid_version <- get_property_data("valid_version", dp_data)

  ## Projection parameters
  yr_start <- get_property_data("yr_start", dp_data)
  yr_end <- get_property_data("yr_end", dp_data)
  proj_years <- seq.int(yr_start, yr_end)

  ## Demographic inputs
  total_population <- get_property_data("total_population", dp_data, proj_years)
  sx <- get_property_data("survey_rate", dp_data, proj_years)
  fertility_rate <- get_property_data("fertility_rate", dp_data, proj_years)
  births_proportion <- get_property_data("asfd", dp_data, proj_years)
  age_specific_fertility_rate <-
    calc_age_specific_fertility_rate(fertility_rate, births_proportion)
  sex_ratio_at_birth <-
    get_property_data("sex_ratio_at_birth", dp_data, proj_years)
  births <- get_property_data("births", dp_data, proj_years)
  total_net_migr <- get_property_data("total_net_migr", dp_data, proj_years)
  net_migr_age_dist <-
    get_property_data("net_migr_age_dist", dp_data, proj_years)
  net_migration <- calc_net_migration(total_net_migr, net_migr_age_dist)

  ## Epidemic inputs
  hiv_pop <- get_property_data("hiv_pop", dp_data, proj_years)
  new_infections <- get_property_data("new_infections", dp_data, proj_years)
  art_pop <- get_property_data("art_pop", dp_data, proj_years)

  ## AIM parameters
  adult_infec_reduc <- 1.0 - get_property_data("adult_infec_reduc", dp_data)
  incid_pop_ages <- get_property_data("incid_pop_ages", dp_data)
  hiv_sex_ratio <- get_property_data("hiv_sex_ratio", dp_data, proj_years)
  dist_of_hiv <- get_property_data("dist_of_hiv", dp_data, proj_years)

  aim_params <- get_impact_model_params(dp_data, proj_years)

  ## Natural history

  new_infections_cd4 <- get_property_data("new_infections_cd4", dp_data)
  progress_cd4 <- get_property_data("progress_cd4", dp_data)
  mortality_cd4 <- get_property_data("mortality_cd4", dp_data)
  art_mortality_cd4 <- get_art_mortality(dp_data)
  art_mortality_rates <- get_property_data("art_mortality_rates",
                                           dp_data, proj_years)

  ## ART programme data

  art_15plus_num_percent <- get_property_data("art_15plus_num_percent", dp_data,
                                              proj_years)
  art_15plus_num <- get_property_data("art_15plus_num", dp_data, proj_years)
  art_15plus_need_art <- get_property_data("art_15plus_need_art", dp_data,
                                           proj_years)
  art_15plus_eligibility_threshold <-
    get_property_data("art_15plus_eligibility_threshold", dp_data, proj_years)

  art_eligibility_pop <- get_property_data("art_eligibility_pop", dp_data,
                                           proj_years)
  median_cd4_init <- get_property_data("median_cd4_init", dp_data, proj_years)
  art_dropout <- get_property_data("art_dropout", dp_data, proj_years)
  art_allocation_method <- get_property_data("art_allocation_method", dp_data,
                                             proj_years)
  art_prop_alloc <- get_property_data("art_prop_alloc", dp_data, c("mx", "elig"))
  scale_cd4_mortality <- get_scale_cd4_mortality(valid_version)
  age_14_hiv_population <- get_property_data("age_14_hiv_population", dp_data,
                                             proj_years)

  output <- list(
    country = pjn_metadata$country,
    iso3 = pjn_metadata$iso3,
    region = pjn_metadata$region,
    projection_name = pjn_metadata$projection_name,
    spectrum_version = pjn_metadata$spectrum_version,
    valid_date = valid_date,
    yr_start = yr_start,
    yr_end = yr_end,
    totpop = total_population,
    Sx = sx,
    asfr = age_specific_fertility_rate,
    srb = sex_ratio_at_birth,
    births = births,
    netmigr = net_migration,
    hivpop = hiv_pop,
    infections = new_infections,
    artpop = art_pop,
    relinfectART = adult_infec_reduc,
    incidpopage = incid_pop_ages,
    incrr_sex = hiv_sex_ratio,
    incrr_age = dist_of_hiv,
    fert_rat = aim_params$fert_rat,
    cd4fert_rat = aim_params$cd4fert_rat,
    frr_art6mos = aim_params$frr_art6mos,
    frr_scalar = aim_params$frr_scalar,
    cd4_initdist = new_infections_cd4,
    cd4_prog = progress_cd4,
    cd4_mort = mortality_cd4,
    art_mort = art_mortality_cd4,
    artmx_timerr = art_mortality_rates,
    art15plus_numperc = art_15plus_num_percent,
    art15plus_num = art_15plus_num,
    art15plus_needart = art_15plus_need_art,
    art15plus_eligthresh = art_15plus_eligibility_threshold,
    artelig_specpop = art_eligibility_pop,
    median_cd4init = median_cd4_init,
    art_dropout = art_dropout,
    art_alloc_method = art_allocation_method,
    art_prop_alloc = art_prop_alloc,
    scale_cd4_mort = scale_cd4_mortality,
    age14hivpop = age_14_hiv_population
  )
}

#' Extract AIM module parameters.
#'
#' @param dp_data The full set of dp data to extract data from.
#' @param proj_years Active years for the projection.
#'
#' @return List of impact model params.
#'
#' @keywords internal
get_impact_model_params <- function(dp_data, proj_years) {
  fertility_ratio <- get_property_data("fertility_ratio", dp_data, proj_years)
  cd4_fertility_ratio <- get_property_data("cd4_fertility_ratio", dp_data)
  women_on_art <- get_property_data("women_on_art", dp_data,
                                    get_agegr_labels(between_15_49_only = TRUE))
  frr_scalar <- get_property_data("frr_scalar", dp_data)

  output <- list(
    fert_rat = fertility_ratio,
    cd4fert_rat = cd4_fertility_ratio,
    frr_art6mos = women_on_art,
    frr_scalar = frr_scalar
  )
}

#' Retrieve and tidy ART mortality data.
#'
#' @param dp_data The full set of dp data to extract data from.
#'
#' @keywords internal
get_art_mortality <- function(dp_data) {
  mortality_0to6 <- get_property_data("mortality_by_art_cd4_0to6", dp_data)
  mortality_7to12 <- get_property_data("mortality_by_art_cd4_7to12", dp_data)
  mortality_gt12 <- get_property_data("mortality_by_art_cd4_gt12", dp_data)

  art_mort <- array(NA, c(cfg$params$TS, cfg$params$DS, 4, cfg$params$NG),
                    list(artdur = c("ART0MOS", "ART6MOS", "ART1YR"),
                         cd4stage = seq_len(cfg$params$DS),
                         agecat = c("15-24", "25-34", "35-44", "45+"),
                         sex = c("male", "female")))
  art_mort[1, , , "male"] <- mortality_0to6[, , "male"]
  art_mort[1, , , "female"] <- mortality_0to6[, , "female"]
  art_mort[2, , , "male"] <- mortality_7to12[, , "male"]
  art_mort[2, , , "female"] <- mortality_7to12[, , "female"]
  art_mort[3, , , "male"] <- mortality_gt12[, , "male"]
  art_mort[3, , , "female"] <- mortality_gt12[, , "female"]
  art_mort
}

#' Interpret CD4 mortality scale from version number.
#'
#' @param valid_version The version number.
#'
#' @keywords internal
get_scale_cd4_mortality <- function(valid_version) {
  version <- valid_version
  beta_version <- NULL

  if (is.character(valid_version)) {
    version <- as.numeric(sub("^([0-9\\.]+).*", "\\1", valid_version))
    if (grepl("Beta", valid_version)) {
      beta_version <- as.numeric(sub(".*Beta ([0-9]+)$", "\\1", valid_version))
    }
  }

  if (version >= 5.73 && (beta_version >= 15 || is.null(beta_version))) {
    scale_cd4_mortality <- 1L
  } else {
    scale_cd4_mortality <- 0L
  }
  scale_cd4_mortality
}

#' Get data for a property from full dp_data via its tag.
#'
#' Gets the list of tags for a particular property, identifies which one
#' should be used and then invokes the configured function for that tag.
#'
#' @param property The property to get data for.
#' @param dp_data The full dp dataset.
#'
#' @return The parsed data
#' @keywords internal
#'
get_property_data <- function(property, dp_data, ...) {
  tags <- get_property_tags(property)
  tag <- get_tag(tags, dp_data)
  if (tag == "fallback") {
    metadata <- tags[[tag]][-which((names(tags[[tag]]) == "func"))]
    return(tags[[tag]]$func(metadata, ...))
  } else {
    tag_data <- get_raw_tag_data(tag, dp_data)
    return(tags[[tag]]$func(tag, tag_data, tags[[tag]], ...))
  }
}

#' Calculate ASFR from TFR and fertility distribution.
#'
#' Splits each 5 year age group into 5, distributing proportion of births (asfd)
#' evenly between the single ages. Then multiply each age group by the
#' corresponding fertility rate for that that age group and year.
#'
#' @param fertility_rate vector of annual fertility rate values.
#' @param births_proportion array of proportion of births by 5 year age group
#' 15-49.
#'
#' @return array of age-specific fertility rate by single-year of age 15-49.
#'
#' @keywords internal
calc_age_specific_fertility_rate <- function(fertility_rate, births_proportion){
  asfr <- apply(births_proportion / 5, 2, rep, each = 5)
  asfr <- sweep(asfr, 2, fertility_rate, "*")
  dimnames(asfr) <- list(age = 15:49,
                         year = dimnames(births_proportion)[["year"]])
  dim(asfr) <- lengths(dimnames(asfr))
  asfr
}


#' Calculate net migration for single years using beers coefficients.
#'
#' This uses beers coefficients to calculate net migration for a single age
#' from data aggregated into 5 year age groups.
#'
#' @param total_net_migr The total net migration by 5 year age groups.
#' @param net_migr_age_dist The net migration age distribution.
#'
#' @return Single year migration data.
#'
#' @keywords internal
calc_net_migration <- function(total_net_migr, net_migr_age_dist){

  net_migr <- sweep(net_migr_age_dist, 2:3, total_net_migr, "*")
  ## Disaggregate 5 year age groups into singe-year age groups.
  ## Except for last age group which represents 80+ range.
  net_migr_by_age <- apply(net_migr[-dim(net_migr)["agegr"], , ], 2:3,
                           beers::beers_sub_ordinary)

  ## Add 80+ age group back
  migr_data <- list()
  for (year in dimnames(net_migr_by_age)[["year"]]) {
    migr_data[[year]] <- rbind(net_migr_by_age[, , year],
                               net_migr[dim(net_migr)["agegr"], , year])
  }

  dn <- dimensions_age_sex_year(colnames(total_net_migr))
  migr_data <- array(unlist(migr_data), lengths(dn), dn)
  migr_data
}
