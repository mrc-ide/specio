#' Get tags mapping for a property.
#'
#' This gets list of tags which may be used to store data for the property
#' within the DP file. Also specifies for each of these properties the function
#' which should be used to extract the data from full set of DP data. Plus any
#' other metadata related to property required for accessing.
#'
#' @param property Property to get the tags for.
#' @param proj_years Years of the projection.
#'
#' @return List of possible tags used to refer to the property, in order of
#' which they should be used. Also returns the function which should be used
#' to parse the data for the property from the full set of DP data.
#'
#' @keywords internal
get_property_tags <- function(property, proj_years = NULL) {
  model_params <- get_model_params()
  mapping <- switch(
    property,
    version = list(
      "VersionNum MV2" = list(
        func = get_tag_data
      ),
      "VersionNum MV1" = list(
        func = get_tag_data
      )
    ),
    valid_date = list(
      "ValidDate MV" = list(
        func = get_tag_notes
      )
    ),
    valid_version = list(
      "ValidVers MV" = list(
        func = get_tag_data
      )
    ),
    yr_start = list(
      "FirstYear MV2" = list(
        func = get_tag_data
      ),
      "FirstYear MV" = list(
        func = get_tag_data
      )
    ),
    yr_end = list(
      "FinalYear MV2" = list(
        func = get_tag_data
      ),
      "FinalYear MV" = list(
        func = get_tag_data
      )
    ),
    total_population = list(
      "BigPop MV3" = list(
        func = get_array_data,
        rows = 2:163,
        type = "numeric",
        dimensions = dimensions_age_sex_year
      ),
      "BigPop MV2" = list(
        func = get_array_data,
        ## BigPop MV2 contains pop data split by sex: Male, Female, then
        ## subgrouped on region Total, Urban, Rural then one row for each age
        ## group 0 - 80. We only want to get the total data.
        rows = c(2 + 0:80, 245 + 0:80),
        type = "numeric",
        dimensions = dimensions_age_sex_year
      ),
      "BigPop MV" = list(
        func = get_array_data,
        rows = 2:163,
        type = "numeric",
        dimensions = dimensions_age_sex_year
      ),
      "BigPop3" = list(
        func = get_array_data,
        rows = 1:162,
        type = "numeric",
        dimensions = dimensions_age_sex_year
      )
    ),
    survey_rate = list(
      ## Extracts Sx for age 0-79 and 80+. Spectrum caculates a separate Sx for
      ## age 80. Population projection model in EPP-ASM needs to be updated to
      ## handle this.
      "SurvRate MV2" = list(
        func = get_array_data,
        rows = 2 + c(0:79, 81, 82 + 0:79, 82 + 81),
        type = "numeric",
        dimensions = dimensions_age_sex_year
      ),
      "SurvRate MV" = list(
        func = get_array_data,
        rows = 2 + c(0:79, 81, 83 + 0:79, 83 + 81),
        type = "numeric",
        dimensions = dimensions_age_sex_year
      )
    ),
    ## Age-specific fertility rate by single-year
    fertility_rate = list(
      "TFR MV" = list(
        func = get_tag_data
      )
    ),
    asfd = list(
      "ASFR MV" = list(
        func = get_array_data,
        rows = 2:8,
        type = "numeric",
        dimensions = dimensions_agegr_year,
        convert_percent = TRUE
      )
    ),
    sex_ratio_at_birth = list(
      "SexBirthRatio MV" = list(
        func = get_tag_data
      )
    ),
    births = list(
      "Births MV" = list(
        func = get_tag_data
      )
    ),
    total_net_migr = list(
      "MigrRate MV2" = list(
        func = get_array_data,
        rows = c(3, 5),
        type = "numeric",
        dimensions = dimensions_sex_year
      ),
      "MigrRate MV" = list(
        func = get_array_data,
        rows = c(4, 7),
        type = "numeric",
        dimensions = dimensions_sex_year
      )
    ),
    net_migr_age_dist = list(
      ## A row for each of the 17 age groups for male and female, 34 rows total
      "MigrAgeDist MV2" = list(
        func = get_array_data,
        rows = 1 + 1:34,
        type = "numeric",
        dimensions = dimensions_agegr_sex_year,
        convert_percent = TRUE
      ),
      "MigrAgeDist MV" = list(
        func = get_array_data,
        rows = 5 + c(1:17 * 2, 37 + 1:17 * 2),
        type = "numeric",
        dimensions = dimensions_agegr_sex_year,
        convert_percent = TRUE
      )
    ),
    hiv_pop = list(
      "HIVBySingleAge MV2" = list(
        func = get_array_data,
        rows = 2:163,
        type = "numeric",
        dimensions = dimensions_age_sex_year
      ),
      "HIVBySingleAge MV" = list(
        func = get_array_data,
        rows = c(2:82, 84:164),
        type = "numeric",
        dimensions = dimensions_age_sex_year
      )
    ),
    new_infections = list(
      "NewInfectionsBySingleAge MV" = list(
        func = get_array_data,
        rows = c(0:80 * 3 + 2, 0:80 * 3 + 3),
        type = "numeric",
        dimensions = dimensions_age_sex_year
      )
    ),
    art_pop = list(
      "OnARTBySingleAge MV" = list(
        func = get_array_data,
        rows = c(0:80 * 3 + 2, 0:80 * 3 + 3),
        type = "numeric",
        dimensions = dimensions_age_sex_year
      )
    ),
    adult_infec_reduc = list(
      "AdultInfectReduc MV" = list(
        func = get_tag_data
      )
    ),
    incid_pop_ages = list(
      "EPPPopulationAges MV" = list(
        func = get_tag_data
      )
    ),
    hiv_sex_ratio = list(
      "HIVSexRatio MV" = list(
        func = get_tag_data
      )
    ),
    dist_of_hiv = list(
      "DistOfHIV MV2" = list(
        func = get_array_data,
        rows = 2:35,
        type = "numeric",
        dimensions = dimensions_agegr_sex_year
      ),
      "DistOfHIV MV" = list(
        func = get_array_data,
        rows = c(3:19, 21:37),
        type = "numeric",
        dimensions = dimensions_agegr_sex_year
      )
    ),
    fertility_ratio = list(
      "HIVTFR MV4" = list(
        func = get_array_data,
        rows = 1:7,
        type = "numeric",
        dimensions = dimensions_agegr_year
      ),
      "HIVTFR MV3" = list(
        func = get_array_data,
        rows = 1:7,
        type = "numeric",
        dimensions = dimensions_agegr_year
      ),
      "HIVTFR MV2" = list(
        func = get_array_data,
        rows = 1:6,
        type = "numeric",
        ## this version of Spectrum stratified fertility reduction by
        ## 15-17, 18-19, 20-24, ...
        dimensions = dimensions_agegr_year_alternative
      ),
      "HIVTFR MV" = list(
        func = get_array_data,
        rows = 1:7,
        type = "numeric",
        dimensions = dimensions_agegr_year
      )
    ),
    cd4_fertility_ratio = list(
      "FertCD4Discount MV" = list(
        func = get_vector_data,
        cols = seq_len(model_params$DS) + 4,
        type = "numeric"
      ),
      "fallback" = rep(1.0, model_params$DS)
    ),
    women_on_art = list(
      "RatioWomenOnART MV2" = list(
        func = get_tag_data
      ),
      "RatioWomenOnART MV" = list(
        func = get_women_on_art
      ),
      "fallback" = stats::setNames(rep(1.0, model_params$fAG),
                                   get_agegr_labels(between_15_49_only = TRUE))
    ),
    frr_scalar = list(
      "FRRbyLocation MV" = list(
        func = get_tag_data
      ),
      "fallback" = 1.0
    ),
    new_infections_cd4 = list(
      "AdultDistNewInfectionsCD4 MV" = list(
        func = get_cd4_array_data,
        rows = list(male = 2, female = 3),
        cols = 4:31,
        type = "numeric",
        convert_percent = TRUE,
        dimensions = dimensions_cd4
      )
    ),
    mortality_cd4 = list(
      "AdultMortByCD4NoART MV" = list(
        func = get_cd4_array_data,
        rows = list(male = 2, female = 3),
        cols = 4:31,
        type = "numeric",
        dimensions = dimensions_cd4
      )
    ),
    progress_cd4 = list(
      "AdultAnnRateProgressLowerCD4 MV" = list(
        func = get_cd4_array_data,
        rows = list(male = 2, female = 3),
        ## Ignore the value for final CD4 stage as we do not need for progress.
        cols = 3 + c(seq_len(model_params$DS - 1),
                     seq_len(model_params$DS - 1) + model_params$DS,
                     seq_len(model_params$DS - 1) + 2 * model_params$DS,
                     seq_len(model_params$DS - 1) + 3 * model_params$DS),
        type = "numeric",
        dimensions = dimensions_cd4_progress
      )
    ),
    mortality_by_art_cd4_0to6 = list(
      "AdultMortByCD4WithART0to6 MV2" = list(
        func = get_cd4_array_data,
        rows = list(male = 1, female = 2),
        cols = 4:31,
        type = "numeric",
        dimensions = dimensions_cd4
      ),
      "AdultMortByCD4WithART0to6 MV" = list(
        func = get_cd4_array_data,
        rows = list(male = 2, female = 3),
        cols = 4:31,
        type = "numeric",
        dimensions = dimensions_cd4
      )
    ),
    mortality_by_art_cd4_7to12 = list(
      "AdultMortByCD4WithART7to12 MV2" = list(
        func = get_cd4_array_data,
        rows = list(male = 1, female = 2),
        cols = 4:31,
        type = "numeric",
        dimensions = dimensions_cd4
      ),
      "AdultMortByCD4WithART7to12 MV" = list(
        func = get_cd4_array_data,
        rows = list(male = 2, female = 3),
        cols = 4:31,
        type = "numeric",
        dimensions = dimensions_cd4
      )
    ),
    mortality_by_art_cd4_gt12 = list(
      "AdultMortByCD4WithARTGt12 MV2" = list(
        func = get_cd4_array_data,
        rows = list(male = 1, female = 2),
        cols = 4:31,
        type = "numeric",
        dimensions = dimensions_cd4
      ),
      "AdultMortByCD4WithARTGt12 MV" = list(
        func = get_cd4_array_data,
        rows = list(male = 2, female = 3),
        cols = 4:31,
        type = "numeric",
        dimensions = dimensions_cd4
      )
    ),
    art_mortality_rates = list(
      "MortalityRates MV2" = list(
        func = get_art_mortality_rates,
        rows = 1:2
      ),
      "MortalityRates MV" = list(
        func = get_art_mortality_rates,
        rows = 1
      ),
      "fallback" = get_default_art_mortality_rates(proj_years)
    ),
    art_15plus_num_percent = list(
      "HAARTBySexPerNum MV" = list(
        func = get_array_data,
        rows = 3:4,
        dimensions = dimensions_sex_year,
        type = "numeric"
      )
    ),
    art_15plus_num = list(
      "HAARTBySex MV" = list(
        func = get_array_data,
        rows = 3:4,
        dimensions = dimensions_sex_year,
        type = "numeric"
      )
    ),
    art_15plus_need_art = list(
      "NeedARTDec31 MV" = list(
        func = get_array_data,
        rows = 2:3,
        dimensions = dimensions_sex_year,
        type = "numeric"
      )
    ),
    art_15plus_eligibility_threshold = list(
      "CD4ThreshHoldAdults MV" = list(
        func = get_tag_data
      )
    ),
    art_eligibility_pop = list(
      "PopsEligTreat MV" = list(
        func = get_eligibility_pop_data,
        rows = 2:8,
        cols = 2:6
      )
    ),
    median_cd4_init = list(
      "MedCD4CountInit MV" = list(
        func = get_tag_data
      )
    ),
    art_dropout = list(
      "PercLostFollowup MV" = list(
        func = get_tag_data
      )
    ),
    art_allocation_method = list(
      "NewARTPatAllocationMethod MV2" = list(
        func = get_tag_data
      ),
      "fallback" = 1L
    ),
    art_prop_alloc = list(
      "NewARTPatAlloc MV" = list(
        func = get_tag_data
      ),
      "fallback" = stats::setNames(c(0.5, 0.5), c("mx", "elig"))
    ),
    age_14_hiv_population = list(
      "ChAged14ByCD4Cat MV" = list(
        func = get_array_data,
        type = "numeric",
        ## 1 row for each combination of sex, number of paediatric disease
        ## stages (PAED_DS) and each ART treatment stage (TS) plus 4 other
        ## stages (perinatal, breast feeding 0 months, 6 months and 1 year)
        rows = seq_len(
          model_params$NG * model_params$PAED_DS * (4 + model_params$TS)
        ),
        dimensions = dimensions_age_14_hiv_population
      )
    ),
    stop(sprintf(
      "Can't get the tag names for property %s. Property missing from mapping.",
      property
    ))
  )
  mapping
}

