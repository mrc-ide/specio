context("dp_accessors")

testthat::test_that("properties can be retrieved from DP file", {
  tag_data <- get_raw_tag_data("HAARTBySex MV", dp_data)
  haart_by_sex <- get_tag_data("HAARTBySex MV", tag_data)
  male_female <- c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 50, 200, 500, 1000, 2000, 6156,
    15858, 34600, 52825, 71659, 85665, 108894, 132870, 152062,
    169577, 195926, 213953, 235485, 264482, 290288, 311823,
    0, 0, 0, 0, 0, 0, 0, 0
  )
  males <- c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 24, 95, 237, 469, 932, 2850, 7295, 15826, 24033, 32441, 38605,
    48489, 51952, 59456, 67523, 73081, 83241, 88904, 98381, 106642, 114602,
    85, 87.5, 90, 95, 95, 95, 95, 95
  )
  females <- c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 26, 105, 263, 531, 1068, 3306, 8563, 18774, 28792, 39218, 47060,
    60405, 80918, 92606, 102054, 122845, 130712, 146581, 166101, 183646,
    197221, 90, 91.25, 92.5, 95, 95, 95, 95, 95
  )
  expect_length(haart_by_sex, 3)
  expect_true("Male+Female" %in% names(haart_by_sex))
  expect_true("Males" %in% names(haart_by_sex))
  expect_true("Females" %in% names(haart_by_sex))
  expect_equal(as.numeric(haart_by_sex$`Male+Female`), male_female)
  expect_equal(as.numeric(haart_by_sex$Males), males)
  expect_equal(as.numeric(haart_by_sex$Females), females)

  tag_data <- get_raw_tag_data("NumNewARTPats MV", dp_data)
  new_art_patients <- get_tag_data("NumNewARTPats MV", tag_data)
  expect_equal(rownames(new_art_patients), c("Males", "Females"))
  expect_equal(dim(new_art_patients), c(2, 56))
})

testthat::test_that("scalar properties can be retrieved", {
  tag_data <- get_raw_tag_data("VersionNum MV2", dp_data)
  version <- get_tag_data("VersionNum MV2", tag_data)
  expect_equal(version, 5.1)
})

testthat::test_that("can get notes from DP data", {
  tag_data <- get_raw_tag_data("ValidDate MV", dp_data)
  notes <- get_tag_notes("ValidDate MV", tag_data)
  expect_equal(notes, "03-20-18  11:52:25 AM")
})

testthat::test_that("array data can be retrieved with different configs", {
  tag_data <- get_raw_tag_data("BigPop MV3", dp_data)
  total_pop_config <- list(
    rows = 2:163,
    type = "numeric",
    dimensions = dimensions_age_sex_year
  )
  tot_pop <- get_array_data("BigPop MV3", tag_data, total_pop_config, 1970:2025)

  expect_type(tot_pop, "double")
  expect_equal(dim(tot_pop), setNames(c(81, 2, 56),
                                      c("age", "sex", "year")))

  asfr_data <- get_raw_tag_data("ASFR MV", dp_data)
  asfr_config <- list(
    rows = 2:8,
    type = "numeric",
    dimensions = dimensions_agegr_year,
    convert_percent = TRUE
  )
  asfr <- get_array_data("ASFR MV", asfr_data, asfr_config, 1970:2025)

  expect_type(asfr, "double")
  expect_equal(dim(asfr), setNames(c(7, 56),
                                      c("agegr", "year")))
  ## Data has been converted from persisted percent.
  expect_equal(asfr[1,1], 12.0599 / 100)

  title_data <- get_raw_tag_data("FileTitle MV2", dp_data)
  title_config <- list(
    rows = 1,
    cols = 4,
    dimensions = function(x) {
      list("test" = 1,
           "test2" = 1)
    }
  )
  title <- get_array_data("FileTitle MV2", title_data, title_config, 1970:2025)

  expect_type(title, "character")
  expect_equivalent(title, "DP FILE TITLE")
})

testthat::test_that("incomplete array data cfg returns a useful error", {
  tag_data <- get_raw_tag_data("BigPop MV3", dp_data)
  total_pop_config <- list(
    type = "numeric",
    dimensions = dimensions_age_sex_year
  )
  expect_error(
    get_array_data("BigPop MV3", tag_data, total_pop_config, 1970:2025),
    sprintfr("Can't get array data for tag BigPop MV3. Configuration is
             incomplete. Must specify rows and dimensions at minimum.
             rows are null: TRUE, dimension function is null: FALSE.")
  )

  total_pop_config <- list(
    rows = 2:163,
    type = "numeric"
  )
  expect_error(
    get_array_data("BigPop MV3", tag_data, total_pop_config, 1970:2025),
    sprintfr("Can't get array data for tag BigPop MV3. Configuration is
             incomplete. Must specify rows and dimensions at minimum.
             rows are null: FALSE, dimension function is null: TRUE.")
  )
})

test_that("vector data can be retrieved", {
  tag_data <- get_raw_tag_data("FertilityDisc MV", dp_data)
  fert_config <- list(
    type = "numeric",
    cols = 5:11
  )
  cd4fert <- get_vector_data("FertilityDisc MV", tag_data, fert_config, NULL)

  expect_equal(cd4fert, c(1.00, 0.91, 0.77, 0.57, 0.38, 0.29, 0.28))
})

test_that("incomplete vector data cfg returns a useful error", {
  tag_data <- get_raw_tag_data("FertilityDisc MV", dp_data)
  fert_config <- list(
    type = "numeric"
  )
  expect_error(
    get_vector_data("FertilityDisc MV", tag_data, fert_config, NULL),
    sprintfr("Can't get vector data for tag FertilityDisc MV. Configuration
              is incomplete. Must specify cols at a minimum.")
  )
})

test_that("getting vector data for property with multiple rows returns error", {
  tag_data <- get_raw_tag_data("BigPop MV3", dp_data)
  config <- list(
    type = "numeric",
    cols = 2:9
  )
  expect_error(
    get_vector_data("BigPop MV3", tag_data, config, NULL),
    sprintfr("Can't get vector data for tag BigPop MV3. Must only be 1 row
              of data for this tag to get as a vector. Got %i rows.",
             nrow(tag_data))
  )
})

test_that("cd4 data can be retrieved", {
  tag_data <- get_raw_tag_data("AdultMortByCD4NoART MV", dp_data)
  config <- list(
    rows = list(male = 2, female = 3),
    cols = 4:31,
    type = "numeric",
    dimensions = dimensions_cd4
  )
  cd4_mortality <- get_cd4_array_data("AdultMortByCD4NoART MV", tag_data,
                                      config, NULL)

  expected_mort <- c(0.005, 0.011, 0.026, 0.061, 0.139, 0.321, 0.737,
                     0.004, 0.010, 0.026, 0.069, 0.185, 0.499, 1.342,
                     0.005, 0.013, 0.036, 0.096, 0.258, 0.691, 1.851,
                     0.005, 0.013, 0.032, 0.080, 0.203, 0.513, 1.295)
  expect_equivalent(cd4_mortality, array(rep(expected_mort, 2), c(7, 4, 2)))
  expect_equal(dimnames(cd4_mortality)$cd4stage, as.character(seq_len(7)))
  expect_equal(dimnames(cd4_mortality)$agecat,
               c("15-24", "25-34", "35-44", "45+"))
  expect_equal(dimnames(cd4_mortality)$sex, c("male", "female"))
})

test_that("misconfigured cd4 cfg returns useful error", {
  tag_data <- get_raw_tag_data("AdultMortByCD4NoART MV", dp_data)
  config <- list(
    rows = list(female = 3),
    cols = 4:31,
    type = "numeric",
    dimensions = dimensions_cd4
  )
  expect_error(
    get_cd4_array_data("AdultMortByCD4NoART MV", tag_data, config, NULL),
    sprintfr("Can't get CD4 array data for tag AdultMortByCD4NoART MV.
              Must specify rows for both male and female data."))
})

test_that("art mortality rates can be retrieved", {
  tag_data <- get_raw_tag_data("MortalityRates MV2", dp_data)
  cfg <- list(
    rows = 1:2,
    type = "numeric"
  )
  rates <- get_art_mortality_rates("MortalityRates MV2", tag_data, cfg,
                                   1970:2025)
  expect_equivalent(dim(rates), c(3, 56))
  expect_equal(rates[1, ], rates[2, ])
  expect_true(all(rates[1, ] != rates[3, ]))

  tag_data <- get_raw_tag_data("MortalityRates MV", dp_data)
  cfg <- list(
    rows = 1,
    type = "numeric"
  )
  rates <- get_art_mortality_rates("MortalityRates MV", tag_data, cfg,
                                   1970:2025)
  expect_equivalent(dim(rates), c(3, 56))
  expect_equal(rates[1, ], rates[2, ])
  expect_equal(rates[1, ], rates[3, ])

  tag_data <- get_raw_tag_data("MortalityRates Error", dp_data)
  cfg <- list(
    rows = 1:3,
    type = "numeric"
  )
  expect_error(
    get_art_mortality_rates("MortalityRates Error", tag_data, cfg, 1970:2025),
    sprintfr("Can't handle MortalityRates Error with 3 rows. Either config is
                  misconfigured or this is a new case which needs handling."))

  cfg <- list(
    type = "numeric"
  )
  expect_error(
    get_art_mortality_rates("MortalityRates MV", tag_data, cfg, 1970:2025),
    sprintfr("Can't get art mortality rates using tag MortalityRates MV, rows
              must be specified by configuration."))
})

test_that("ART eligibility population data can be retrieved", {
  tag_data <- get_raw_tag_data("PopsEligTreat MV", dp_data)
  cfg <- list(
    rows = 2:8,
    cols = 2:6
  )
  eligibility_data <- get_eligibility_pop_data("PopsEligTreat MV", tag_data,
                                               cfg, 1970:2025)
  expect_equal(names(eligibility_data),
               c("description", "pop", "elig", "percent", "year", "idx"))
  expect_type(eligibility_data$elig, "logical")
  expect_type(eligibility_data$percent, "double")
  expect_type(eligibility_data$year, "integer")
  expect_equal(eligibility_data$percent,
               c(0.100, 0.0168, 0.1480, 0.0070, 0.0030, 0.0001, 0.1000))

  cfg <- list(
    rows = 2:8
  )
  expect_error(
    get_eligibility_pop_data("PopsEligTreat MV", tag_data, cfg, 1970:2025),
    sprintfr("Can't get eligibility pop data via tag PopsEligTreat MV.
              Configuration is incomplete. Must specify rows and cols
              at minimum. rows are null: FALSE, cols are null: TRUE.")
  )
})
