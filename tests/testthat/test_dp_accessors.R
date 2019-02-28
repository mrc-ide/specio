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
    dimensions = get_specpop_dimnames
  )
  tot_pop <- get_array_data("BigPop MV3", tag_data, total_pop_config, 1970:2025)

  expect_type(tot_pop, "double")
  expect_equal(dim(tot_pop), setNames(c(81, 2, 56),
                                      c("age", "sex", "year")))

  asfr_data <- get_raw_tag_data("ASFR MV", dp_data)
  asfr_config <- list(
    rows = 2:8,
    type = "numeric",
    dimensions = get_agegr_dimnames,
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

testthat::test_that("incomplete cfg returns a useful error", {
  tag_data <- get_raw_tag_data("BigPop MV3", dp_data)
  total_pop_config <- list(
    type = "numeric",
    dimensions = get_specpop_dimnames
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
