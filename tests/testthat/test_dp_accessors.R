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

testthat::test_that("total population can be retrieved", {
  tag_data <- get_raw_tag_data("BigPop MV3", dp_data)
  tot_pop <- get_total_population("BigPop MV3", tag_data,
                                  list(rows = 2:163), 1970:2025)

  expect_type(tot_pop, "double")
  expect_equal(dim(tot_pop), setNames(c(81, 2, 56),
                                      c("age", "sex", "year")))
})
