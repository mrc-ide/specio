context("dp_file_utils")

testthat::test_that("getting invalid dp properties returns useful error", {
  expect_error(
    get_dp_property("DupeTag", dp_data),
    "Can't find exactly 1 property matching Tag DupeTag, found 2."
  )
  expect_error(
    get_dp_property("Missing", dp_data),
    "Can't find exactly 1 property matching Tag Missing, found 0."
  )
})

testthat::test_that("malformed data returns useful error", {
  expect_error(
    get_dp_property("NumNewARTPats MV", malformed_data),
    "Can't find next occurance of tag <End> starting from index 25."
  )
})

testthat::test_that("getting missing data returns empty data", {
  data <- get_dp_property("EmptyTag", dp_data)
  expect_equal(data, list())
})

testthat::test_that("properties can be retrieved from DP file", {
  haart_by_sex <- get_dp_property("HAARTBySex MV", dp_data)
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

  new_art_patients <- get_dp_property("NumNewARTPats MV", dp_data)
  expect_equal(rownames(new_art_patients), c("Males", "Females"))
  expect_equal(dim(new_art_patients), c(2, 56))
})

testthat::test_that("get_last_non_na_column correctly identifies column", {
  example <- rbind(
    c(1, 2, 3, NA, NA),
    c(3, 4, 5, NA, NA),
    c(6, 7, 8, NA, NA)
  )
  column <- get_last_non_na_column(example)
  expect_equal(column, 3)

  example <- rbind(
    c(1, 2, 3, NA, NA),
    c(3, 4, 5, NA, NA),
    c(6, 7, 8, NA, 5)
  )
  column <- get_last_non_na_column(example)
  expect_equal(column, 5)

  example <- rbind(
    c(NA, 2, 3, NA, NA),
    c(NA, 4, 5, NA, NA),
    c(NA, 7, 8, NA, 5)
  )
  column <- get_last_non_na_column(example)
  expect_equal(column, 5)

  example <- rbind(
    c(NA, NA, NA, NA, NA),
    c(NA, NA, NA, NA, NA),
    c(NA, NA, NA, NA, NA)
  )
  column <- get_last_non_na_column(example)
  expect_equal(column, 0)
})
