context("dp_file_utils")

testthat::test_that("getting invalid raw tag data returns useful error", {
  # tag_data <- get_raw_tag_data("DupeTag", dp_data)
  expect_error(
    get_raw_tag_data("DupeTag", dp_data),
    "Can't find exactly 1 property matching Tag DupeTag, found 2."
  )
  expect_error(
    get_raw_tag_data("Missing", dp_data),
    "Can't find exactly 1 property matching Tag Missing, found 0."
  )
})

testthat::test_that("malformed data returns useful error", {
  expect_error(
    get_raw_tag_data("NumNewARTPats MV", malformed_data),
    "Can't find next occurance of tag <End> starting from index 25."
  )
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

testthat::test_that("Spectrum version can be found from data", {
  spectrum_version <- get_spectrum_version(dp_data)
  expect_equal(spectrum_version, "Spectrum2017")

  spectrum_version <- get_spectrum_version(dp_data_2016)
  expect_equal(spectrum_version, "Spectrum2016")

  expect_error(
    get_spectrum_version(malformed_data),
    "Spectrum DP file version not recognised. Only Spectrum versions from 2016 onwards are supported."
  )
})

test_that("tag can be located within DP data", {
  test_tags <- list(
    "FirstYear MV3" = list(
      func = print
    ),
    "FirstYear MV2" = list(
      func = print
    ),
    "FirstYear MV" = list(
      func = print
    )
  )
  tag <- get_tag(test_tags, dp_data)

  expect_equal(tag, "FirstYear MV2")
})

test_that("tags missing from data return useful error message", {
  test_tags <- list(
    "MissingTag MV2" = list(
      func = print
    ),
    "MissingTag MV" = list(
      func = print
    )
  )

  expect_error(get_tag(test_tags, dp_data),
    sprintfr("Can't find any of the tags MissingTag MV2, MissingTag MV within
             the dp data."))
})

test_that("tags without a function return useful error message", {
  test_tags <- list(
    "FirstYear MV3" = list(
      func = print
    ),
    "FirstYear MV2" = list(
    ),
    "FirstYear MV" = list(
      func = print
    )
  )

  expect_error(get_tag(test_tags, dp_data),
    sprintfr("Can't find a function for extracting tag data for tag FirstYear
              MV2. Fix tag configuration."))
})
