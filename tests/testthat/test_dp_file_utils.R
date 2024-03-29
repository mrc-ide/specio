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

test_that("all NA data returns null", {
  dp_data <- as.data.frame(cbind(NA, NA, NA, NA, NA))
  colnames(dp_data) <- c("Tag", "Description", "Notes", "Data", "")
  data <- get_data(dp_data)

  expect_null(data)
})

test_that("data is converted as expected", {
  dp_data <- as.data.frame(rbind(
    c("1", "test1", 6),
    c("2", "test2", 7),
    c(NA, "test3", 8)
  ), stringsAsFactors = FALSE)

  expect_error(convert_type(dp_data), "Can't convert non-numeric data.")


  dp_data <- as.data.frame(rbind(
    c("1", "1", 6),
    c("2", "2", 7),
    c(NA, "3", 8)
  ), stringsAsFactors = FALSE)
  converted_data <- convert_type(dp_data)

  expect_type(converted_data[, 1], "double")
  expect_type(converted_data[, 2], "double")
  expect_type(converted_data[, 3], "double")
})
