context("dp_tags")

testthat::test_that("tags for a property can be located", {
  mapping <- get_property_tags("version")

  expect_true("VersionNum MV2" %in% names(mapping))
  expect_true("VersionNum MV1" %in% names(mapping))
  expect_equal(mapping$`VersionNum MV2`$func, get_tag_data)
  expect_equal(mapping$`VersionNum MV1`$func, get_tag_data)

  mapping <- get_property_tags("total_population")

  expect_true("BigPop MV3" %in% names(mapping))
  expect_true("BigPop MV2" %in% names(mapping))
  expect_true("BigPop MV" %in% names(mapping))
  expect_true("BigPop3" %in% names(mapping))
  expect_equal(mapping$`BigPop MV3`$rows, 2:163)
  expect_equal(mapping$`BigPop MV2`$rows, c(2+0:80, 245+0:80))
})

testthat::test_that("getting tags for missing property gives useful error", {
  expect_error(get_property_tags("missing_prop"),
    "Can't get the tag names for property missing_prop. Property missing from mapping.")
})
