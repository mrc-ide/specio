context("dp_tags")

testthat::test_that("tags for a property can be located", {
  mapping <- get_property_tags("version")

  expect_equal(mapping$tags, c("VersionNum MV2", "VersionNum MV1"))
  expect_equal(mapping$func, get_tag_data)
})

testthat::test_that("getting tags for missing property gives useful error", {
  expect_error(get_property_tags("missing_prop"),
    "Can't get the tag names for property missing_prop. Property missing from mapping.")
})
