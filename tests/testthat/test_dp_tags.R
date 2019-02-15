context("dp_tags")

testthat::test_that("tags can be retrieved for different Spectrum versions", {
  tags_2016 <- get_dp_tags("Spectrum2016")
  ## Simple test that there are any tags - we don't really care here
  ## specifically what the tags are.
  expect_true(length(tags_2016) > 0)

  tags_2017 <- get_dp_tags("Spectrum2017")
  expect_true(length(tags_2017) > 0)

  expect_error(
    get_dp_tags("Spectrum2015"),
    "Can't get tag names for Spectrum version Spectrum2015, version not supported."
  )
})
