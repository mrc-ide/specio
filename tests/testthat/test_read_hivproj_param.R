context("read_hivproj_param")

test_that("Botswana2017 hivproj data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")

  hivproj <- read_hivproj_param(pjnz_path)
  hivproj_ref <- readRDS("testdata/Botswana2017_hivproj.rds")
 # expect_equal(hivproj, hivproj_ref)
})


test_that("get_property gets the correct data", {
  prop <- get_tag_data("version", dp_data)
  expect_equal(prop, 5.1)

  prop <- get_tag_data("valid_date", dp_data)
  expect_equal(prop, "03-20-18  11:52:25 AM")
})
