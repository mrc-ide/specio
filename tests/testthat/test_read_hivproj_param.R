context("read_hivproj_param")

test_that("Botswana2017 hivproj data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")

  hivproj <- read_hivproj_param(pjnz_path)
  hivproj_ref <- readRDS("testdata/Botswana2017_hivproj.rds")
 # expect_equal(hivproj, hivproj_ref)
})


test_that("get_property_data gets the correct data", {
  prop <- get_property_data("version", dp_data)
  expect_equal(prop, 5.1)

  prop <- get_property_data("valid_date", dp_data)
  expect_equal(prop, "03-20-18  11:52:25 AM")
})

test_that("get_property_data can use a fallback function", {
  prop <- get_property_data("cd4_fertility_ratio", dp_data)
  expect_equal(prop, rep(1.0, get_model_params()$DS))
})

# test_that("Botswana2017 AIM model params can be extracted", {
#   pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", packge - "specio")
#   dp_data <- get_dp_data(pjnz_path)
#   aim <- get_impact_model_params(pjnz_path, seq.int(1970, 2025))
#   aim_reference <-
#   expect_equal(aim, )
# })
