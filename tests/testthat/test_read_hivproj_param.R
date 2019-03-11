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

test_that("scale_cd4_mortality can be interpreted", {
  version <- "5.74 Beta 16"
  expect_equal(get_scale_cd4_mortality(version), 1L)
  version <- 5.74
  expect_equal(get_scale_cd4_mortality(version), 1L)
  version <- "5.74 Beta 12"
  expect_equal(get_scale_cd4_mortality(version), 0L)
  version <- "5.72 Beta 16"
  expect_equal(get_scale_cd4_mortality(version), 0L)
})

test_that("women on ART uses fallback to return default values", {
  women_on_art <- get_property_data("women_on_art", dp_data)
  expect_equal(
    women_on_art,
    setNames(
      rep(1.0, 7),
      c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
    )
  )
})

test_that("ART allocation uses fallback to return default values", {
  art_prop_alloc <- get_property_data("art_prop_alloc", dp_data)
  expect_equal(
    art_prop_alloc,
    stats::setNames(
      c(0.5, 0.5),
      c("mx", "elig")
    )
  )
})
