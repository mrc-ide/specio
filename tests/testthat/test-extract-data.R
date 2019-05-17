context("extract-data")

test_that("population data can be extracted", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")

  population_data <- extract_population(pjnz_path)
  hivproj_ref <- readRDS("testdata/Botswana2017_hivproj.rds")

  expect_equal(population_data, hivproj_ref$totpop)
})

test_that("hiv population data can be extracted", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")

  hiv_population_data <- extract_hiv_population(pjnz_path)
  hivproj_ref <- readRDS("testdata/Botswana2017_hivproj.rds")

  expect_equal(hiv_population_data, hivproj_ref$hivpop)
})
