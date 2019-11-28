context("pjn_metadata")

test_that("projection metadata can be read from PJN file for Botswana 2018", {
  pjn_data <- get_pjn_metadata(pjn_botswana_2018)

  expect_equal(pjn_data$country, "Botswana")
  expect_equal(pjn_data$iso3, 72)
  expect_null(pjn_data$region)
  expect_equal(pjn_data$spectrum_version, "5.63")
  expect_equal(pjn_data$projection_name, "Botswana_ 2018 updated ART")
})

test_that("projection metadata can be read from PJN file for Mozambique 2018", {
  pjn_data <- get_pjn_metadata(pjn_mozambique_2018)

  expect_equal(pjn_data$country, "Mozambique")
  expect_equal(pjn_data$iso3, 508)
  expect_equal(pjn_data$region, "SOUTH_Maputo Cidade")
  expect_equal(pjn_data$spectrum_version, "5.63")
  expect_equal(pjn_data$projection_name,
               "11_MZ_Maputo Cidade_v5_63_updated census_22_01_2018")
})

test_that("spectrum region code can be read", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")
  metadata <- read_pjn_metadata(pjnz_path)
  expect_equal(metadata$country, "Botswana")
  expect_equal(metadata$iso3, 72)
  ## Null for Botswana2017 as not a subnational PJNZ
  expect_equal(metadata$spectrum_region_code, 0)

  pjnz_path <- file.path("pjnz_testdata", "Mozambique_Maputo_Cidade2018.PJNZ")
  metadata <- read_pjn_metadata(pjnz_path)
  expect_equal(metadata$country, "Mozambique")
  expect_equal(metadata$iso3, 508)
  expect_equal(metadata$spectrum_region_code, 1)
})
