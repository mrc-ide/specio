context("specio")

test_that("Missing SPT data throws an error", {
  pjnz_path <- system.file("testdata", "Netherlands2017.PJNZ", package="specio")
  expect_error(specio::read_bf_incidence_and_prevalence(pjnz_path),
               paste0("Only one file of type SPT must exist at path ",
               pjnz_path,
               ", found 0."))
})

test_that("Botswana2017 SPT data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package="specio")

  incidence_prevalence_bf <- specio::read_bf_incidence_and_prevalence(pjnz_path)
  incidence_prevalence_bf_ref <- readRDS(
    "testdata/Botswana2017_incidence_prevalence_bf_data.rds")
  expect_equal(incidence_prevalence_bf, incidence_prevalence_bf_ref)
})

test_that("Botswana2018 SPT data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2018.PJNZ", package="specio")

  incidence_prevalence_bf <- specio::read_bf_incidence_and_prevalence(pjnz_path)
  incidence_prevalence_bf_ref <- readRDS(
    "testdata/Botswana2018_incidence_prevalence_bf_data.rds")
  expect_equal(incidence_prevalence_bf, incidence_prevalence_bf_ref)
})

test_that("Mozambique_Maputo_Cidade2018 SPT data is read correctly", {
  pjnz_path <- system.file("testdata", "Mozambique_Maputo_Cidade2018.PJNZ",
                      package="specio")

  incidence_prevalence_bf <- specio::read_bf_incidence_and_prevalence(pjnz_path)
  incidence_prevalence_bf_ref <- readRDS(
    "testdata/Mozambique_Maputo_Cidade2018_incidence_prevalence_bf_data.rds")
  expect_equal(incidence_prevalence_bf, incidence_prevalence_bf_ref)
})

test_that("Botswana2017 XML data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package="specio")

  hiv_prevalence <- specio::read_epp_prevalence(pjnz_path)
  hiv_prevalence_ref <- readRDS("testdata/Botswana2017_hiv_prevalence_data.rds")
  expect_equal(hiv_prevalence, hiv_prevalence_ref)
})

test_that("Botswana2018 XML data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2018.PJNZ", package="specio")

  hiv_prevalence <- specio::read_epp_prevalence(pjnz_path)
  hiv_prevalence_ref <- readRDS("testdata/Botswana2018_hiv_prevalence_data.rds")
  expect_equal(hiv_prevalence, hiv_prevalence_ref)
})

test_that("Mozambique_Maputo_Cidade2018 SPT data is read correctly", {
  pjnz_path <- system.file("testdata", "Mozambique_Maputo_Cidade2018.PJNZ",
                           package="specio")

  hiv_prevalence <- specio::read_epp_prevalence(pjnz_path)
  hiv_prevalence_ref <- readRDS(
    "testdata/Mozambique_Maputo_Cidade2018_hiv_prevalence_data.rds")
  expect_equal(hiv_prevalence, hiv_prevalence_ref)
})
