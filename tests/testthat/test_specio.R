context("specio")

test_that("Missing SPT data throws an error", {
  pjnz_path <- system.file("testdata", "Netherlands2017.PJNZ", package="specio")
  expect_error(specio::read_spt(pjnz_path),
               paste0("Only one file of type SPT must exist at path ",
               pjnz_path,
               ", found 0."))
})

test_that("Botswana2017 SPT data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package="specio")

  spt <- specio::read_spt(pjnz_path)
  spt_ref <- readRDS("testdata/Botswana2017_spt_data.rds")
  expect_equal(spt, spt_ref)
})

test_that("Botswana2018 SPT data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2018.PJNZ", package="specio")

  spt <- specio::read_spt(pjnz_path)
  spt_ref <- readRDS("testdata/Botswana2018_spt_data.rds")
  expect_equal(spt, spt_ref)
})

test_that("Mozambique_Maputo_Cidade2018 SPT data is read correctly", {
  pjnz_path <- system.file("testdata", "Mozambique_Maputo_Cidade2018.PJNZ",
                      package="specio")

  spt <- specio::read_spt(pjnz_path)
  spt_ref <- readRDS("testdata/Mozambique_Maputo_Cidade2018_spt_data.rds")
  expect_equal(spt, spt_ref)
})

test_that("Botswana2017 XML data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package="specio")

  epp_data <- specio::read_epp_data(pjnz_path)
  epp_data_ref <- readRDS("testdata/Botswana2017_epp_data.rds")
  expect_equal(epp_data, epp_data_ref)
})

test_that("Botswana2018 XML data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2018.PJNZ", package="specio")

  epp_data <- specio::read_epp_data(pjnz_path)
  epp_data_ref <- readRDS("testdata/Botswana2018_epp_data.rds")
  expect_equal(epp_data, epp_data_ref)
})

test_that("Mozambique_Maputo_Cidade2018 SPT data is read correctly", {
  pjnz_path <- system.file("testdata", "Mozambique_Maputo_Cidade2018.PJNZ",
                           package="specio")

  epp_data <- specio::read_epp_data(pjnz_path)
  epp_data_ref <- readRDS(
    "testdata/Mozambique_Maputo_Cidade2018_epp_data.rds")
  expect_equal(epp_data, epp_data_ref)
})
