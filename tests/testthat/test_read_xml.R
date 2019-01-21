context("read_xml")

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

test_that("Mozambique_Maputo_Cidade2018 XML data is read correctly", {
  pjnz_path <- system.file("testdata", "Mozambique_Maputo_Cidade2018.PJNZ",
                           package="specio")

  epp_data <- specio::read_epp_data(pjnz_path)
  epp_data_ref <- readRDS(
    "testdata/Mozambique_Maputo_Cidade2018_epp_data.rds")
  expect_equal(epp_data, epp_data_ref)
})

test_that("DominicanRepublic2017 XML data is read correctly", {
  pjnz_path <- system.file("testdata", "DominicanRepublic2017.PJNZ",
                           package="specio")

  epp_data <- specio::read_epp_data(pjnz_path)
  epp_data_ref <- readRDS("testdata/DominicanRepublic2017_epp_data.rds")
  expect_equal(epp_data, epp_data_ref)
})

test_that("Botswana2019 XML data is read correctly", {
  pjnz_path <- "pjnz_testdata/Botswana2019.PJNZ"

  epp_data <- specio::read_epp_data(pjnz_path)
  epp_data_ref <- readRDS("testdata/Botswana2019_epp_data.rds")
  expect_equal(epp_data, epp_data_ref)
})


test_that("Botswana2017 subpopulation data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package="specio")

  subpop_data <- specio::read_epp_subpops(pjnz_path)
  subpop_data_ref <- readRDS("testdata/Botswana2017_subpop_data.rds")
  expect_equal(subpop_data, subpop_data_ref)
})

test_that("Botswana2018 subpopulation data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2018.PJNZ", package="specio")

  subpop_data <- specio::read_epp_subpops(pjnz_path)
  subpop_data_ref <- readRDS("testdata/Botswana2018_subpop_data.rds")
  expect_equal(subpop_data, subpop_data_ref)
})

test_that("Mozambique_Maputo_Cidade2018 subpopulation data is read correctly", {
  pjnz_path <- system.file("testdata", "Mozambique_Maputo_Cidade2018.PJNZ",
                           package="specio")

  subpop_data <- specio::read_epp_subpops(pjnz_path)
  subpop_data_ref <- readRDS(
    "testdata/Mozambique_Maputo_Cidade2018_subpop_data.rds")
  expect_equal(subpop_data, subpop_data_ref)
})

test_that("DominicanRepublic2017 subpopulation data is read correctly", {
  pjnz_path <- system.file("testdata", "DominicanRepublic2017.PJNZ",
                           package="specio")

  subpop_data <- specio::read_epp_subpops(pjnz_path)
  subpop_data_ref <- readRDS("testdata/DominicanRepublic2017_subpop_data.rds")
  expect_equal(subpop_data, subpop_data_ref)
})
