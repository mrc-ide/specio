context("read_epp_results")

test_that("Missing SPT data throws an error", {
  pjnz_path <- file.path("pjnz_testdata", "Netherlands2017.PJNZ")
  expect_error(
    read_spt(pjnz_path),
    paste0(
      "Only one file of type SPT must exist at path ",
      pjnz_path,
      ", found 0."
    )
  )
})

test_that("Botswana2017 SPT data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")

  spt <- read_spt(pjnz_path)
  spt_ref <- readRDS("testdata/Botswana2017_spt_data.rds")
  expect_equal(spt, spt_ref)
})

test_that("Botswana2018 SPT data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2018.PJNZ", package = "specio")

  spt <- read_spt(pjnz_path)
  spt_ref <- readRDS("testdata/Botswana2018_spt_data.rds")
  expect_equal(spt, spt_ref)
})

test_that("Mozambique_Maputo_Cidade2018 SPT data is read correctly", {
  pjnz_path <- file.path("pjnz_testdata", "Mozambique_Maputo_Cidade2018.PJNZ")

  spt <- read_spt(pjnz_path)
  spt_ref <- readRDS("testdata/Mozambique_Maputo_Cidade2018_spt_data.rds")
  expect_equal(spt, spt_ref)
})

test_that("Botswana2019 SPU data is read correctly", {
  pjnz_path <- "pjnz_testdata/Botswana2019.PJNZ"

  spu_data <- read_spu(pjnz_path)
  spu_ref <- readRDS("testdata/Botswana2019_spu_data.rds")
  expect_equal(spu_data, spu_ref)
})
