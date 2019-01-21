context("read_spu")


test_that("read_spu works as expected", {
  pjnz_path <- "pjnz_testdata/Botswana2019.PJNZ"

  spu_data <- read_spu(pjnz_path)
  spu_ref <- readRDS("testdata/Botswana2019_spu_data.rds")
  expect_equal(spu_data, spu_ref)

})
