context("extract-data")

test_that("population data can be read", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")

  population_data <- read_total_pop(pjnz_path)
  hivproj_ref <- readRDS("testdata/Botswana2017_hivproj.rds")

  expect_equal(population_data, hivproj_ref$totpop)

  total_pop_long <- read_total_pop(pjnz_path, TRUE)
  long_ref <- utils::type.convert(as.data.frame.table(hivproj_ref$totpop))

  expect_equivalent(total_pop_long, long_ref)
})

test_that("hiv population data can be read", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")

  hiv_population_data <- read_hiv_pop(pjnz_path)
  hivproj_ref <- readRDS("testdata/Botswana2017_hivproj.rds")

  expect_equal(hiv_population_data, hivproj_ref$hivpop)

  hiv_long <- read_hiv_pop(pjnz_path, TRUE)
  long_ref <- utils::type.convert(as.data.frame.table(hivproj_ref$hivpop))

  expect_equivalent(hiv_long, long_ref)
})

test_that("art population data can be read", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")

  art_population <- read_art_pop(pjnz_path)
  hivproj_ref <- readRDS("testdata/Botswana2017_hivproj.rds")

  expect_equal(art_population, hivproj_ref$artpop)

  art_long <- read_art_pop(pjnz_path, TRUE)
  long_ref <- utils::type.convert(as.data.frame.table(hivproj_ref$artpop))

  expect_equivalent(art_long, long_ref)
})

