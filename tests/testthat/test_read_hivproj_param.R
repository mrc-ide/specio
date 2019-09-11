context("read_hivproj_param")

test_that("Botswana2017 hivproj data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")

  hivproj <- read_hivproj_param(pjnz_path)
  hivproj_ref <- readRDS("testdata/Botswana2017_hivproj.rds")

  ## specio sets dim labels where available, these are excluded sometimes in
  ## first90 code for some properties, ignore equality checks in this case.
  added_dimlabels <- c("asfr", "netmigr", "fert_rat", "cd4_initdist",
                       "cd4_prog", "cd4_mort", "artmx_timerr",
                       "artelig_specpop", "age14hivpop")

  ## Check each by name as order within list is not important
  for (name in names(hivproj)) {
    if (!(name %in% added_dimlabels)) {
      expect_equal(hivproj[[name]], hivproj_ref[[name]],
                   info = sprintf("Field %s not equal", name))
    } else {
      expect_equivalent(hivproj[[name]], hivproj_ref[[name]],
                        info = sprintf("Field %s not equivalent", name))
    }
  }
})

test_that("Botswana2018 hivproj data is read correctly", {
  pjnz_path <- system.file("testdata", "Botswana2018.PJNZ", package = "specio")

  hivproj <- read_hivproj_param(pjnz_path)
  hivproj_ref <- readRDS("testdata/Botswana2018_hivproj.rds")

  ## specio sets dim labels where available, these are excluded sometimes in
  ## first90 code for some properties, ignore equality checks in this case.
  added_dimlabels <- c("asfr", "netmigr", "fert_rat", "cd4_initdist",
                       "cd4_prog", "cd4_mort", "artmx_timerr",
                       "artelig_specpop", "age14hivpop")

  ## Check each by name as order within list is not important
  for (name in names(hivproj)) {
    if (!(name %in% added_dimlabels)) {
      expect_equal(hivproj[[name]], hivproj_ref[[name]],
                   info = sprintf("Field %s not equal", name))
    } else {
      expect_equivalent(hivproj[[name]], hivproj_ref[[name]],
                        info = sprintf("Field %s not equivalent", name))
    }
  }
})

test_that("Mozambique2018 hivproj data is read correctly", {
  pjnz_path <- system.file("testdata", "Mozambique_Maputo_Cidade2018.PJNZ",
                           package = "specio")

  hivproj <- read_hivproj_param(pjnz_path)
  hivproj_ref <- readRDS("testdata/Mozambique_Maputo_Cidade2018_hivproj.rds")

  ## specio sets dim labels where available, these are excluded sometimes in
  ## first90 code for some properties, ignore equality checks in this case.
  added_dimlabels <- c("asfr", "netmigr", "fert_rat", "cd4_initdist",
                       "cd4_prog", "cd4_mort", "artmx_timerr",
                       "artelig_specpop", "age14hivpop")

  ## Check each by name as order within list is not important
  for (name in names(hivproj)) {
    if (!(name %in% added_dimlabels)) {
      expect_equal(hivproj[[name]], hivproj_ref[[name]],
                   info = sprintf("Field %s not equal", name))
    } else {
      expect_equivalent(hivproj[[name]], hivproj_ref[[name]],
                        info = sprintf("Field %s not equivalent", name))
    }
  }
})

test_that("DominicanRepublic2017 hivproj data is read correctly", {
  pjnz_path <- system.file("testdata", "DominicanRepublic2017.PJNZ",
                           package = "specio")

  hivproj <- read_hivproj_param(pjnz_path)
  hivproj_ref <- readRDS("testdata/DominicanRepublic2017_hivproj.rds")

  ## specio sets dim labels where available, these are excluded sometimes in
  ## first90 code for some properties, ignore equality checks in this case.
  added_dimlabels <- c("asfr", "netmigr", "fert_rat", "cd4_initdist",
                       "cd4_prog", "cd4_mort", "artmx_timerr",
                       "artelig_specpop", "age14hivpop")

  ## Check each by name as order within list is not important
  for (name in names(hivproj)) {
    if (!(name %in% added_dimlabels)) {
      expect_equal(hivproj[[name]], hivproj_ref[[name]],
                   info = sprintf("Field %s not equal", name))
    } else {
      expect_equivalent(hivproj[[name]], hivproj_ref[[name]],
                        info = sprintf("Field %s not equivalent", name))
    }
  }
})

test_that("Netherlands2017 hivproj data is read correctly", {
  pjnz_path <- system.file("testdata", "Netherlands2017.PJNZ",
                           package = "specio")

  hivproj <- read_hivproj_param(pjnz_path)
  hivproj_ref <- readRDS("testdata/Netherlands2017_hivproj.rds")

  ## specio sets dim labels where available, these are excluded sometimes in
  ## first90 code for some properties, ignore equality checks in this case.
  added_dimlabels <- c("asfr", "netmigr", "fert_rat", "cd4_initdist",
                       "cd4_prog", "cd4_mort", "artmx_timerr",
                       "artelig_specpop", "age14hivpop")

  ## Check each by name as order within list is not important
  for (name in names(hivproj)) {
    if (!(name %in% added_dimlabels)) {
      expect_equal(hivproj[[name]], hivproj_ref[[name]],
                   info = sprintf("Field %s not equal", name))
    } else {
      expect_equivalent(hivproj[[name]], hivproj_ref[[name]],
                        info = sprintf("Field %s not equivalent", name))
    }
  }
})


test_that("get_property_data gets the correct data", {
  prop <- get_property_data("version", dp_data)
  expect_equal(prop, 5.1)

  prop <- get_property_data("valid_date", dp_data)
  expect_equal(prop, "03-20-18  11:52:25 AM")
})

test_that("get_property_data can use a fallback function", {
  prop <- get_property_data("cd4_fertility_ratio", dp_data)
  expect_equal(prop, rep(1.0, cfg$params$DS))
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

test_that("params are available to fallback functions", {
  proj_years <- 2010:2018
  art_mort <- get_property_data("art_mortality_rates", dp_data_2016, proj_years)
  expect_equivalent(ncol(art_mort), length(proj_years))
})

test_that("2019 data can be read", {
  pjnz_path <- system.file("testdata", "Malawi2019.PJNZ", package = "specio")
  hivproj <- read_hivproj_param(pjnz_path)
  expect_equal(hivproj$country, "Malawi")
  expect_equal(hivproj$iso3, 454)
})
