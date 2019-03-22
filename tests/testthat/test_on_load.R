context("on load")

test_that(".onLoad correctly reads tag config", {
  expect_true(!is.null(cfg$tags))
  expect_true(!is.null(cfg$tags$version$`VersionNum MV2`))
  expect_type(cfg$tags$version$`VersionNum MV2`$func, "closure")

  expect_true(!is.null(cfg$tags$total_population$`BigPop MV3`))
  expect_type(cfg$tags$total_population$`BigPop MV3`$func, "closure")
  expect_type(cfg$tags$total_population$`BigPop MV3`$rows, "integer")

  expect_true(!is.null(cfg$tags$art_prop_alloc$fallback))
  expect_equal(cfg$tags$art_prop_alloc$fallback(),
               stats::setNames(c(0.5, 0.5), c("mx", "elig")))

  expect_true(!is.null(cfg$tags$art_eligibility_pop$`PopsEligTreat MV`))
  expect_type(cfg$tags$art_eligibility_pop$`PopsEligTreat MV`$cols,
              "integer")

  expect_true(!is.null(
    cfg$tags$new_infections_cd4$`AdultDistNewInfectionsCD4 MV`))
  expect_true(
    cfg$tags$new_infections_cd4$`AdultDistNewInfectionsCD4 MV`$convert_percent)
  expect_type(
    cfg$tags$new_infections_cd4$`AdultDistNewInfectionsCD4 MV`$rows, "list")
})

test_that(".onLoad correctly reads model params", {
  expect_true(!is.null(cfg$params))
  expect_true("NG" %in% names(cfg$params))
  expect_true("AG" %in% names(cfg$params))
  expect_true("DS" %in% names(cfg$params))
  expect_true("TS" %in% names(cfg$params))
  expect_true("fAG" %in% names(cfg$params))
  expect_true("PAED_DS" %in% names(cfg$params))
})
