context("on load")

test_that(".onLoad correctly reads tag config", {
  expect_true(!is.null(tags$config))
  expect_true(!is.null(tags$config$version$`VersionNum MV2`))
  expect_type(tags$config$version$`VersionNum MV2`$func, "closure")

  expect_true(!is.null(tags$config$total_population$`BigPop MV3`))
  expect_type(tags$config$total_population$`BigPop MV3`$func, "closure")
  expect_type(tags$config$total_population$`BigPop MV3`$rows, "integer")

  expect_true(!is.null(tags$config$art_prop_alloc$fallback))
  expect_equal(tags$config$art_prop_alloc$fallback,
               stats::setNames(c(0.5, 0.5), c("mx", "elig")))

  expect_true(!is.null(tags$config$art_eligibility_pop$`PopsEligTreat MV`))
  expect_type(tags$config$art_eligibility_pop$`PopsEligTreat MV`$cols,
              "integer")

  expect_true(!is.null(
    tags$config$new_infections_cd4$`AdultDistNewInfectionsCD4 MV`))
  expect_true(
    tags$config$new_infections_cd4$`AdultDistNewInfectionsCD4 MV`$convert_percent)
  expect_type(
    tags$config$new_infections_cd4$`AdultDistNewInfectionsCD4 MV`$rows, "list")

})
