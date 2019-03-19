test_that("array dimensions are set up correctly", {
  years <- seq.int(2000, 2005)
  dimensions <- dimensions_agegr_year_alternative(years)

  expect_equal(dimensions$agegr,
               c("15-17", "18-19", "20-24", "25-29", "30-34", "35-49"))
  expect_equal(dimensions$year, years)
})
