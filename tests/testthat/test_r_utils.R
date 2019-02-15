context("r_utils")

testthat::test_that("strict lists work as expected", {
  list <- strict_list(a = 1, b = "foo", c = c(1,2,3))
  expect_equal(class(list), "strict_list")
  expect_length(list, 3)
  expect_equal(list$a, 1)
  expect_error(list$missing, "Element 'missing' does not exist.")
  expect_error(list[["missing"]], "Element 'missing' does not exist.")
})
