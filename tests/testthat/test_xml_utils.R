context("xml_utils")


test_that("parse_array can parse integer arrays", {
  xml_node <- xml2::xml_child(xml_test_data$test_array_1)
  parsed_array <- parse_array(xml_node)
  expect_equal(typeof(parsed_array), "integer")
  expect_equal(parsed_array, c(6, 7, 8))
})

test_that("parse_array can parse integer arrays with missing values", {
  xml_node <- xml2::xml_child(xml_test_data$test_array_2)
  parsed_array <- parse_array(xml_node)
  expect_equal(typeof(parsed_array), "integer")
  expect_equal(parsed_array, c(0, 6, 7, 0, 8))
})

test_that("parse_array can parse string arrays with missing values", {
  xml_node <- xml2::xml_child(xml_test_data$string_array)
  parsed_array <- parse_array(xml_node)
  expect_equal(typeof(parsed_array), "character")
  expect_equal(parsed_array, c("Gaborone (%)", "Francistown (%)", ""))
})

test_that("parse_array can parse double arrays with missing values", {
  xml_node <- xml2::xml_child(xml_test_data$double_array)
  parsed_array <- parse_array(xml_node)
  expect_equal(typeof(parsed_array), "double")
  expect_equal(parsed_array, c(6.2, 7.000005, 0))
})

test_that("parse_array can parse logical arrays with missing values", {
  xml_node <- xml2::xml_child(xml_test_data$logical_array)
  parsed_array <- parse_array(xml_node)
  expect_equal(typeof(parsed_array), "logical")
  expect_equal(parsed_array, c(TRUE, FALSE, FALSE))
})

test_that("parse_matrix throws error if node is not of correct type", {
  xml_node <- xml2::xml_child(xml_test_data$test_array_1)
  expect_error(parse_matrix(xml_node), paste0("Can't parse matrix for array of",
    " type int, array class must be '[D' or '[I'."), fixed = TRUE)
})

test_that("parse_matix throws error if matrix is ragged", {
  xml_node <- xml2::xml_child(xml_test_data$test_ragged_matrix)
  expect_error(parse_matrix(xml_node), paste0("Not all rows are the same ",
    "length, can't parse matrix. This might be a ragged array."))
})

test_that("parse_matix can parse matrix with missing rows", {
  xml_node <- xml2::xml_child(xml_test_data$test_matrix)
  parsed_matrix <- parse_matrix(xml_node)
  expect_equal(typeof(parsed_matrix), "integer")
  expect_equal(parsed_matrix, rbind(c(6, 7, 8, 0 ,0),
                                   c(NA, NA, NA, NA, NA),
                                   c(0, 6, 7, 0, 8)))
})

test_that("get_property throws error if no property can be found", {
  xml_nodeset <- xml2::xml_children(xml_test_data$test_nodeset)
  expect_error(get_property(xml_nodeset, "prop"),
               paste0("Can't get property data for property prop from nodeset.",
                      " Expected only 1 property but found 0."))
})

test_that("get_property can return array property", {
  xml_nodeset <- xml2::xml_children(xml_test_data$test_nodeset)
  property1 <- get_property(xml_nodeset, "testProperty1")
  expect_equal(property1, c(6, -1, 8))
})

test_that("get_property can return matrix property", {
  xml_nodeset <- xml2::xml_children(xml_test_data$test_nodeset)
  property2 <- get_property(xml_nodeset, "testProperty2")
  expect_equal(property2, rbind(c(6, 7, 8, 0 ,0),
                                c(NA, NA, NA, NA, NA),
                                c(0, 6, 7, 0, -1)))
})

test_that("get_property can return integer property", {
  xml_nodeset <- xml2::xml_children(xml_test_data$test_nodeset)
  property3 <- get_property(xml_nodeset, "testProperty3")
  expect_equal(property3, 72)
})

test_that("get_property can return double property", {
  xml_nodeset <- xml2::xml_children(xml_test_data$test_nodeset)
  property4 <- get_property(xml_nodeset, "testProperty4")
  expect_equal(property4, 0.1)
})

test_that("get_property can return character property", {
  xml_nodeset <- xml2::xml_children(xml_test_data$test_nodeset)
  property5 <- get_property(xml_nodeset, "testProperty5")
  expect_equal(property5, "Example")
})

test_that("get_property can return logical property", {
  xml_nodeset <- xml2::xml_children(xml_test_data$test_nodeset)
  property6 <- get_property(xml_nodeset, "testProperty6")
  expect_equal(property6, TRUE)
})

test_that("get_property can return enum property", {
  xml_nodeset <- xml2::xml_children(xml_test_data$test_nodeset)
  property7 <- get_property(xml_nodeset, "testProperty7")
  expect_equal(property7, "URBAN")
})

test_that("get field data works as expected", {
  field_node <- xml2::xml_children(xml_test_data$test_field_node)
  field_data <- get_fields(field_node)
  expect_equal(field_data, list(
    "name" = stats::setNames("2004 BAIS", "name"),
    "year" = stats::setNames(2004, "year"),
    "surveyHIV" = stats::setNames(24.68, "surveyHIV"),
    "surveyStandardError" = stats::setNames(0.74, "surveyStandardError")
  ))
})
