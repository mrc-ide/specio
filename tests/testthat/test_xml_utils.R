context("xml_utils")


test_that("parse_array works as expected", {
  xml_node <- xml2::xml_child(test_array_1)
  parsed_array <- parse_array(xml_node)
  expect_equal(typeof(parsed_array), "integer")
  expect_equal(parsed_array, c(6, 7, 8))

  xml_node <- xml2::xml_child(test_array_2)
  parsed_array <- parse_array(xml_node)
  expect_equal(typeof(parsed_array), "integer")
  expect_equal(parsed_array, c(0, 6, 7, 0, 8))

  xml_node <- xml2::xml_child(string_array)
  parsed_array <- parse_array(xml_node)
  expect_equal(typeof(parsed_array), "character")
  expect_equal(parsed_array, c("Gaborone (%)", "Francistown (%)", ""))

  xml_node <- xml2::xml_child(double_array)
  parsed_array <- parse_array(xml_node)
  expect_equal(typeof(parsed_array), "double")
  expect_equal(parsed_array, c(6.2, 7.000005, 0))

  xml_node <- xml2::xml_child(logical_array)
  parsed_array <- parse_array(xml_node)
  expect_equal(typeof(parsed_array), "logical")
  expect_equal(parsed_array, c(TRUE, FALSE, FALSE))
})

test_that("parse_matrix works as expected", {
  xml_node <- xml2::xml_child(test_array_1)
  expect_error(parse_matrix(xml_node), paste0("Can't parse matrix for array of",
    " type int, array class must be '[D' or '[I'."), fixed = TRUE)

  xml_node <- xml2::xml_child(test_ragged_matrix)
  expect_error(parse_matrix(xml_node), paste0("Not all rows are the same ",
    "length, can't parse matrix. This might be a ragged array."))

  xml_node <- xml2::xml_child(test_matrix)
  parsed_matrix <- parse_matrix(xml_node)
  expect_equal(typeof(parsed_matrix), "integer")
  expect_equal(parsed_matrix, rbind(c(6, 7, 8, 0 ,0),
                                   c(NA, NA, NA, NA, NA),
                                   c(0, 6, 7, 0, 8)))
})

test_that("get property works as expected", {
  xml_nodeset <- xml2::xml_children(test_nodeset)
  expect_error(get_property(xml_nodeset, "prop"),
               paste0("Can't get property data for property prop from nodeset.",
                      " Expected only 1 property but found 0."))

  property1 <- get_property(xml_nodeset, "testProperty1")
  expect_equal(property1, c(6, -1, 8))

  property2 <- get_property(xml_nodeset, "testProperty2")
  expect_equal(property2, rbind(c(6, 7, 8, 0 ,0),
                                c(NA, NA, NA, NA, NA),
                                c(0, 6, 7, 0, -1)))

  property3 <- get_property(xml_nodeset, "testProperty3")
  expect_equal(property3, 72)

  property4 <- get_property(xml_nodeset, "testProperty4")
  expect_equal(property4, 0.1)

  property5 <- get_property(xml_nodeset, "testProperty5")
  expect_equal(property5, "Example")

  property6 <- get_property(xml_nodeset, "testProperty6")
  expect_equal(property6, TRUE)

  property7 <- get_property(xml_nodeset, "testProperty7")
  expect_equal(property7, "URBAN")
})
