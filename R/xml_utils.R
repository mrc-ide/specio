#' Parse xml node containing array to vector.
#'
#' Parses XML representation of java array from EPP .xml file.
#'
#' Note that the Java XML representation for an array can have omitted array
#' entries. This indicate that that value should be the default value for the
#' type of the array e.g. can get an array of length 5 with only 3 elements in
#' it. For the missing elements we should use default values at their index in
#' the array.
#'
#' The default values are
#' Java type | R type    | Default value
#' int       | integer   | 0
#' double    | numeric   | 0
#' boolean   | logical   | FALSE
#' String    | character | ""
#'
#'
#' @param xml_node `xml_node` object representing an array.
#' @return A vector representing the array.
#'
#' @keywords internal
#'
parse_array <- function(xml_node) {
  a_length <- as.integer(xml2::xml_attr(xml_node, "length"))
  a_mode <- switch(xml2::xml_attr(xml_node, "class"),
                   int = "integer",
                   double = "numeric",
                   boolean = "logical",
                   java.lang.String = "character")
  arr <- vector(a_mode, a_length)

  elem <- xml2::xml_children(xml_node)
  idx <- as.integer(xml2::xml_attr(elem, "index")) + 1L ## Java is 0-based
  convert_elem <- switch(a_mode,
                         integer = xml2::xml_integer,
                         numeric = xml2::xml_double,
                         logical = function(x) as.logical(xml2::xml_text(x)),
                         character = xml2::xml_text)
  arr[idx] <- convert_elem(elem)

  return(arr)
}

#' Parse xml node representing matrix to an R matrix.
#'
#' Parses XML representation of java array of arrays from EPP .xml file.
#'
#' This uses parse_array to parse each array in turn then groups together into
#' a matrix. If an array is missing at specified index this will add an NA row
#' to the returned matrix at that index.
#'
#' @param xml_node `xml_node` object representing a matrix.
#' @return Matrix
#'
#' @keywords internal
#'
parse_matrix <- function(xml_node) {
  node_class <- xml2::xml_attr(xml_node, "class")
  if(!node_class %in% c("[D", "[I")) {
    stop(paste0("Can't parse matrix for array of type ",
                node_class,
                ", array class must be '[D' or '[I'."))
  }
  rows <- xml2::xml_children(xml_node)
  idx <- as.integer(xml2::xml_attr(rows, "index")) + 1L ## Java is 0-based
  parsed_rows <- lapply(xml2::xml_find_first(rows, "array"), parse_array)
  column_length <- length(parsed_rows[[1]])
  if(!all(lengths(parsed_rows) == column_length)) {
    stop(paste0("Not all rows are the same length, can't parse matrix. This ",
                "might be a ragged array."))
  }

  ## Put rows at correct index in returned matrix
  no_of_rows <- as.integer(xml2::xml_attr(xml_node, "length"))
  parsed_matrix <- matrix(nrow = no_of_rows, ncol = column_length)
  for (i in seq_along(parsed_rows)) {
    parsed_matrix[idx[i],] <- parsed_rows[[i]]
  }
  parsed_matrix
}

#' Get data for an array property from the xml_node.
#'
#' Will get array data from one xml_node. If node represents a vector then a
#' vector is returned. If node repsents a matrix return matrix.
#'
#' @param node The xml node.
#'
#' @return Parsed property data.
#' @keywords internal
#'
get_array_property_data <- function(node) {
  property_data <- xml2::xml_find_all(node, "array")
  if(xml2::xml_attr(property_data, "class") %in% c("[D", "[I")) {
    parsed_data <- parse_matrix(property_data)
  } else {
    parsed_data <- parse_array(property_data)
  }
  return(parsed_data)
}

#' Get character value of enum property from the xml_node.
#'
#' Get the enum property of the xml node as a character.
#'
#' @param node The xml node.
#'
#' @return Parsed property data.
#' @keywords internal
#'
get_enum_property <- function(node) {
  as.character(xml2::xml_text(xml2::xml_find_all(node, ".//string")))
}


#' Get data for a property in a nodeset.
#'
#' Locates an xml property wthin the nodeset, parses the value depending on
#' the type of the property and returns the parsed value.
#'
#' @param nodeset The nodeset to get the property from.
#' @param property The property to get.
#'
#' @return Parsed property data.
#' @keywords internal
#'
get_property <- function(nodeset, property) {
  property_node <- nodeset[xml2::xml_attr(nodeset, "property") == property]
  if (length(property_node) != 1) {
    stop(paste0("Can't get property data for property ", property,
                " from nodeset. Expected only 1 property but found ",
                length(property_node), "."))
  }
  conversion_function <- get_conversion_function(property_node)
  conversion_function(property_node)
}

#' Get the relevant conversion function for an xml node.
#'
#' Looks at the children of the node to determine the type of data contained in
#' the node. If the node represents an array then this will return all of the
#' array data. If the children contain single property data then it will return
#' the function to convert the data at the specified search location.
#'
#' @param node The nodeset to get the property from.
#' @param search Passed to xml_child, either the child number to convert, or
#' name of the child node to convert. If multiple with the same name exist, the
#' first will be returned. Defaults to first.
#'
#' @return Conversion function for the node type.
#' @keywords internal
#'
get_conversion_function <- function(node, search = 1) {
  switch(xml2::xml_name(xml2::xml_child(node, search)),
    string = function(x) xml2::xml_text(xml2::xml_child(x, search)),
    int = function(x) { xml2::xml_integer(xml2::xml_child(x, search)) },
    double = function(x) { xml2::xml_double(xml2::xml_child(x, search)) },
    boolean = function(x) {
      as.logical(xml2::xml_text(xml2::xml_child(x, search)))
    },
    array = get_array_property_data,
    object = get_enum_property,
    stop("can't work out how to convert this")
  )
}


#' Get field data from parent node.
#'
#' Get all key value pair field data containing in within the node.
#'
#' @param node The node containing field data.
#'
#' @keywords internal
#'
get_fields <- function(node) {
  field_nodes <- xml2::xml_find_all(node, './/void[@method="getField"]')
  lapply(field_nodes, get_field_data)
}

#' Get field data from xml node representing serialised field.
#'
#' Function relies on knowing the structure in which fields are persisted in
#' the xml. e.g.
#'
#' <void class=".." method="getField">
#'   <string>fieldName</string>
#'   <void method="set">
#'     <object idref="../>
#'     <double>fieldValue</double>
#'   </void>
#' </void>
#'
#' @param field_node Node representing a serialised field.
#'
#' @keywords internal
#'
get_field_data <- function(field_node) {
  field_title_converter <- get_conversion_function(field_node)
  value_node <- xml2::xml_child(field_node, 2)
  field_value_converter <- get_conversion_function(value_node, 2)
  stats::setNames(field_value_converter(value_node),
                  field_title_converter(field_node))
}
