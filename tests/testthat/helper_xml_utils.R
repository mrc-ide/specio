files <- dir("xml_test_data", full.name = TRUE)
xml_test_data <- lapply(files, xml2::read_xml)
names(xml_test_data) <- sub(".xml$", "", basename(files))
