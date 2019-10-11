files <- dir("xml_testdata", full.names = TRUE)
xml_testdata <- lapply(files, xml2::read_xml)
names(xml_testdata) <- sub(".xml$", "", basename(files))
