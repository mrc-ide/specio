files <- dir("xml_testdata", full.name = TRUE)
xml_testdata <- lapply(files, xml2::read_xml)
names(xml_testdata) <- sub(".xml$", "", basename(files))
