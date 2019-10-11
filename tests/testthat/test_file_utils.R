context("file_utils")


test_that("filename can be read from zip", {
  zip_path <- file.path("pjnz_testdata", "Netherlands2017.PJNZ")
  expect_error(
    get_filename_from_extension("xml", zip_path),
    paste0(
      "Only one file of type xml must exist at path ",
      zip_path,
      ", found 0."
    )
  )

  zip_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")
  file_name <- get_filename_from_extension("xml", zip_path)
  expect_equal(file_name, "Botswana_29_05_2017 updated BF.xml")

  file_name <- get_filename_from_extension("ep4", zip_path)
  expect_equal(file_name, "Botswana_29_05_2017 updated BF.ep4")
})

test_that("xml workset can be read from file", {
  zip_path <- system.file("testdata", "Botswana2017.PJNZ", package = "specio")
  workset <- get_eppxml_workset(zip_path)
  ## Smoke test that data has been read
  expect_length(names(workset), 66)
  expect_equal(names(workset), attr(workset, "names"))
})

test_that("DP and PJN data can be read", {
  zip_path <- system.file("testdata", "Botswana2018.PJNZ", package = "specio")
  dp_data <- get_dp_data(zip_path)
  ## Simple smoke test data has been read
  expect_length(names(dp_data), 84)

  pjn_data <- get_pjn_data(zip_path)
  ## Simple smoke test data has been read
  expect_length(names(pjn_data), 59)
})

test_that("reading csv data correctly strips any preceeding BOM", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "specio")
  data <- get_pjnz_csv_data(pjnz, "DP")
  ## Not a problem on Linux machines
  expect_equal(colnames(data)[[1]], "Tag")

  ## Mock windows response
  file <- get_filename_from_extension("DP", pjnz)
  csv <- read_csv(unz(pjnz, file))
  colnames(csv)[[1]] <- paste0("ï..", colnames(csv)[[1]])
  mock_read_csv <- mockery::mock(csv)
  with_mock("specio:::read_csv" = mock_read_csv, {
    data <- get_pjnz_csv_data(pjnz, "DP")
  })
  expect_equal(colnames(data)[[1]], "Tag")
})

