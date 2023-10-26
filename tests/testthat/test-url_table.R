test_that("read url", {



  # Test
  url <- "https://www.example.com"
  expect_true(is_url(url))
  url <- "http://www.example.com"
  expect_true(is_url(url))
  url <- "not_a_url"
  expect_false(is_url(url))

  # Read public github file
  # Test
  expect_true(is_github_or_gist_url("https://github.com/user/repo/blob/main/file.txt"))
  expect_true(is_github_or_gist_url("https://gist.github.com/user/gist_id")))
  expect_false(is_github_or_gist_url("https://example.com"))) # FALSE

  url <- "https://github.com/datasketch/turn/blob/main/inst/table/grupos-biologicos.txt"
  read_public_github_file(url)


  # Read googlesheets

  # Árboles bogotá
  url <- "https://docs.google.com/spreadsheets/d/1rV5NRUKeYndVeZ2EDQuCABEDxnOL6g8DtJcAI7szDnA/edit#gid=0"
  d <- read_public_googlesheet_table(url)
  expect_true(nrow(d) > 10)
  # Encuesta alcaldia
  url <- "https://docs.google.com/spreadsheets/d/1W8JBhrs6R49fXbqTxQLSahLrFCeo3OLPs6xL-dnaB2k/edit#gid=0"
  d <- read_public_googlesheet_tables(url)
  expect_true(nrow(d) > 10)
  # Private table
  url <- "https://docs.google.com/spreadsheets/d/1Xqq8yT1WIR7i4vMaifUDy4snKVsXvnJGc2uHPz-Ntkw/edit#gid=0"
  expect_error(read_public_googlesheet_table(url),
               "The Google Sheet appears to be private or inaccessible. Please make sure the sheet is public."
  )

})
