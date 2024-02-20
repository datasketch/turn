test_that("Read url", {

  # Sample url
  url <- "https://docs.google.com/spreadsheets/d/1MJNbYfegJ_W3m2WlTL5WjT0lEHr3aP6ufMAqOM8ELCk/edit#gid=0"
  d <- url_into_table(url)
  expect_true(nrow(d) > 3)

  url <- "https://docs.google.com/spreadsheets/d/1lzMEFcq8k5gL7somEwN9ahuppw3Wp0YhgZpC1WAMFrw/edit?usp=sharing"
  d <- url_into_table(url)
  expect_true(is.list(d))
  expect_true(nrow(d[[1]]) > 3)
  expect_true(nrow(d[[2]]) > 3)


})

test_that("Read from github", {

  # Read public github file
  # Test
  expect_true(is_github_or_gist_url("https://github.com/user/repo/blob/main/file.txt"))
  expect_true(is_github_or_gist_url("https://gist.github.com/user/gist_id"))
  expect_false(is_github_or_gist_url("https://example.com")) # FALSE

  url <- "https://github.com/datasketch/turn/blob/main/inst/txt/grupos-biologicos.txt"
  d <- url_into_table(url)
  expect_true(nrow(d) > 10)

})


test_that("Read from googlesheets", {
  # Read googlesheets

  # Árboles bogotá
  url <- "https://docs.google.com/spreadsheets/d/1rV5NRUKeYndVeZ2EDQuCABEDxnOL6g8DtJcAI7szDnA/edit#gid=0"
  d <- read_public_googlesheet_table(url)
  expect_true(nrow(d) > 10)
  # Encuesta alcaldia
  url <- "https://docs.google.com/spreadsheets/d/1W8JBhrs6R49fXbqTxQLSahLrFCeo3OLPs6xL-dnaB2k/edit#gid=0"
  d <- read_public_googlesheet_tables(url)
  expect_true(is.list(d))
  expect_equal(length(d), 2)
  expect_true(nrow(d[[1]]) == 6)

  # Private table
  url <- "https://docs.google.com/spreadsheets/d/1Xqq8yT1WIR7i4vMaifUDy4snKVsXvnJGc2uHPz-Ntkw/edit#gid=0"
  expect_error(read_public_googlesheet_table(url),
               "The Google Sheet appears to be private or inaccessible. Please make sure the sheet is public."
  )

})

test_that("Read url helpers", {

  # Test
  url <- "https://www.example.com"
  expect_true(is_url(url))
  url <- "http://www.example.com"
  expect_true(is_url(url))
  url <- "not_a_url"
  expect_false(is_url(url))

})

