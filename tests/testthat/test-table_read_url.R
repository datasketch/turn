

test_that("write tables work",{

  path <- "tmp/sub"
  d <- cars

  table_write(d, to = path, format = "json")
  table_write(d, to = path, format = "csv.gz")

  path <- "tmp/googlesheets"

  url <- "https://docs.google.com/spreadsheets/d/1lzMEFcq8k5gL7somEwN9ahuppw3Wp0YhgZpC1WAMFrw/edit?usp=sharing"
  d <- url_into_table(url)
  gsheet <- d
  expect_true(inherits(d,"turn_tables"))





  url <- "https://docs.google.com/spreadsheets/d/1lzMEFcq8k5gL7somEwN9ahuppw3Wp0YhgZpC1WAMFrw/edit?usp=sharing"
  d <- turn::url_into_table(url)
  tmpdir <- "tmp/tst"
  path <- table_write(d, to = tmpdir, format = "csv")
  table_read(path)

})
