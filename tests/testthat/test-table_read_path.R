test_that("Table read TXT", {

  # Separator \t
  path <- turn_sys("txt/grupos-biologicos.txt")
  #delim <- guess_delimiter(path)
  d <- table_read(path)
  expect_true(is.data.frame(d))
  #readr::spec(d)
  #txt_into_xlsx(path, to = "tmp")

})

test_that("Table read CSV", {

  # Read csv, tsv, different separators

  path <- turn_sys("csv/base-de-datos-final-mms.csv")
  d <- read_tabular(path)
  expect_true(is.data.frame(d))

})

test_that("Read from folder", {

  path <- turn_sys("xlsx/sample.xlsx")
  ld <- table_read(path)

  path <- "tmp/folder"


  path <- "tmp/folder/csv_files"
  table_write(ld, path, format = "csv", pretty = TRUE)


  ld2 <- table_read(path)
  expect_true(inherits(ld2, "turn_tables"))
  expect_equal(length(ld2),2)

  unlink("tmp/folder", recursive = TRUE)

})


