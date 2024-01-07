test_that("tables", {

  # Separator \t

  path <- turn_sys("table/grupos-biologicos.txt")

  #delim <- guess_delimiter(path)

  d <- table_read(path)
  #readr::spec(d)
  txt_into_xlsx(path, to = "tmp")

})

test_that("write tables work",{

 path <- "tmp/sub"
 d <- cars

 table_write(d, to = path, format = "json")
 table_write(d, to = path, format = "csv.gz")

 path <- "tmp/googlesheets"

 url <- "https://docs.google.com/spreadsheets/d/1lzMEFcq8k5gL7somEwN9ahuppw3Wp0YhgZpC1WAMFrw/edit?usp=sharing"
 d <- url_into_table(url)
 expect_true(class(d) == "turn_tables")

 table_write(d, path, format = "xlsx")
 table_write(d, path, format = "json", pretty = TRUE)


})
