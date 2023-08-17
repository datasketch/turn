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


})
