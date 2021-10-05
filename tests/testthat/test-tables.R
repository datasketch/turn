test_that("tables", {

  # Separator \t

  path <- turn_sys("table/grupos-biologicos.txt")

  delim <- guess_delimiter(path)

  d <- table_read(path)
  #readr::spec(d)
  txt_into_xlsx(path, to = "tmp")




})
