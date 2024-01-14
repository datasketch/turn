test_that("multiplication works", {


  path <- turn_sys("xlsx/sample.xlsx")
  ld <- table_read(path)

  path <- "tmp/folder"
  table_write(ld, path, format = "xlsx")
  expect_true(file.exists("tmp/folder/folder.xlsx"))
  table_write(ld, path, format = "json", pretty = TRUE)
  expect_true(file.exists("tmp/folder/folder.json"))


})
