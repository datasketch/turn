test_that("parse path to save works", {


  # Rename ext
  expect_equal(rename_to_ext("test.csv", ext = "xlsx"), "test.xlsx")

  expect_equal(rename_to_ext("test.csv", ext = "csv.gz"), "test.csv.gz")

  expect_equal(rename_to_ext("test.csv.gz", ext = "xls"), "test.xls")

  expect_equal(rename_to_ext("test", "csv"), "test/test.csv")
  expect_equal(rename_to_ext("test/sub", "csv"), "test/sub/sub.csv")

  # to path

  expect_equal(to_parse("test/tmp.csv", "txt"), "test/tmp.txt")
  expect_equal(to_parse("test/tmp.csv.gz", "txt"), "test/tmp.txt")
  expect_equal(to_parse("test/tmp.csv", "csv.gz"), "test/tmp.csv.gz")

})
