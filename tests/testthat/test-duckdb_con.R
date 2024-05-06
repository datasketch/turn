test_that("test duckdb connection", {

  con <- duckdb_con()
  duckdb_valid_con(con)

  dbexts <- duckdb_extensions(con)

  expect_true(duckdb_is_installed_ext("spatial", con))
  expect_error(duckdb_is_installed_ext("csv", con))
  duckdb_is_installed_ext("json", con)

  duckdb_load_ext('spatial', con)
  expect_true(duckdb_is_installed_ext("spatial", con))

  duckdb_disconnect(con)



})
