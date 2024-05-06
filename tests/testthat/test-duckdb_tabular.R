test_that("duckdb tabular", {

  con <- duckdb_con()

  # CSV file
  path <- turn_sys("csv/2023-08-03-wifi_gratuito_en_escuelas_normales.csv")
  x <-duckdb_read_file(path, con)
  x |> collect()

  # JSON file
  path <- turn_sys("json/iris.json")
  x <-duckdb_read_file(path, con)
  x |> collect()


  # geographic file
  path <- turn_sys("geo/col_departments2.geojson")
  x <-duckdb_read_file(path, con)
  x |> collect()

  path <- turn_sys("geo/rutas.geojson")
  x <-duckdb_read_file(path, con)
  x |> collect()

  path <- turn_sys("geo/rutas.kml")
  x <-duckdb_read_file(path, con)
  x |> collect()

  # Check encodings
  path <- turn_sys("csv/base-de-datos-final-mms.csv")
  d <-duckdb_read_file(path, con)
  expect_true(is.data.frame(d))

  path <- turn_sys("csv/encuesta_mujeres.csv")
  d <- duckdb_read_file(path, con) |> collect()
  expect_true(is.data.frame(d))

  # x <- dstools::discard_all_empty_rows(x)
  # x <- dstools::discard_all_empty_columns(x)

  path <- turn_sys("csv/Mumbai Local Train Dataset.csv")
  d <- duckdb_read_file(path, con) |> collect()
  expect_true(is.data.frame(d))


  # x <- dstools::discard_all_empty_rows(x)
  # x <- dstools::discard_all_empty_columns(x)



})
