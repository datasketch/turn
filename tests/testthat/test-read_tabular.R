test_that("Test tabular", {


  # Check encodings
  path <- turn_sys("csv/2023-08-03-wifi_gratuito_en_escuelas_normales.csv")
  d <-read_tabular(path)
  expect_true(is.data.frame(d))

  path <- turn_sys("csv/base-de-datos-final-mms.csv")
  d <- read_tabular(path)
  expect_true(is.data.frame(d))


  path <- turn_sys("csv/encuesta_mujeres.csv")
  d <- read_tabular(path)
  expect_true(is.data.frame(d))

  # x <- dstools::discard_all_empty_rows(x)
  # x <- dstools::discard_all_empty_columns(x)

  path <- turn_sys("csv/Mumbai Local Train Dataset.csv")
  d <- read_tabular(path)
  expect_true(is.data.frame(d))


  # x <- dstools::discard_all_empty_rows(x)
  # x <- dstools::discard_all_empty_columns(x)


})
