test_that("File read works", {


  # Read csv, tsv, different separators

  # Check encodings
  path <- "tmp/tables/2023-08-03-wifi_gratuito_en_escuelas_normales.csv"
  encoding <- readr::guess_encoding(path)$encoding[1]
  x <- vroom::vroom(path, show_col_types = FALSE,
                    locale = readr::locale(encoding = encoding))

  path <- "tmp/tables/base-de-datos-final-mms.csv"
  encoding <- readr::guess_encoding(path)$encoding[1]
  x <- vroom::vroom(path, show_col_types = FALSE,
                    locale = readr::locale(encoding = encoding))

  path <- "tmp/tables/cicloparqueaderos-del-sistema-transmilenio-agosto-2020.csv"
  encoding <- readr::guess_encoding(path)$encoding[1]
  if(encoding == "ISO-8859-1"){
    grouping_mark <- "."
    decimal_mark <- ","
  }
  x <- vroom::vroom(path, show_col_types = FALSE,
                    locale = readr::locale(encoding = encoding,
                                           grouping_mark = grouping_mark,
                                           decimal_mark = decimal_mark))
  x <- dstools::discard_all_empty_rows(x)
  x <- dstools::discard_all_empty_columns(x)


  path <- "tmp/tables/encuentas mujeres.csv"
  encoding <- readr::guess_encoding(path)$encoding[1]
  if(encoding == "ISO-8859-1"){
    grouping_mark <- "."
    decimal_mark <- ","
  }
  x <- vroom::vroom(path, show_col_types = FALSE,
                    locale = readr::locale(encoding = encoding,
                                           grouping_mark = grouping_mark,
                                           decimal_mark = decimal_mark))
  x <- dstools::discard_all_empty_rows(x)
  x <- dstools::discard_all_empty_columns(x)


  path <- "tmp/tables/Mumbai Local Train Dataset.csv"
  encoding <- readr::guess_encoding(path)$encoding[1]
  if(encoding == "ISO-8859-1"){
    grouping_mark <- "."
    decimal_mark <- ","
  }
  x <- vroom::vroom(path, show_col_types = FALSE,
                    locale = readr::locale(encoding = encoding,
                                           grouping_mark = grouping_mark,
                                           decimal_mark = decimal_mark))
  x <- dstools::discard_all_empty_rows(x)
  x <- dstools::discard_all_empty_columns(x)


  path <- "tmp/tables/tmp data.xlsx"
  l <- table_read(path)







})
