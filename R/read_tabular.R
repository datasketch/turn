
read_tabular <- function(path, ...){

  ext <- turn::which_ext(path)
  encoding <- readr::guess_encoding(path)$encoding[1]
  grouping_mark <- ","
  decimal_mark <- "."
  if(encoding == "ISO-8859-1"){
    grouping_mark <- "."
    decimal_mark <- ","
  }
  d <- tryCatch({
    vroom::vroom(path, show_col_types = FALSE,
                 locale = readr::locale(encoding = encoding,
                                        grouping_mark = grouping_mark,
                                        decimal_mark = decimal_mark))

  }, error = function(e) {
    #warning("Vroom Error:")
    #print(paste0("Vroom error: ", e$message, "Trying with readr"))
    e
  })
  if(inherits(d, "error")){
    if(ext == "csv"){
      cat("HERE")
      d <- readr::read_csv(path, locale = readr::locale(encoding = encoding))
    }
    if(ext %in% c("txt", "tsv")){
      d <- readr::read_delim(path, delim = "\t",
                             locale = readr::locale(encoding = encoding))
    }
  }
  d



}

