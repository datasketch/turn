

#' @export
txt_table_into_table <- function(){

}


#' @export
txt_into_csv <- function(path, to){

}

#' @export
txt_into_xlsx <- function(path, to){
  validate_ext(path, ext = "txt")
  if(!is_folder(to)) validate_ext(to, ext = "xlsx")
  d <- table_read(path)
  to <- to_parse(to, ext = "xlsx")
  table_write(d, to, format = "xlsx")
}


#' @export
table_read <- function(path){
  d <- NULL
  ld <- NULL

  ext <- tools::file_ext(path)

  if(ext %in% c("csv","txt","tsv")){
    encoding <- readr::guess_encoding(path)$encoding[1]
    grouping_mark <- ","
    decimal_mark <- "."
    if(encoding == "ISO-8859-1"){
      grouping_mark <- "."
      decimal_mark <- ","
    }
    d <- vroom::vroom(path, show_col_types = FALSE,
                      locale = readr::locale(encoding = encoding,
                                             grouping_mark = grouping_mark,
                                             decimal_mark = decimal_mark))
  }

  if( ext == "json"){
    d <- jsonlite::read_json(path)
  }

  if(ext %in% c("xls","xlsx")){

    sheets <- readxl::excel_sheets(path)
    if(length(sheets) > 1){
      ld <- purrr::map(sheets, ~ readxl::read_excel(path, sheet = .))
      names(ld) <- sheets
    }else{
      d <- readxl::read_excel(path)
    }
  }
  if(ext %in% "ods"){
    if(length(sheets > 1)){
      ld <- purrr::map(sheets, ~ readxl::read_excel(path, sheet = .))
      ld <- purrr::map(ld, function(d){
        d <- dstools::discard_all_empty_rows(d)
        dstools::discard_all_empty_columns(d)
      })
    }else{
      d <- readODS::read_ods(path)
    }
  }

  # if(is.null(d))
  #   stop("Format not supported")

  # if(tools::file_ext(path) %in% c("sql")){
  #   stop("xlsx not supported yet")
  # }

  if(!is.null(d)){
    d <- dstools::discard_all_empty_rows(d)
    d <- dstools::discard_all_empty_columns(d)
  }

  if(!is.null(ld)) return(ld)

  d
}

#' @export
tables_read <- function(path){
  d <- NULL

  if(tools::file_ext(path) %in% c("xlsx")){
    stop("xlsx not supported yet")
  }
  if(tools::file_ext(path) %in% c("sql")){
    stop("xlsx not supported yet")
  }
  d
}


#' @export
table_write <- function(d, to, format = NULL, ...){
  if(is.null(format) & nchar(tools::file_ext(to)) == 0){
    stop("Need format or extension in to argument")
  }else if(is.null(format)){
    format <- tools::file_ext(to)
  }
  if(format == "csv"){
    to <- to_parse(to, ext = "csv")
    readr::write_csv(d, to)
  }else if(format == "csv.gz"){
    to <- to_parse(to, ext = "csv.gz")
    readr::write_csv(d, to)
  }
  else if(format == "json"){
    to <- to_parse(to, ext = "json")
    jsonlite::write_json(d, to, auto_unbox = TRUE, ...)
  }else if(format == "xlsx"){
    to <- to_parse(to, ext = "xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Data")
    openxlsx::writeDataTable(wb, 1, d)
    openxlsx::saveWorkbook(wb, file = to, overwrite = TRUE)
  }else {
    stop("Format not supported")
  }
}



# guess_delimiter <- function(path){
#   d <- readr::read_csv(path, n_max = 10, show_col_types = FALSE)
#   if(ncol(d) == 1 & all(grepl(".*;.*", d[[1]]))) #grepl(";.+;", d[[1]])
#     return(";")
#   if(ncol(d) == 1 &  all(grepl(".*\t.*", d[[1]]))) #grepl(";.+;", d[[1]])
#     return("\t")
#   if(ncol(d) > 1)
#     return(",")
#   NULL
# }

