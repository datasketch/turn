

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
  if(tools::file_ext(path) %in% c("csv","txt","tsv")){
    delim <- guess_delimiter(path)
    d <- readr::read_delim(path, delim = delim, show_col_types = FALSE)
  }
  if(tools::file_ext(path) %in% c("xlsx")){
    d <- readxl::read_xlsx(path)
  }
  # if(tools::file_ext(path) %in% c("sql")){
  #   stop("xlsx not supported yet")
  # }
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
table_write <- function(d, to, format = NULL){
  if(is.null(format) & nchar(tools::file_ext(to)) == 0){
    stop("Need format or extension in to argument")
  }else if(is.null(format)){
    format <- tools::file_ext(to)
  }
  if(format == "csv"){
    to <- to_parse(to, ext = "csv")
    write_csv(d, to)
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



guess_delimiter <- function(path){
  d <- readr::read_csv(path, n_max = 10, show_col_types = FALSE)
  if(ncol(d) == 1 & all(grepl(".*;.*", d[[1]]))) #grepl(";.+;", d[[1]])
    return(";")
  if(ncol(d) == 1 &  all(grepl(".*\t.*", d[[1]]))) #grepl(";.+;", d[[1]])
    return("\t")
  if(ncol(d) > 1)
    return(",")
  NULL
}

