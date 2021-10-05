

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
  table_write(d, format = "xlsx", to)
}


#' @export
table_read <- function(path){
  d <- NULL
  if(tools::file_ext(path) %in% c("csv","txt","tsv")){
    delim <- guess_delimiter(path)
    d <- readr::read_delim(path, delim = delim, show_col_types = FALSE)
  }
  # if(tools::file_ext(path) %in% c("xlsx")){
  #   stop("xlsx not supported yet")
  # }
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
table_write <- function(d, format, to){
  if(format == "xlsx"){
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Data")
    openxlsx::writeDataTable(wb, 1, d)
    openxlsx::saveWorkbook(wb, file = to, overwrite = TRUE)
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

