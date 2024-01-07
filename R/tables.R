

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
table_write <- function(d, to, format = NULL, ...){

  if(is_turn_tables(d) || is_list_of_data_frames(d)){
    return(tables_write(d, to, format = format, ...))
  }

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

  to
}

tables_write <- function(ld, to, format = NULL, ...){

  if(!inherits(ld, "turn_tables")){
    if(!is_list_of_data_frames(ld)){
      stop("Must be a list tables")
    }
  }
  if(is.null(names(ld))){
    names(ld) <- paste("Sheet", 1:length(l))
  }
  to_init <- to
  nms <- names(ld)

  if(is.null(format) & nchar(tools::file_ext(to)) == 0){
    stop("Need format or extension in to argument")
  }else if(is.null(format)){
    format <- tools::file_ext(to)
  }

  if(format == "csv"){
    to <- to_parse(to, ext = "csv")
    lapply(seq_along(ld), function(i){
      to <- append_sheet_to_path(to, nms[i])
      readr::write_csv(ld[[i]], to)
    })
    to <- to_init
  }else if(format == "csv.gz"){
    to <- to_parse(to, ext = "csv.gz")
    lapply(seq_along(ld), function(i){
      to <- append_sheet_to_path(to, nms[i])
      readr::write_csv(ld[[i]], to)
    })
    to <- to_init
  }
  else if(format == "json"){
    to <- to_parse(to, ext = "json")
    jsonlite::write_json(ld, to, auto_unbox = TRUE, ...)
  }else if(format == "xlsx"){
    to <- to_parse(to, ext = "xlsx")
    wb <- openxlsx::createWorkbook()
    lapply(seq_along(ld), function(i){
      openxlsx::addWorksheet(wb, nms[i])
      openxlsx::writeDataTable(wb, nms[i], ld[[i]])
    })
    openxlsx::saveWorkbook(wb, file = to, overwrite = TRUE)
  }else {
    stop("Format not supported")
  }

  to

}

is_turn_tables <- function(l){
  inherits(l, "turn_tables")
}

is_list_of_data_frames <- function(l){
  all(unlist(lapply(l, is.data.frame)))
}


append_sheet_to_path <- function(to, append, sep = "___"){
  paste0(sans_ext(to),sep, append,".", which_ext(to))

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

