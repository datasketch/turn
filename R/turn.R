
#' @export
turn <- function(from, to){

}



file_type <- function(path){
  ext <- tools::file_ext(path)

  doc_formats <- c("docx", "doc", "pdf")
  slides_formats <- c("ppt", "pptx")
  table_formats <- c("csv","xlsx")
  data_formats <- c("json")

  if(ext %in% doc_formats) return("doc")
  if(ext %in% table_formats) return("table")

}



