

#' @export
pdf_text_into_csv <- function(path, to = ""){
  # type... document or table
  validate_ext(path, ext = "pdf")
  if(!is_folder(to)) validate_ext(to, ext = "csv")
  to <- to_parse(to)
  table <- pdf_into_text_table(to)
  write_csv(table, to)
}

#' @export
pdf_into_text_vector <- function(path){
  if(is_pdf_with_text(path)){
    txt <- suppressMessages(pdftools::pdf_text(path))
  }else{
    txt <- pdftools::pdf_ocr_text(path, language = "spa")
  }
  txt
}


#' @export
pdf_into_text_table <- function(path){
  txt <- pdf_into_text_vector()
  d <- data.frame(page = seq_along(txt), text = txt)
}

#' @export
is_pdf_with_text <- function(path){
  # PDF error: Invalid Font Weight
  suppressMessages(
    nrow(pdftools::pdf_fonts(path)) > 0
  )
}







