

#' @export
pdf_into_csv <- function(path, type = NULL){
  # type... document or table

}


#' @export
pdf_into_text_vector <- function(path){
  if(is_pdf_with_text(path)){
    txt <- pdftools::pdf_text(path)
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
  nrow(pdftools::pdf_fonts(path)) > 0
}







