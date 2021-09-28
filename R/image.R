

#' @export
image_into_text <- function(path, lang = "en"){
  tesseract::ocr(path, engine = turn:::ocr_engines[[lang]])

}

#' @export
image_into_pdf <- function(path, to = NULL){
  img <- magick::image_read(path)
  img <- image_convert(img, format = "pdf")
  to <- to %||% rename_to_ext(path, "pdf")
  if(is_path(to)){
    if(!dir.exists(to)) dir.create(to)
    to <- file.path(to, basename(rename_to_ext(path, "pdf")))
  }
  image_write(img, to)
}


#' @export
images_into_pdf <- function(path){



}
