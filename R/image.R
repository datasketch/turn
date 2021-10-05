
#' @export
image_into_text <- function(path, lang = "en"){
  tesseract::ocr(path, engine = turn:::ocr_engines[[lang]])

}

#' @export
images_into_pdf <- function(path, to = NULL){
  if(length(path) == 1 && !is_folder(path)){
    stop("path must be a vector of image paths or a folder")
  }
  if(is_folder(path)){
    images <- list.files(path, pattern = "\\.|jpg|jpeg|png|svg|ai|",
                         full.names =  TRUE)
  }else{
    images <- path
  }
  tmpdir <- tempdir()
  l <- purrr::map2(images, seq_along(images),
              ~ image_into_pdf(.x,
                               to = file.path(tmpdir, sprintf("%04d.pdf",.y))))
  to <- to_parse(to, ext = "pdf")
  pdftools::pdf_combine(unlist(l), to)
  unlink(tmpdir)
  to
}


#' @export
image_read <- function(path){
  if(tools::file_ext(path) %in% c("png", "jpg", "jpeg")){
    img <- magick::image_read(path)
  } else if(tools::file_ext(path) == "svg"){
    img <- magick::image_read_svg(path)
  }
  img
}


#' @export
image_into_image <- function(path, ext, to = NULL){
  img <- image_read(path)
  img <- image_convert(img, format = ext)
  to <- to_parse(to, ext = ext)
  image_write(img, to)
}

#' @export
image_into_pdf <- function(path, to = NULL){
  image_into_image(path, ext = "pdf", to = to)
}



#' @export
png_into_pdf <- function(path, to){
  validate_ext(path, "png")
  image_into_image(path, ext = "pdf")
}

#' @export
png_into_jpg <- function(path, to){
  validate_ext(path, "png")
  if(!is_folder(to)) validate_ext(to, "jpg")
  image_into_image(path, ext = "jpg")
}

#' @export
png_into_jpeg <- function(path, to){
  validate_ext(path, "png")
  if(!is_folder(to)) validate_ext(to, "jpeg")
  image_into_image(path, ext = "jpeg", to = to)
}



#' @export
jpeg_into_pdf <- function(path, to){
  validate_ext(path, "pdf")
  if(!is_folder(to)) validate_ext(to, "pdf")
  image_into_image(path, ext = "pdf", to = to)
}

#' @export
jpeg_into_jpg <- function(path, to){
  validate_ext(path, "jpeg")
  if(!is_folder(to)) validate_ext(to, "jpg")
  image_into_image(path, ext = "jpg", to = to)
}

#' @export
jpeg_into_png <- function(path, to){
  validate_ext(path, "jpeg")
  if(!is_folder(to)) validate_ext(to, "png")
  image_into_image(path, ext = "png", to = to)
}




#' @export
svg_into_png <- function(path, to){
  validate_ext(path, "png")
  if(!is_folder(to)) validate_ext(to, "svg")
  image_into_image(path, ext = "png", to = to)
}

#' @export
svg_into_pdf <- function(path, to){
  validate_ext(path, "svg")
  if(!is_folder(to)) validate_ext(to, "pdf")
  image_into_image(path, ext = "pdf", to = to)
}

#' @export
svg_into_jpg <- function(path, to){
  validate_ext(path, "svg")
  if(!is_folder(to)) validate_ext(to, "jpg")
  image_into_image(path, ext = "jpg", to = to)
}

#' @export
svg_into_jpeg <- function(path, to){
  validate_ext(path, "svg")
  if(!is_folder(to)) validate_ext(to, "jpeg")
  image_into_image(path, ext = "jpeg", to = to)
}



