
pdf_into_image <- function(){

}


pdf_into_images <- function(path){
  # path <- turn_sys("pdf/human-rights-txt.pdf")
  n_pages <- pdftools::pdf_length(path)
  imgs <- lapply(seq(n_pages), function(i){
    img <- pdftools::pdf_render_page(pdf = path, page = i)
    image_read(img)
  }) %>% unlist
  #imgs1 <- magick::image_read_pdf(path)
  imgs
}

