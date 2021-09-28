test_that("PDF into TXT", {

  # PDF TEXT
  path <- turn_sys("pdf/human-rights-txt.pdf")
  txts <- pdf_into_text_vector(path)

  # PDF WITH TEXT AND IMAGES
  path <- turn_sys("pdf/human-rights-ilustrated.pdf")
  txts <- pdf_into_text_vector(path)

  # IMAGE/SCANNED TEXT
  path <- turn_sys("image/human-rights-women.pdf")
  txts <- pdf_into_text_vector(path)

  # Turn text pdf into txt

  # Turn image pdf into txt







  library(tidyverse)
  library(pdftools)
  library(tesseract)


  tesseract_info()$available

  if(is.na(match("spa", tesseract_info()$available)))
    tesseract_download("spa")
  spanish <- tesseract("spa")



  files <- list.files("contratos", full.names = TRUE, pattern = "\\.pdf|\\.PDF")
  is_pdf_with_text <- function(path){
    nrow(pdf_fonts(path)) > 0
  }

  lapply(files, function(f){
    if(is_pdf_with_text(f)){
      txt <- pdf_text(f)
    }else{
      txt <- pdf_ocr_text(f, language = "spa")
      #message(f)
      #txt <- "hola"
    }
    d <- data.frame(page = seq_along(txt), text = txt)
    write_csv(d, paste0(tools::file_path_sans_ext(f), ".csv"))
  })




})
