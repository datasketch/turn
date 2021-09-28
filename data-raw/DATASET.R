
library(tidyverse)


file_types <- tribble(
  ~type, ~format,
  "table","csv",
  "table","xlsx",
  "table", "sav",
  "table", "tsv",
  "table", "json",
  "table", "sql",
  "table", "txt",
  "tables","xlsx",
  "tables","zip",
  "document", "doc",
  "document", "txt",
  "document", "docx",
  "document", "pdf",
  "document", "csv",
  "document", "html",
  "documents", "zip",
  "geo", "shp",
  "geo", "topojson",
  "geo", "geojson",
  "geo", "zip",
  "image", "png",
  "image", "svg",
  "image", "jpeg",
  "images", "gif",
  "images", "zip",
  "video", "gif",
  "video", "mpeg",
  "files", "zip",
  "files", "folder"
)



from_to <- tribble(
  ~from, ~to,
  "table_csv","xlsx",
  "tables_csv","xlsx",
  "doc_pdf", "txt",
  "doc_pdf", "document_csv",
  "html", "csv",
  "html", "document_csv"
)

ocr_engines <- list(en = "eng", es = "spa")



usethis::use_data(file_types, from_to, ocr_engines,
                  overwrite = TRUE, internal = TRUE)
