

#' @export
folder_into_zip <- function(folder, to){
  to <- to_parse(to, ext = "zip")
  zip::zipr(file.path(to),
            list.files(folder, full.names = TRUE))
  to
}

#' @export
files_into_zip <- function(files, to){
  to <- to_parse(to, ext = "zip")
  zip::zipr(file.path(to), files)
  to
}

