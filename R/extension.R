

#' @export
which_ext <- function(x){
  ext <- tools::file_ext(x)
  if(length(ext) == 1 && nchar(ext) == 0) return(NULL)
  ext
}

#' @export
sans_ext <- function(x){
  tools::file_path_sans_ext(x)
}

#' @export
change_ext <- function(x, ext){
  paste0(tools::file_path_sans_ext(x), ".", ext)
}

