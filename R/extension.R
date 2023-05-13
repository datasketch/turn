

#' @export
which_ext <- function(x){
  tools::file_ext(x)
}

#' @export
remove_ext <- function(x){
  tools::file_path_sans_ext(x)
}

#' @export
change_ext <- function(x, ext){
  paste0(tools::file_path_sans_ext(x), ".", ext)
}

