



turn_sys <- function(file){
  system.file(file, package = "turn")
}





`%||%` <- function (x, y){
  suppressWarnings({
    if (is.empty(x))
      return(y)
    else if (is.null(x) || is.na(x))
      return(y)
    else if (class(x) == "character" && all(nchar(x) == 0))
      return(y)
    else x
  })
}

is.empty <- function(x){
  !as.logical(length(x))
}

is_folder <- function(x){
  tools::file_path_sans_ext(x) == x
}

