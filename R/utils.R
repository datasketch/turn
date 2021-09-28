

turn_sys <- function(file){
  system.file(file, package = "turn")
}

rename_to_ext <- function(path, ext, full_path = FALSE){
  base_path <- tools::file_path_sans_ext(path)
  if(full_path){
    return(paste0(base_path,".", ext))
  }
  paste0(basename(base_path),".", ext)
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

is_path <- function(x){
  tools::file_path_sans_ext(x) == x
}

