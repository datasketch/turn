
to_parse <- function(to, ext = NULL){
  if(is.null(ext)) stop("need a default extension")
  # if given a path leave it as is, if not, add ext from name
  to_path <- to
  to <- rename_to_ext(to, ext)
  # # if path given is a folder, prepend the path and keep the same filename
  # if(is_folder(to_path)){
  #   if(!dir.exists(to)) dir.create(to)
  #   to <- file.path(to, basename(rename_to_ext(to, ext)))
  # }
  # if dir doesnt exist create it
  if(!fs::dir_exists(to)){
    fs::dir_create(fs::path_dir(to))
  }
  to
}


turn_sys <- function(file){
  system.file(file, package = "turn")
}

validate_ext <- function(path, ext = NULL){
  if(is.null(ext))
    stop("Need a target extension")
  if(tolower(tools::file_ext(path)) != ext)
    stop("File ext different from required ext: ", ext)
}

rename_to_ext <- function(path, ext, full_path = TRUE){
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

is_folder <- function(x){
  tools::file_path_sans_ext(x) == x
}

