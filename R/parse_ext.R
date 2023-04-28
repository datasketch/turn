
#' @export
to_parse <- function(to = NULL, ext = NULL){
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


validate_ext <- function(path, ext = NULL){
  if(is.null(ext))
    stop("Need a target extension")
  if(tolower(tools::file_ext(path)) != ext)
    stop("File ext different from required ext: ", ext)
}


rename_to_ext <- function(path, ext, full_path = TRUE){
  two_exts <- FALSE

  is_dir <- FALSE
  if(!has_ext(path)){
    is_dir <- TRUE
  }

  base_path <- tools::file_path_sans_ext(path)
  if(has_ext(base_path)){
    base_path <- tools::file_path_sans_ext(base_path)
    two_exts <- TRUE
  }

  if(is_dir){
    base_path <- file.path(base_path, basename(base_path))
  }

  if(full_path){
    return(paste0(base_path,".", ext))
  }
  paste0(basename(base_path),".", ext)
}


has_ext <- function(x){
  grepl("\\.", x)
}




