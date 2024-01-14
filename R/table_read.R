

#' @export
table_read <- function(path){
  d <- NULL
  ld <- NULL

  ext <- which_ext(path)
  if(is.null(ext) || ext %in% c("xlsx", "xlsx", "ods")){
    return(tables_read(path))
  }

  if(ext %in% c("csv","txt","tsv")){
    d <- read_tabular(path)
      } else if( ext == "json"){
    d <- jsonlite::read_json(path)
  }

  # if(tools::file_ext(path) %in% c("sql")){
  #   stop("sql not supported yet")
  # }

  if(!is.null(d)){
    d <- dstools::discard_all_empty_rows(d)
    d <- dstools::discard_all_empty_columns(d)
  }else{
    stop("Could not read file")
  }

  if(!is.null(ld)){
    class(ld) <- c(class(ld), "turn_tables")
    return(ld)
  }
  class(d) <- c(class(d), "turn_table")
  d
}

#' @export
tables_read <- function(path){
  d <- NULL
  ext <- which_ext(path)

  if(is.null(ext)){
    return(tables_read_folder(path))
  }

  if(ext %in% c("xls","xlsx")){

    sheets <- readxl::excel_sheets(path)
    if(length(sheets) > 1){
      ld <- purrr::map(sheets, ~ readxl::read_excel(path, sheet = .))
      names(ld) <- sheets
    }else{
      d <- readxl::read_excel(path)
    }
  }
  if(ext %in% "ods"){
    if(length(sheets > 1)){
      ld <- purrr::map(sheets, ~ readxl::read_excel(path, sheet = .))
      ld <- purrr::map(ld, function(d){
        d <- dstools::discard_all_empty_rows(d)
        dstools::discard_all_empty_columns(d)
      })
    }else{
      d <- readODS::read_ods(path)
    }
  }

  if(!is.null(ld)){
    ld <- set_turn_class(ld, "turn_tables")
    return(ld)
  }
  d <- set_turn_class(d, "turn_table")
  d
}


tables_read_folder <- function(path){
  if(!dir.exists(path)){
    stop("Path not found")
  }
  nms <- NULL
  files <- list.files(path, full.names = TRUE)
  if(all(which_ext(files) == "csv")){
    nms <- sub(".*[___]", "", sans_ext(basename(files)))
  }else{
    stop("Only reading folder with CSVs")
  }
  ld <- lapply(files, function(file){
    table_read(file)
  })
  if(!is.null(nms)){
    names(ld) <- nms
  }
  if(length(ld) == 1){
    class(ld) <- c(class(ld), "turn_table")
    return(ld)
  }
  class(ld) <- c(class(ld), "turn_tables")
  ld
}

