
#' @export
duckdb_con <- function(con = NULL, dbdir = NULL){
  dbdir <- dbdir %||% ":memory:"
  if(duckdb_valid_con(con)){
  } else {
    con <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
  }
  con
}

#' @export
duckdb_valid_con <- function(con){
  if(is.null(con)) return(FALSE)
  tryCatch({
    version <- DBI::dbExecute(con, "PRAGMA version;")
    TRUE  # Connection is valid
  }, silent = TRUE, error = function(e) {
    FALSE  # Connection is not valid
  })
}

#' @export
duckdb_disconnect <- function(con){
  duckdb::dbDisconnect(con, shutdown = TRUE)
}


#' @export
duckdb_extensions <- function(con = NULL) {
  query <- "SELECT * FROM duckdb_extensions();"
  DBI::dbGetQuery(con, query)
}


#' @export
duckdb_load_ext <- function(ext, con){
  if(!duckdb_is_installed_ext(ext, con)){
    duckdb_install_ext(ext, con)
  }
  if(!duckdb_is_loaded_ext(ext, con)){
    DBI::dbExecute(con, glue::glue("LOAD {ext};"))
  }
}

#' @export
duckdb_install_ext <- function(ext, con){
  if(!duckdb_is_loaded_ext(ext, con)){
    DBI::dbExecute(con, glue::glue("INSTALL {ext};"))
  }
}

#' @export
duckdb_is_loaded_ext <- function(ext, con){
  exts <- duckdb_extensions(con) |>
    filter(extension_name == ext) |>
    pull(loaded)
  if(length(exts) == 0) stop("No extension_name found")
  exts
}

#' @export
duckdb_is_installed_ext <- function(ext, con){
  exts <- duckdb_extensions(con) |>
    filter(extension_name == ext) |>
    pull(installed)
  if(length(exts) == 0) stop("No extension_name found")
  exts
}





