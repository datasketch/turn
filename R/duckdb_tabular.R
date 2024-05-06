
#' @export
duckdb_read_file <- function(path,
                             con,
                             tblname = NULL){

  con <- duckdb_con(con)
  ext <- which_ext(path)

  if(ext == "csv"){
    format <- "csv"
  } else if(ext == "json"){
    format <- "json"
    duckdb_load_ext('json', con)
  } else if(is_geo_format(ext)){
    format <- "sf"
    duckdb_load_ext('spatial', con)
  }

  tblname <- tblname %||% clean_names(sans_ext(basename(path)))
  q <- query_string(tblname, path, format, "TABLE")

  out <- tryCatch({
    DBI::dbSendQuery(con, q)
    d <- dplyr::tbl(con, tblname)
    d
  }, silent = TRUE, error = function(e) {

    if(is_table_format(ext)){
      encoding_issue <- grepl("Invalid unicode", e)
      if(encoding_issue){
        d0 <- read_tabular(path)
        DBI::dbWriteTable(con, tblname, d0, overwrite = TRUE)
      }else{
        stop("Something went wrong while reading table", e)
      }
      return(dplyr::tbl(con, tblname))
    }


    if(is_geo_format(ext)){

      geoformat_issue <- grepl("Geometry.*not supported", e)
      if(geoformat_issue){
        geo0 <- sf::st_read(path)
        DBI::dbWriteTable(con, tblname, geo0)
        dplyr::tbl(con, tblname)
      }
    }
    stop(e)

  })
  out
}


# Borrowed from https://github.com/cboettig/duckdbfs/blob/main/R/open_dataset.R
query_string <- function(tblname,
                         sources,
                         format = c("parquet", "csv", "tsv", "text", "sf"),
                         mode = c("VIEW", "TABLE"),
                         hive_partitioning = TRUE,
                         union_by_name = FALSE,
                         filename = FALSE) {
  # format <- match.arg(format)
  scanner <- switch(format,
                    #"parquet" = "parquet_scan(",
                    "csv"  = "read_csv_auto(",
                    "json" = "read_json_auto(",
                    "sf" = "st_read(",
                    "read_csv_auto("
  )

  source_uris <- vec_as_str(sources)

  vec_as_str <- function(x) {
    if(length(x) <= 1) return(paste0("'",x,"'"))
    paste0("[", paste0(paste0("'", x, "'"), collapse = ","),"]")
  }

  ## Allow overwrites on VIEW
  mode <- switch(mode,
                 "VIEW" = "OR REPLACE TEMPORARY VIEW",
                 "TABLE" = "TABLE")

  tabular_options <- paste0(
    ", HIVE_PARTITIONING=",hive_partitioning,
    ", UNION_BY_NAME=",union_by_name,
    ", FILENAME=",filename)

  options <- switch(format,
                    "parquet" = tabular_options,
                    "csv"  = tabular_options,
                    "sf" = "",
                    tabular_options
  )
  paste0(
    paste("CREATE OR REPLACE", mode, tblname, "AS SELECT * FROM "),
    paste0(scanner, source_uris, options,
           ");")
  )
}


vec_as_str <- function(x) {
  if(length(x) <= 1) return(paste0("'",x,"'"))
  paste0("[", paste0(paste0("'", x, "'"), collapse = ","),"]")
}

