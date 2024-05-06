

#' @export
kml_into_geojson <- function(path, to = NULL){
  validate_ext(path, "kml")
  if(is.null(to)) to <- change_ext(path, "geojson")
  if(!is_folder(to)) validate_ext(to, "geojson")
  to <- geo_into_geo(path, to)
  to
}

#' @export
kml_into_csv <- function(path, to = NULL, ...){
  validate_ext(path, "kml")
  if(is.null(to)) to <- change_ext(path, "csv")
  if(!is_folder(to)) validate_ext(to, "csv")

  opts <- list(...)
  filter_geometry <- opts$geometry_type

  geo <- sf::st_read(path)

  d <- geo |>
    dplyr::mutate(
      lon = unlist(purrr::map(geo$geometry,1)),
      lat = unlist(purrr::map(geo$geometry,2)),
      geometry_type = purrr::map_chr(geo$geometry, ~ class(.)[2])
      )
  if(!is.null(filter_geometry)){
    available_geometries <- unique(d$geometry_type)
    if(!filter_geometry %in% available_geometries)
      stop("geometry_type must be one of: ",
           paste0(available_geometries,collapse = ", "))

    d <- d |>
      sf::st_drop_geometry() |>
      filter(geometry_type == "POINT")
  }
  readr::write_csv(d, to)
  d
}


#' @export
geo_into_geo <- function(path, to){
  geo <- sf::st_read(path)
  #to <- to_parse(to, ext = ext)
  sf::st_write(geo, dsn = to, delete_dsn = TRUE)
  to
}


#' @export
is_geo_format <- function(format){
  format %in% c("fgb", "shp", "json", "geojson", "gdb", "gpkg",
                   "kml", "gmt")
}
