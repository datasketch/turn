test_that("Geo transformations", {

  path <- turn_sys("geo/rutas.kml")


  kml_into_geojson(path)

  tmp <- kml_into_csv(path, geometry = "POINTS")


})
