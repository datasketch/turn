test_that("Images", {

  if(!dir.exists("tmp")) dir.create("tmp")


  path <- turn_sys("image/human-rights-women.png")

  #txt <- image_into_text(path)
  #cat(txt)

  image_into_pdf(path, to = "tmp")

  png_into_jpeg(path, to = "tmp/image2.jpeg")
  expect_true(file.exists("tmp/image2.jpeg"))
  expect_error(png_into_jpeg(path, to = "tmp/image2.png"))

  path <- turn_sys("images")
  images_into_pdf(path, to = "tmp")



})
