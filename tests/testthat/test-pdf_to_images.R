context("PDF conversion")

test_that("PDF converted to PNG", {

  skip_on_cran()

  ex_file = system.file("extdata", "example.pdf", package = "didactr")
  res = pdf_to_images(ex_file)

  expect_length(res, 2)
  expect_type(res, "character")
})



