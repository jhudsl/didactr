context("PPTX notes")

test_that("The notes are ordered", {

  ex_file = system.file("extdata", "example.pptx", package = "didactr")
  res = pptx_notes(ex_file)
  ans = c("notesSlide1.xml", "notesSlide2.xml")

  expect_identical(names(res), ans)
})


