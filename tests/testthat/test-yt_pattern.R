context("YouTube Links")

test_that("YouTube links identified correctly", {

  slide  <- "![text](https://docs.google.com/presentation/d/1fueR3mDU3mGjq0qemekAalmbeYop4_I_7Z9CPHW7fcY/export/png?id=1fueR3mDU3mGjq0qemekAalmbeYop4_I_7Z9CPHW7fcY&pageid=g5cdfd9360d_0_244)"
  youtube_link1  <- "![text](https://www.youtube.com/watch?v=mPg8PD0wM6s)"
  youtube_link2  <- "![text](https://youtu.be/NhRlrMg2k2Q)"

  # ensure that PNGs not identified as youtube link
  png <- grep(pattern = yt_pattern(), slide, perl = TRUE)
  # ensure both versions of youtube links *are* identified as links
  yt <-  grep(pattern = yt_pattern(), youtube_link1, perl = TRUE)
  yt2 <-  grep(pattern = yt_pattern(), youtube_link2, perl = TRUE)

  # test length
  expect_length(png, 0)
  expect_equal(yt, 1)
  expect_equal(yt2, 1)
})
