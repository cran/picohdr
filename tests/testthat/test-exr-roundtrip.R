

test_that("EXR round-trip works", {
  exr_file <- system.file("image", "rstats.exr", package = "picohdr")
  arr      <- read_exr(exr_file)
  
  tmp_file <- tempfile(fileext = ".exr")
  write_exr(arr, tmp_file, pixel_type = 'half')  
  
  arr2 <- read_exr(tmp_file)

  expect_equal(arr, arr2, ignore_attr = TRUE)
})
