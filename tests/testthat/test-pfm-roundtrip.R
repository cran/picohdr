

test_that("PFM round-trip works", {
  exr_file <- system.file("image", "rstats.pfm.bz2", package = "picohdr")
  arr      <- read_pfm(exr_file)
  
  tmp_file <- tempfile(fileext = ".pfm")
  write_pfm(arr, tmp_file)  
  
  arr2 <- read_pfm(tmp_file)

  expect_equal(arr, arr2, ignore_attr = TRUE)
})
