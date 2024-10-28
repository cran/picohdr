## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(picohdr)

## ----pfm1---------------------------------------------------------------------
filename <- system.file("image/rstats.pfm.bz2", package = "picohdr") 
im <- read_pfm(filename)
dim(im)

im |> 
  tm_reinhard() |>
  adj_gamma() |> 
  plot()

## ----exr1---------------------------------------------------------------------
library(picohdr)

# EXR file of meta-information about the rendered scene
filename <- system.file("image/rstats.exr", package = "picohdr") 

# Load all images
images <- read_exr(filename)
dim(im)

# Channel names. EXR format wants channels arranged alphabetically
dimnames(images)[[3]]

# Extract RGB channels. Tone-map. Adjust gamma.
images[,,c('R', 'G', 'B')] |>
  tm_reinhard() |>
  adj_gamma() |>
  plot()

# Plot the albedo Green channel
plot(images[, , 'Albedo.G'])

## ----exr2---------------------------------------------------------------------
# Rescale the derivative channel to the range [0,1] and display
images[,,'dzdx'] |>
  adj_rescale(0, 1) |> 
  plot()

## -----------------------------------------------------------------------------
exr_info(filename)

