## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 4
)


## ----setup--------------------------------------------------------------------
library(picohdr)

## -----------------------------------------------------------------------------
file <- system.file("image/rstats.exr", package = "picohdr")
im <- picohdr::read_exr(file)
dim(im)

## -----------------------------------------------------------------------------
dimnames(im)[[3]]

## -----------------------------------------------------------------------------
im[1:5, 1:5, 1]

## -----------------------------------------------------------------------------
rgb_arr <- im[, , c('R', 'G', 'B')]

## ----error=TRUE---------------------------------------------------------------
plot(as.raster(rgb_arr))

## -----------------------------------------------------------------------------
library(ggplot2)
df <- array_to_df(rgb_arr)

ggplot(df) + 
  geom_density(aes(value, group = channel, colour = channel)) + 
  theme_bw() + 
  theme(legend.position = 'bottom') + 
  coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 5)) +
  scale_color_manual(values = c('blue', 'green', 'red')) +
  labs(
    title = "Raw HDR pixel values",
    subtitle = "Some values greater than 1.0 in this image"
  )

## -----------------------------------------------------------------------------
rgb_clamped <- rgb_arr |>
  adj_clamp(lo = 0, hi = 1) |>
  adj_gamma()

oldpar <- par(mai = c(0, 0, 0, 0))
plot(as.raster(rgb_clamped))
par(oldpar)

## ----echo=FALSE---------------------------------------------------------------
df <- array_to_df(rgb_clamped)

ggplot(df) + 
  geom_density(aes(value, group = channel, colour = channel)) + 
  theme_bw() + 
  theme(legend.position = 'bottom') + 
  coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 5)) +
  scale_color_manual(values = c('blue', 'green', 'red')) +
  labs(
    title = "Pixel values clamped to [0, 1]"
  )

## -----------------------------------------------------------------------------
rgb_rescale <- rgb_arr |>
  adj_rescale(lo = 0, hi = 1) |>
  adj_gamma()

oldpar <- par(mai = c(0, 0, 0, 0))
plot(as.raster(rgb_rescale))
par(oldpar)

## ----echo=FALSE---------------------------------------------------------------
df <- array_to_df(rgb_rescale)

ggplot(df) + 
  geom_density(aes(value, group = channel, colour = channel)) + 
  theme_bw() + 
  theme(legend.position = 'bottom') + 
  coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 5)) +
  scale_color_manual(values = c('blue', 'green', 'red')) +
  labs(
    title = "Pixel values rescaled to [0, 1]"
  )

## -----------------------------------------------------------------------------
rgb_rh1<- rgb_arr |>
  tm_reinhard() |>
  adj_gamma() 

oldpar <- par(mai = c(0, 0, 0, 0))
plot(as.raster(rgb_rh1))
par(oldpar)

## ----echo=FALSE---------------------------------------------------------------
df <- array_to_df(rgb_rh1)

ggplot(df) + 
  geom_density(aes(value, group = channel, colour = channel)) + 
  theme_bw() + 
  theme(legend.position = 'bottom') + 
  coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 5)) +
  scale_color_manual(values = c('blue', 'green', 'red')) +
  labs(
    title = "Tone mapping: Reinhard"
  )

## -----------------------------------------------------------------------------
rgb_rh2 <- rgb_arr |>
  tm_reinhard_basic() |>
  adj_gamma() 

oldpar <- par(mai = c(0, 0, 0, 0))
plot(as.raster(rgb_rh2))
par(oldpar)

## ----echo=FALSE---------------------------------------------------------------
df <- array_to_df(rgb_rh2)

ggplot(df) + 
  geom_density(aes(value, group = channel, colour = channel)) + 
  theme_bw() + 
  theme(legend.position = 'bottom') + 
  coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 5)) +
  scale_color_manual(values = c('blue', 'green', 'red')) +
  labs(
    title = "Tone mapping: Reinhard (extended)"
  )

## -----------------------------------------------------------------------------
rgb_rh3 <- rgb_arr |>
  tm_reinhard_variant() |>
  adj_gamma() 

oldpar <- par(mai = c(0, 0, 0, 0))
plot(as.raster(rgb_rh3))
par(oldpar)

## ----echo=FALSE---------------------------------------------------------------
df <- array_to_df(rgb_rh3)

ggplot(df) + 
  geom_density(aes(value, group = channel, colour = channel)) + 
  theme_bw() + 
  theme(legend.position = 'bottom') + 
  coord_cartesian(xlim = c(0, 2), ylim = c(0, 5)) +
  scale_color_manual(values = c('blue', 'green', 'red')) +
  labs(
    title = "Tone mapping: Reinhard (variant)"
  )

