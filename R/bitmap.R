#' Convert Picture to Bitmap
#'
#' @param img.in Full path to the picture
#' @param img.out Full path to the converted bitmap picture
#'
#' @return A bitmap picture
#' @export
conv_bitmap <- function(img.in, img.out) {
  system(paste("magick convert", img.in, "-canny 0x1+10%+30%", img.out))
}


#' Adjust bitmap picture
#'
#' @param img.in Full path to the bitmap picture
#' @param img.out Full path to the adjusted bitmap picture
#' @param xpix.ex Minimum number of pixels to form a straight line in the x-axis
#' @param ypix.ex Minimum number of pixels to form a straight line in the y-axis
#' @param xpix.in Maximum number of pixels to connect lines in the x-axis
#' @param ypix.in Maximum number of pixels to connect lines in the y-axis
#'
#' @return
#' A bitmap picture
#' @export
adj_bitmap <- function(img.in, img.out, xpix.ex, ypix.ex, xpix.in, ypix.in) {

  # read bitmap image
  img <- png::readPNG(img.in, info = TRUE)

  # exclude pixels =======================================================================
  # run length encoding on x- and y-pixels
  x <- apply(img, 1, rle)
  y <- apply(img, 2, rle)

  # convert pixels to zero if not in straight line
  for (i in 1:length(x)) x[[i]]$values[which(x[[i]]$lengths < xpix.ex)] <- 0
  for (i in 1:length(y)) y[[i]]$values[which(y[[i]]$lengths < ypix.ex)] <- 0

  # inverse run length encoding
  x <- t(sapply(x, inverse.rle))
  y <- sapply(y, inverse.rle)

  # get new bitmap picture
  z <- x + y
  z[which(z > 1)] <- 1

  # include pixels =======================================================================
  # run length encoding on x- and y-pixels
  x <- apply(z, 1, rle)
  y <- apply(z, 2, rle)

  # convert pixels to one if they are near stright lines
  for (i in 1:length(x)) x[[i]]$values[which(x[[i]]$lengths < xpix.in)] <- 1
  for (i in 1:length(y)) y[[i]]$values[which(y[[i]]$lengths < ypix.in)] <- 1

  # inverse run length encoding
  x <- t(sapply(x, inverse.rle))
  y <- sapply(y, inverse.rle)

  # get new bitmap picture
  z <- x + y
  z[which(z > 1)] <- 1

  # write output
  png::writePNG(z, img.out)
}


#' Crop picture with Magick
#'
#' @param img.in Original image
#' @param img.out Cropped image
#' @param bound.box bounding box
#'
#' @return A picture
#' @export
img_crop <- function(img.in, img.out, bound.box) {
  system(paste("magick convert", pngs$path_png[1], "-crop", bound.box, img.out))
}


