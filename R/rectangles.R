#' Detect rectangles
#'
#' @param img Full path to the image
#' @param threshold ??
#' @param area.threshold Minimum size of the rectangle
#'
#' @return A dataframe
#'
#' @export
detect_rectangles <- function(img, threshold = "50%", area.threshold = 500) {
  `%>%` <- magrittr::`%>%`
  raw <- system(
    paste0(
      "magick convert ", img,
      " -threshold ", threshold,
      " -define connected-components:verbose=true",
      " -define connected-components:area-threshold=", area.threshold,
      " -connected-components 4 -auto-level magick_temp"
    ),
    intern = TRUE
  )

  out <- tibble::as_tibble(raw[2:length(raw)]) %>%
    tidyr::separate(value, c(NA, NA, NA, "bound_box", "centroid", "area", "rgb"), sep = "\\s") %>%
    tidyr::separate(bound_box, c("width", "height", "x_off", "y_off"), "\\+|x", F) %>%
    dplyr::mutate_at(c("width", "height", "x_off", "y_off", "area"), as.integer) %>%
    dplyr::mutate(x0 = x_off, x1 = x_off + width, y0 = y_off, y1 = y_off + height) %>%
    dplyr::mutate(pix_size = width * height) %>%
    dplyr::select(-width, -height, -y_off, -x_off) %>%
    dplyr::arrange(desc(pix_size))

  unlink(paste0(getwd(), "/magick_temp"))
  obj.img <<- magick::image_read(img)

  hw  <- magick::image_attributes(image = obj.img)$value[8]
  hw  <- as.integer(unlist(stringi::stri_split(hw, fixed = ", ")))
  pix <- hw[1] * hw[2]

  out <- dplyr::mutate(out, rel_size = pix_size / pix)

  return(out)
}
