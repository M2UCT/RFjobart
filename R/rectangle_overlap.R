#' Generate pixel positions of rectangle in larger image
#'
#' @param v A vector containing the start- and end-positions of the rectangles
#' (x0, y0, x1, y1) and the width of the larger image (w)
#'
#' @return
#' A vector or a matrix
#' @export
#'
#' @examples
#' pix_vec(v = c(2,2,5,5,6,8), "vector")
#' pix_vec(v = c(2,2,5,5,6,8), "matrix")
pix_vec <- function(v = c(x0, y0, x1, y1, w, h), out = c("vector", "matrix")) {
  vec <- unlist(lapply(v[2]:v[4], function(i) (v[1]:v[3]) + (v[5] * (i - 1))))
  out <- out[1]

  if (out == "matrix") {
    mat <- integer(v[5] * v[6])
    mat[vec] <- 1
    mat <- matrix(mat, v[6], v[5], byrow = TRUE)
  }

  if (out == "vector") return(vec) else return(mat)

}


#' Calculates the overlap of rectangles in an image
#'
#' @param coord.list list of coordinates for the rectangles
#' @param compress number of compression
#'
#' @return
#' A dataframe
#' @export
#'
#' @examples
#' rect_overlap(coord.list = list(c(2,2,5,5), c(3,3,7,7), c(2,3,6,6)), compress = 0)
#'
#' # check correctness
#' c1 <- pix_vec(c(2,2,5,5,10))
#' c2 <- pix_vec(c(3,3,7,7,10))
#' length(which(c1 %in% c2)) / length(c1)
#'
rect_overlap <- function(
  coord.list = list(c(x01, y01, x11, y11), ..., c(x0n, y0n, x1n, y1n)), compress = 100
  ) {

  `%>%` <- magrittr::`%>%`
  if (compress == 0) compress <- 1

  # "compress" pixels by dividing
  coord.list <- relist(floor(unlist(coord.list) / sqrt(compress)), coord.list)

  # get minimum x- and y-position
  xmin <- min(sapply(coord.list, `[`)[1,])
  ymin <- min(sapply(coord.list, `[`)[2,])

  # 'move' pixels to the edges of the picture
  along <- seq_along(coord.list)
  l <- unlist(coord.list)
  l[along %% 2 == 1] <- l[along %% 2 == 1] - xmin + 1
  l[along %% 2 == 0] <- l[along %% 2 == 0] - ymin + 1
  coord.list <- relist(l, coord.list)

  # get maximum x- and y-position (after the adjustments above)
  x <- max(sapply(coord.list, `[`)[3,]) + 1
  y <- max(sapply(coord.list, `[`)[4,]) + 1

  # resulting matrix
  v <- logical(x * y)

  # apply pix_vec function over all coordinates
  m <- vapply(coord.list, function(i) {
    z <- v
    z[pix_vec(c(i, x))] <- TRUE
    return(z)
    }, FUN.VALUE = logical(x * y))

  # get sizes of the rectangles
  s <- apply(m, 2, function(x) length(which(x)))

  # get overlaps
  a <- vapply(1:ncol(m), function(x) {
    apply((m[, x] == m)[which(m[, x]),], 2, function(y) length(which(y))) / s[x]
    }, FUN.VALUE = numeric(ncol(m)))

  # in the rare cases vectors have the same length (not so rare if compressed)
  repeat {
    if (!any(duplicated(s)))
      break
    s[which(duplicated(s))] <- s[which(duplicated(s))] + 0.01
  }

  diag(a) <- NA
  table.size <- tibble::tibble(id = seq_along(s), size = s)

  table <- reshape2::melt(a) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(table.size, by = c("Var1" = "id")) %>%
    dplyr::left_join(table.size, by = c("Var2" = "id")) %>%
    dplyr::rename(rec1 = Var1, rec2 = Var2, size1 = size.x, size2 = size.y, ovl21 = value)


  # %>%
  #   dplyr::arrange(dplyr::desc(as.numeric(size.x > size.y)), dplyr::desc(size.x + size.y))
  #
  # table <- tibble::tibble(
  #   rec1  = table$Var1[1:(nrow(table) / 2)],
  #   rec2  = table$Var2[1:(nrow(table) / 2)],
  #   size1 = table$size.x[1:(nrow(table) / 2)],
  #   size2 = table$size.y[1:(nrow(table) / 2)],
  #   ovl21 = table$value[1:(nrow(table) / 2)],
  #   ovl12 = table$value[(nrow(table) / 2 + 1):nrow(table)]
  #   )

  gc()

  return(table)

}
