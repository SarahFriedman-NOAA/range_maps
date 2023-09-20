# function to estimate break point for different data densities
getLevel <- function(x, y, prob) { 
  kk <- MASS::kde2d(x,y)
  dx <- diff(kk$x[1:2])
  dy <- diff(kk$y[1:2])
  sz <- sort(kk$z)
  c1 <- cumsum(sz) * dx * dy
  approx(c1, sz, xout = 1 - prob)$y
}


# remove outliers from dataset
remove_outliers <- function(data){
  if (nrow(data) > 3) {
    clustering <- dbscan::dbscan(data[, c("lon", "lat")],
                                 eps = 7, minPts = 4, borderPoints = FALSE
    )
    out <- data %>%
      dplyr::mutate(
        cluster = clustering$cluster,
        outlier = ifelse(cluster == 0, T, F)
      ) %>%
      dplyr::filter(!outlier)
    if (nrow(out) == 0) {
      out <- data
    }
  } else {
    out <- data
  }
  out
}
