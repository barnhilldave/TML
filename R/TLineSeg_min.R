#' Construct a min-plus tropical line segment accor
#'
#' This function constructs a min-plus tropical line segment between two points.
#'
#' @param D1 Point in the tropical projective torus
#' @param D2 Point in the tropical projective torus
#' @return A list of points defining the tropical line segment.
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#'D1 <-c(0,4,2)
#'D2 <- c(0,7,-1)
#'TLineSeg_min(D1, D2)

TLineSeg_min <- function(D1, D2){
  d <- length(D1)
  if(length(D1) != length(D2))
    warning("dimension is wrong!")
  index <- order(D2 - D1,decreasing = TRUE)
  lambda <- (D2 - D1)[index]
  x1 <- rep(0, d)
  segment <- list()
  for(j in 1:d){
    for(i in 1:d){
      x1[i] <- 0
      x1[i] <- min(lambda[j] + D1[i], D2[i])
    }
    segment[[j]] <- normaliz.vector(x1)
  }
  return(segment)
}
