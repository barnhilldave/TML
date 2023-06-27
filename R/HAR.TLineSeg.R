#' Uniformly sample from a max-plus tropical line segment
#'
#' This function uses a hit-and-run sampler to uniformly sample from a max-plus tropical line segment.
#'
#' @param D1 Point in the tropical projective torus
#' @param D2 Point in the tropical projective torus
#' @return A point on the line segment defined by D1 and D2
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @export
#' @examples
#'D1 <-c(0,4,2)
#'D2 <- c(0,7,-1)
#'HAR.TLineSeg(D1, D2)

HAR.TLineSeg <- function(D1, D2){
  d <- length(D1)
  if(length(D1) != length(D2))
    warning("dimension is wrong!")
  index <- order(D2 - D1)
  lambda <- (D2 - D1)[index]
  l <- runif(1, min=lambda[1], max=lambda[d])
  x1 <- rep(0, d)

  for(i in 1:d){
    x1[i] <- max(l + D1[i], D2[i])
  }

  return(x1)
}
