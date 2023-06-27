#' Sample k equally spaced points on a max-plus tropical line segment
#'
#' This function calculates k equally spaced points on a tropical line segment.
#'
#' @param D1 Point in the tropical projective torus
#' @param D2 Point in the tropical projective torus
#' @param k Number of points
#' @return A matrix of k equally spaced points on a tropical line segment.
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @export
#' @examples
#'D1 <-c(0,4,2)
#'D2 <- c(0,7,-1)
#'p<-Points.TLineSeg(D1, D2, k = 5)

Points.TLineSeg <- function(D1, D2, k = 20){
  d <- length(D1)
  k<-k+1
  x <- matrix(rep(0, d*(k)), k, d)
  if(length(D1) != length(D2))
    warning("dimension is wrong!")
  index <- order(D2 - D1)
  lambda <- (D2 - D1)[index]
  L <- lambda[d] - lambda[1]
  for(j in 1:(k))
    for(i in 1:d){
      x[j, i] <- max((lambda[1] + (j * L)/k) + D1[i], D2[i])
    }

  return(x[1:(nrow(x)-1),])
}
