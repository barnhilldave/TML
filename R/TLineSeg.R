#'Construct a max- or min-plus tropical line segment between two points
#'
#'This function constructs a max-plus tropical line segment between two points
#'
#'@param D1 point in the tropical projective torus
#'@param D2 point in the tropical projective torus
#'@param tadd function; max indicates max-plus addition, min indicates min-plus
#'  addition. Defaults to max
#'@return list of points defining the tropical line segment
#'@author Ruriko Yoshida \email{ryoshida@@nps.edu}
#'@export
#' @examples
#'D1 <-c(0,4,2)
#'D2 <- c(0,7,-1)
#'TLineSeg(D1, D2)
#'TLineSeg(D1, D2,tadd=min)

TLineSeg <- function(D1, D2,tadd=max){
  d <- length(D1)
  if(length(D1) != length(D2))
    warning("dimension is wrong!")

  decreasing <- identical(tadd, base::min)

  index <- order(D2 - D1,decreasing=decreasing)
  lambda <- (D2 - D1)[index]

  x1 <- rep(0, d)
  segment <- list()
  for(j in 1:d){
    for(i in 1:d){
      x1[i] <- 0
      x1[i] <- tadd(lambda[j] + D1[i], D2[i])
    }
    segment[[j]] <- normaliz.vector(x1)
  }

  return(segment)
}

