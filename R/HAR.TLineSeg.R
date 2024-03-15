#'Uniformly sample from a max-plus tropical line segment
#'
#'This function uses a hit-and-run sampler to uniformly sample from a max-plus
#'tropical line segment
#'
#'@param D1 point in the tropical projective torus
#'@param D2 point in the tropical projective torus
#'@param add string; 'max' indicates max-plus addition, 'min' indicates min-plus
#'  addition. Defaults to 'max'
#'@return point on the line segment defined by D1 and D2
#'@author Ruriko Yoshida \email{ryoshida@@nps.edu}
#'@references Yoshida, Ruriko, Keiji Miura and David Barnhill (2022). Hit and
#'  Run Sampling from Tropically Convex Sets.
#'@export
#' @examples
#'D1 <-c(0,4,2)
#'D2 <- c(0,7,-1)
#'HAR.TLineSeg(D1, D2,add='max')
#'#'HAR.TLineSeg(D1, D2,add='min')

HAR.TLineSeg <- function(D1, D2,add='max'){
  d <- length(D1)
  if(length(D1) != length(D2))
    warning("dimension is wrong!")
  if(add=='max'){
    index <- order(D2 - D1)
    lambda <- (D2 - D1)[index]
    l <- runif(1, min=lambda[1], max=lambda[d])
    x1 <- rep(0, d)

    for(i in 1:d){
      x1[i] <- max(l + D1[i], D2[i])
    }
  }
  if(add=='min'){
    index <- order(D2 - D1,decreasing=TRUE)
    lambda <- (D2 - D1)[index]
    l <- runif(1, min=lambda[d], max=lambda[1])
    x1 <- rep(0, d)

    for(i in 1:d){
      x1[i] <- min(l + D1[i], D2[i])
    }
  }

  return(x1)
}
