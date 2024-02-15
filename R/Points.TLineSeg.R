#' Sample k equally spaced points on a max- or min-plus tropical line segment
#'
#' This function calculates k equally spaced points on a tropical line segment
#'
#' @param D1 point in the tropical projective torus
#' @param D2 point in the tropical projective torus
#' @param k number of points
#' @param add string; 'max' indicates max-plus addition, 'min' indicates min-plus addition. Defaults to 'max'.
#' @return matrix of k equally spaced points on a tropical line segment
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @export
#' @examples
#'D1 <-c(0,4,2)
#'D2 <- c(0,7,-1)
#'p<-Points.TLineSeg(D1, D2, k = 5)
#'p<-Points.TLineSeg(D1, D2, k = 5,add='min')

Points.TLineSeg <- function(D1, D2, k = 20,add='max'){
  d <- length(D1)
  k<-k+1
  x <- matrix(rep(0, d*(k)), k, d)
  if(length(D1) != length(D2))
    warning("dimension is wrong!")


  if(add=='max'){
    index <- order(D2 - D1)
    lambda <- (D2 - D1)[index]
    L <- lambda[d] - lambda[1]
  for(j in 1:(k))
    for(i in 1:d){
      x[j, i] <- max((lambda[1] + (j * L)/k) + D1[i], D2[i])
    }
    }
  if(add=='min'){
    index <- order(D2 - D1,decreasing = TRUE)
    lambda <- (D2 - D1)[index]
    L <- lambda[d] - lambda[1]
    for(j in 1:(k))
      for(i in 1:d){
        x[j, i] <- min((lambda[1] + (j * L)/k) + D1[i], D2[i])
      }
  }
  return(x[1:(nrow(x)-1),])
}
