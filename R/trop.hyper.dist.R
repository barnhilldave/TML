#' Calculate the tropical distance to a max-tropical hyperplane
#'
#' Calculate the tropical distance to a max-tropical hyperplane
#'
#' @param O normal vector of a tropical hyperplane; numeric vector
#' @param x0 point of interest; numeric vector
#' @param tadd function; max indicates max-plus addition, min indicates
#'   min-plus addition. Defaults to max
#' @return tropical distance to max-plus tropical hyperplane
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' O <-c(0,-1,-1)
#' x0 <- c(0,-2,-8)
#' trop.hyper.dist(O,x0)
#' trop.hyper.dist(O,x0,tadd=min)

#### Calculate distance from a point to a max hyperplane ####
trop.hyper.dist<-function(O,x0,tadd=max){
  sign <- if (identical(tadd, base::max))  +1 else -1
  decreasing <- identical(tadd, base::max)

  x<-O+x0
  x_prime<-x[order(x,decreasing = decreasing)]
  dist<-sign*(x_prime[1]-x_prime[2])

  return(dist)
}

