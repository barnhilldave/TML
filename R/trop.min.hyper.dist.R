#' Calculate the tropical distance to a min-tropical hyperplane
#'
#' Calculate the tropical distance to a min-tropical hyperplane.
#'
#' @param O normal vector of a tropical hyperplane; numeric vector
#' @param x0 point of interest; numeric vector
#' @return tropical distance to min-plus tropical hyperplane
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' O <-c(0,-1,-1)
#' x0 <- c(0,-2,-8)
#' trop.dist.hyp_min(O,x0)

trop.dist.hyp_min<-function(O,x0){
  x<-O+x0
  x_prime<-x[order(x,decreasing = FALSE)]
  (dist<-x_prime[2]-x_prime[1])
  return(dist)
}
