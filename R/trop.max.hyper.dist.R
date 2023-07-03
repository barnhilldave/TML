#' Calculate the tropical distance to a max-tropical hyperplane
#'
#' Calculate the tropical distance to a max-tropical hyperplane
#'
#' @param O normal vector of a tropical hyperplane; numeric vector
#' @param x0 point of interest; numeric vector
#' @return tropical distance to max-plus tropical hyperplane
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' O <-c(0,-1,-1)
#' x0 <- c(0,-2,-8)
#' trop.dist.hyp_max(O,x0)

#### Calculate distance from a point to a max hyperplane ####
trop.dist.hyp_max<-function(O,x0){
  x<-O+x0
  x_prime<-x[order(x,decreasing = TRUE)]
  dist<-x_prime[1]-x_prime[2]
  return(dist)
}


