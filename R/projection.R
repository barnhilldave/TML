#' Project a point on the tropical projective torus onto a tropical polytope.
#'
#' This function projects points in the tropical projective torus onto a tropical polytope based on tropical distance
#'
#' @param D_s matrix where each row is a point defining a tropical polytope
#' @param D point to be projected onto D_s
#' @return projection of point D onto the tropical polytope defined by D_s
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#'D_s <-matrix(c(0,0,0,0,2,5,0,3,1),3,3,TRUE)
#'D <- c(0,7,-1)
#'project_pi(D_s,D)

project_pi<-function(D_s,D){
  d <- dim(D_s)
  lambda <- rep(0, d[1])
  for(i in 1:d[1])
    lambda[i] <- min(D - D_s[i,])

  x <- rep(0, d[2])
  for(i in 1:d[2])
    x[i] <- max((lambda+D_s)[, i])
  return(normaliz.vector(x))
}
