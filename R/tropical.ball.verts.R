#' Calculate the minimum generating vertex set of a tropical ball.
#'
#' This function calculates the coordinates of the minimum vertex set of a tropical ball given a center point.
#'
#' @param x Matrix where each row is a point defining a tropical polytope
#' @param d radius of the tropical ball in terms of tropical distance
#' @return square matrix of normalized tropical points defining the tropical ball. Rows are the points.
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#'x <-c(0,3,7)
#'d <- 2
#'trop_bal.vert(x,d)
trop_bal.vert<-function(x,d){
  dm<-length(x)
  A<-matrix(0,dm,dm,TRUE)
  i=1
  while (i < dm){
    i=i+1
    a<-c(rep(0,dm))
    a[i]<-d
    A[i,]<-x+a
  }
  A[1,]<-x-c(0,rep(d,(dm-1)))
  return(A)
}
