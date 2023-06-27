#' Gaussian-like Sampling on a max-plus tropical line segment.
#'
#' This function samples points on a tropical line segment about a location parameter for a given scale parameter defined in terms of tropical distance.
#'
#' @param D1 Point in the tropical projective torus
#' @param D2 Point in the tropical projective torus
#' @param mu Location parameter
#' @param stdev Scale parameter
#' @return Point on the line segment defined by D1 and D2 sampled about mu
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#'D1 <-c(0,4,2)
#'D2 <- c(0,7,-1)
#'mu<-c(0,7,2)
#'sd<-1
#'HAR.TLineSeg.Norm(D1, D2,mu,sd)
#'

HAR.TLineSeg.Norm <- function(D1, D2,mu,stdev){
  d <- length(D1)
  D<-rbind(D1,D2)
  if(length(D1) != length(D2))
    warning("dimension is wrong!")
  proj_mu<-project_pi(D,mu)
  l0<-trop.dist(D1,proj_mu)+min(D1-D2)
  pro_true<-FALSE
  while(pro_true==FALSE){
    l <- rnorm(1, mean = 0, sd=stdev)
    x_p <- rep(0, d)
    for(i in 1:d){
      x_p[i] <-max(((l0+l) + D2[i]), D1[i])
    }
    pro_x<-project_pi(D,x_p)
    dist_pro<-trop.dist(x_p,pro_x)
    if(dist_pro<=1e-8){
      x1<-normaliz.vector(x_p)
      pro_true<-TRUE
    }
  }
  return(x1)
}
