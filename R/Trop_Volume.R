#' Estimate the volume of a tropical polytope
#'
#' This function uses tropical HAR with a uniform target distribution to estimate the volume of a tropical polytope
#'
#' @param B matrix of points defining a minimum enclosing ball for a polytope P; rows are the points
#' @param P matrix of points defining a tropical polytope; rows are the points
#' @param x0 initial point used for the HAR sampler
#' @param S number of points to sample from the minimum enclosing ball
#' @param I number of iterations for the HAR sampler
#' @param R radius of the minimum enclosing tropical ball
#' @return list containing ratio of points falling in P; volume of the tropical ball; volume estimate of P
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references Barnhill, David, Ruriko Yoshida and Keiji Miura (2023). Maximum Inscribed and Minimum Enclosing Tropical Balls of Tropical Polytopes and Applications to Volume Estimation and Uniform Sampling.
#' @export
#' @examples
#' P <-matrix(c(0,0,0,0,3,1,0,2,5),3,3,TRUE)
#' BR<-min_enc_ball(P)
#' B<-trop_bal.vert(BR[[1]],BR[[2]])
#' x0<-c(0,1.5,.4)
#' S<-500
#' I<-50
#' R<-BR[[2]]
#' Trop_Volume(B,P,x0,S,I,R)

Trop_Volume<-function(B,P,x0,S,I,R){
  count<-0
  d<-ncol(P)
  #har_points<-matrix(0,S,d,TRUE)
  #har_points1<-matrix(0,0,d)
  for (i in (1:S)){
    print(i)
    x<-TropicalPolytope.extrapolation.HAR(B, x0, I)
    proj<-project_pi(P,x)
    if(trop.dist(x,proj)<=1e-8){
      count<-count+1
      #har_points1<-rbind(har_points1,x)
    }
    #har_points[i,]<-x
    x0<-x
  }
  r<-count/S
  VolB<-(d)*R^(d-1)
  VolP<-r*VolB
  return(list(r,VolB,VolP))
}
