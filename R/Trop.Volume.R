#' Estimate the volume of a tropical polytope
#'
#' This function uses tropical HAR with a uniform target distribution to estimate the volume of a tropical polytope
#'
#' @param B matrix of points defining a minimum enclosing ball for a polytope P; rows are the points
#' @param P matrix of points defining a tropical polytope; rows are the points
#' @param x0 initial point used for the HAR sampler
#' @param s number of points to sample from the minimum enclosing ball
#' @param I number of iterations for the HAR sampler
#' @param r radius of the minimum enclosing tropical ball
#' @return list containing ratio of points falling in P; volume of the tropical ball; volume estimate of P
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references Barnhill, David, Ruriko Yoshida and Keiji Miura (2023). Maximum Inscribed and Minimum Enclosing Tropical Balls of Tropical Polytopes and Applications to Volume Estimation and Uniform Sampling.
#' @export
#' @examples
#' P <-matrix(c(0,0,0,0,3,1,0,2,5),3,3,TRUE)
#' BR<-min_enc.ball(P)
#' B<-trop.bal.vert(BR[[1]],BR[[2]])
#' x0<-c(0,1.5,.4)
#' S<-200
#' I<-50
#' R<-BR[[2]]
#' trop.Volume(B,P,x0,S,I,R)

trop.Volume<-function(B,P,x0,s,I,r){
  count<-0
  d<-ncol(P)
  for (i in (1:s)){
    x<-VE.HAR(B, x0, I)
    proj<-project.pi(P,x)
    if(trop.dist(x,proj)<=1e-8){
      count<-count+1
    }
    x0<-x
  }
  rr<-count/s
  VolB<-(d)*r^(d-1)
  VolP<-rr*VolB
  return(list(Ratio=rr,Vol_Ball=VolB,Vol_Poly=VolP))
}
