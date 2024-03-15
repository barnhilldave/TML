#' Calculate a minimum enclosing ball for a tropical polytope
#'
#' This function constructs a minimum enclosing ball for a set of points
#' defining a tropical polytope.
#'
#' @param A matrix of points defining a tropical polytope. Rows are the points.
#' @return list containing center point and radius of minimum enclosing ball of
#'   P
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references Barnhill, David, Ruriko Yoshida and Keiji Miura (2023). Maximum
#'   Inscribed and Minimum Enclosing Tropical Balls of Tropical Polytopes and
#'   Applications to Volume Estimation and Uniform Sampling.
#' @export
#' @examples
#' P <-matrix(c(0,0,0,0,3,1,0,2,5),3,3,TRUE)
#' min_enc.ball(P)

min_enc.ball<-function(A){
  P<-permutations(ncol(A),2)
  V<-matrix(0,0,ncol(A))
  bb<-c()
  for (j in 1:nrow(P)){
    for (i in 1:nrow(A)){
      k<-A[i,P[j,1]]-A[i,P[j,2]]
      bb<-append(bb,k)
      a<-rep(0,ncol(A))
      a[P[j,1]]<-1
      a[P[j,2]]<--1
      V<-rbind(V,a)
    }
  }
  r<-rep(-1,nrow(V))
  V<-cbind(V,r)
  f.obj<-c(rep(0,ncol(A)),1)
  f.con<-V
  f.dir <- rep("<=",nrow(V))
  f.rhs<-bb

  res<-lp ("min", f.obj, f.con, f.dir, f.rhs)
  sol<-res$solution
  cent<-sol[1:ncol(A)]
  rad<-sol[length(sol)]
  return(list(Center=cent,Radius=rad))
}
