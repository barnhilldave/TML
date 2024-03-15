#' Remove all tentacles from a tropical simplex
#'
#' This function removes all tentacles from a tropical simplex. The remaining
#' portion is a full-dimensional tropical polytope known as the trunk of the
#' tropical polytope.
#'
#' @param P matrix of points defining a tropical simplex. Rows are the points
#' @return matrix of points defining only the full-dimensional element (the
#'   trunk) of a tropical polytope; rows are points
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references Barnhill, David, Ruriko Yoshida and Keiji Miura (2023). Maximum
#'   Inscribed and Minimum Enclosing Tropical Balls of Tropical Polytopes and
#'   Applications to Volume Estimation and Uniform Sampling.
#' @export
#' @examples
#' P<-matrix(c(0,-1,1,0,0,0,0,1,-1),3,3,TRUE)
#' BP<-min_enc.ball(P)
#' RP<-rounding(P)
#' BRP<-min_enc.ball(RP)

rounding<-function(P){
  PP_star<-tdets(P)
  PP<-PP_star[[2]]
  for (i in 1:ncol(PP)){
    PP[,i]<-PP[,i]-PP[i,i]
  }
  b1<-rep(0,(nrow(PP)*(nrow(PP)-1)))
  k<-1
  for (i in 1:nrow(PP)){
    for(j in 1:ncol(PP)){
      if (i!=j){
        b1[k]<-PP[i,j]
        k=k+1}
    }
  }

  B<-matrix(NA,0,ncol(P))
  for (i in 1:nrow(PP)){
    p<-rep(0,ncol(PP))
    for (j in 1:ncol(PP)){
      pt<-p
      if(i==j) next
      if (j!=i){}
      pt[i]=1
      pt[j]=-1
      B<-rbind(B,pt)
    }
  }
  H<-makeH(-B[,-1],-b1)
  V<-scdd(H)
  V1<-cbind(rep(0,nrow(V$output[,c(3:ncol(V$output))])),V$output[,c(3:ncol(V$output))])
  return(V1)
}
