#' Remove all tentacles from a tropical simplex.
#'
#' This function removes all tentacles from a tropical simplex. The remaining portion is a full-dimensional tropical polytope known as the trunk of the tropical polytope.
#'
#' @param A Matrix of points defining a tropical simplex. Rows are the points.
#' @return Matrix of points defining only the full-dimensional element (the trunk) of a tropical polytope. Rows are points.
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' P<-matrix(c(0,-1,1,0,0,0,0,1,-1),3,3,TRUE)
#' BP<-min_enc_ball(P)
#' RP<-rounding(P)
#' BRP<-min_enc_ball(RP)

############Rounding Algorithm for a tropical polytope###################
## Author :  David Barnhill                                            ##
## Date   :  January 8th 2023                                          ##
## Program:  This code enumerates the pseudo-vertices of the boundary  ##
##           of a full-dimensional sector of a tropical polytope.      ##
## Input  :  A matrix, V, of vertices defining the tropical convex hull##
##           of a polytope P.                                          ##
## Output :  matrix of pseudo-vertices for the full dimensional part   ##
##           of a tropical polytope.                                   ##
## Execute:  type in R as                                              ##
#########################################################################

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
