#' K-means clustering over the tropical projective torus
#'
#' This function performs k-means clustering over the tropical projective torus
#'
#' @param A matrix of points defining a tropical polytope; rows are the tropical points
#' @param C number of clusters
#' @param M maximum number of iterations of algorithm to find cluster centroids
#' @return list with matrix of observation classified by centroid; matrix of centroid coordinates; number of iterations used
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over the Tropically Convex Sets.
#' @export
#' @examples
#' P <-Sim_points
#' C<-3
#' M<-10
#' res<-TKmeans(P,C,M)
#' try<-res[[1]]
#' cen<-res[[2]]
#' plot(try[,2],try[,3],col=try[,4],asp=1)
#' plot(try[,2],try[,3],col=try[,4],asp=1,xlab='x2',ylab='x3')
#' points(cen[,2],cen[,3],col=c('purple','hotpink','orange'),pch=19)

TKmeans<-function(A,C,M){
  or_cents<-matrix(0,C,dim(A)[2],TRUE)
  clus<-sample(1:C,nrow(A),TRUE)
  try<-cbind(A,clus)
  CL=FALSE
  ct<-0
  while(CL==FALSE|ct<=M){
    for (i in 1:C){
      c<-Trop_FW(try[which(try[,ncol(try)]==i),1:(ncol(try)-1)])
      or_cents[i,]<-c
    }
    try1<-try
    for (j in 1:nrow(try)){
      try1[j,ncol(try1)]<-which.min(apply(or_cents,1,function(x) trop.dist(x,A[j,])))
    }

    if(all(try[,ncol(try)]==try1[,ncol(try)])){
      try=try1
      CL=TRUE
      ct<-ct+1
    }
    else{
      try=try1
      ct<-ct+1
    }
  }
  RES<-list(try,or_cents,ct)
  return(RES)
}
