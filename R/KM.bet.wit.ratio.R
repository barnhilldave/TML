#' Ratio of within and between tropical measures for k-means clusters
#'
#' Ratio of within and between cluster tropical measures for k-means derived clusters
#' @param A matrix of tropical points; rows are points
#' @param C number of clusters
#' @param method method to use for within cluster measure; "avg" or "max"
#' @return vector of ratios for each cluster
#' @references David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over the Tropically Convex Sets.
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' P<-matrix(c(0,-100,-100,0,100,0,0,0,100),3,3,TRUE)
#' x0<-c(0,0,0)
#' y0<-c(0,0,0)
#' z0<-c(0,0,0)
#' mu1<-c(0,-10,-20)
#' sig1<-5
#' mu2<-c(0,20,30)
#' sig2<-8
#' mu3<-c(0,30,10)
#' sig3<-3
#' N<-50
#' har_norms1<-matrix(0,N,3)
#' har_norms2<-matrix(0,N,3)
#' har_norms3<-matrix(0,N,3)
#'
#' for(i in 1:N){
#'   print(i)
#'   x<-TropicalPolytope.extrapolation.HAR_NORM(P, x0, I = 50,mu1,sig1)
#'   y<-TropicalPolytope.extrapolation.HAR_NORM(P, y0, I = 50,mu2,sig2)
#'   z<-TropicalPolytope.extrapolation.HAR_NORM(P, z0, I = 50,mu3,sig3)
#'   x0<-x
#'   y0<-y
#'   z0<-z
#'   har_norms1[i,]<-x
#'   har_norms2[i,]<-y
#'   har_norms3[i,]<-z
#' }
#'
#' hars<-rbind(har_norms1,har_norms2,har_norms3)
#' cls<-c(rep(1,50),rep(2,50),rep(3,50))
#' cl_pt<-cbind(hars,cls)
#'
#' C<-3
#' cluster.ratio_KM(cl_pt,C,method='avg')

cluster.ratio_KM<-function(A,C,method='avg'){
  wits<-c()
  for (i in 1:C){
    if(method=='avg'){
      wit<-trop_wi_dist(A[which(A[,ncol(A)]==i),1:(ncol(A)-1)],method='avg')
      wits<-append(wits,wit)
    }
    else{
      wit<-trop_wi_dist(A[which(A[,ncol(A)]==i),1:(ncol(A)-1)],method='max')
      wits<-append(wits,wit)
    }
  }
  bets<-over_bet_KM(A,C)
  rats<-c()
  for(i in 1:C){
    bet_p<-bets[-i]
    rat<-wits[i]/mean(bet_p)
    rats<-append(rats,rat)
  }
  return(rats)
}
