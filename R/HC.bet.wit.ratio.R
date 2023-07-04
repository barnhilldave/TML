#' Ratio of within and between tropical measures for tropical hierarchical clusters
#'
#' Ratio of within and between cluster tropical measures for a set hierarchical clusters
#' @param A matrix of tropical points; rows are points
#' @param V list of clusters where each cluster is defined as a matrix
#' @param method method to use for within cluster measure; "avg" or "max"
#' @return vector of ratios for each cluster
#' @references David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over the Tropically Convex Sets.
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' set.seed(125)
#' P<-matrix(c(0,-100,-100,0,100,0,0,0,100),3,3,TRUE)
#' x0<-c(0,0,0)
#' y0<-c(0,0,0)
#' mu1<-c(0,-10,-20)
#' sig1<-5
#' mu2<-c(0,20,30)
#' sig2<-8
#' N<-20
#' har_norms1<-matrix(0,N,3)
#' har_norms2<-matrix(0,N,3)
#'
#' for(i in 1:N){
#'   print(i)
#'   x<-TropicalPolytope.extrapolation.HAR_NORM(P, x0, I = 50,mu1,sig1)
#'   y<-TropicalPolytope.extrapolation.HAR_NORM(P, y0, I = 50,mu2,sig2)
#'   x0<-x
#'   y0<-y
#'   har_norms1[i,]<-x
#'   har_norms2[i,]<-y
#' }
#'
#' hars<-rbind(har_norms1,har_norms2)
#' V<-Tropical.HC.AGNES(hars, method="average")
#' inds<-V[[2]][[38]]
#' cluster.ratio_HC(hars,inds,method='avg')

cluster.ratio_HC<-function(A,V,method='avg'){
  wits<-c()
  for (i in 1:length(V)){
    if(method=='avg'){
      wit<-trop_wi_dist(A[V[[i]],],method='avg')
      wits<-append(wits,wit)
    }
    else{
      wit<-trop_wi_dist(A[V[[i]],],method='max')
      wits<-append(wits,wit)
    }
  }
  bets<-over_bet_HC(A,V)
  rats<-c()
  for(i in 1:length(V)){
    bet_p<-bets[-i]
    rat<-wits[i]/mean(bet_p)
    rats<-append(rats,rat)
  }
  return(rats)
}
