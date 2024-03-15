#' Ratio of within and between tropical measures for k-means clusters
#'
#' Ratio of within and between cluster tropical measures for k-means derived
#' clusters
#' @param A matrix of tropical points; rows are points
#' @param C number of clusters
#' @param method method to use for within cluster measure; "avg" or "max"
#' @return vector of ratios for each cluster
#' @references David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over
#'   the Tropically Convex Sets.
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#'
#' hars<-Sim_points
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
