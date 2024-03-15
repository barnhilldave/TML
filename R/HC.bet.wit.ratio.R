#' Ratio of within and between tropical measures for tropical hierarchical
#' clusters
#'
#' Ratio of within and between cluster tropical measures for a set hierarchical
#' clusters
#' @param A matrix of tropical points; rows are points
#' @param V list of clusters where each cluster is defined as a matrix
#' @param method method to use for within cluster measure; "avg" or "max"
#' @return vector of ratios for each cluster
#' @references David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over
#'   the Tropically Convex Sets.
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' har<-rbind(Sim_points[1:20,],Sim_points[51:70,])
#'
#' V<-Tropical.HC.AGNES(har, method="average")
#' inds<-V[[2]][[38]]
#' cluster.ratio_HC(har,inds,method='avg')

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
