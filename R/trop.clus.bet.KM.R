#' Tropical cluster betweeness measure for a each of a set of k-means derived
#' set of clusters
#'
#' This function calculates an overall betweenness measure between a set of
#' clusters derived from tropical k-means clustering
#' @param A matrix of tropical points; rows are points with the last column
#'   representing a numbered cluster assignment
#' @param C number of clusters
#' @return betweenness cluster measure
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
#' over_bet_KM(cl_pt,C)

over_bet_KM<-function(A,C){
  bets<-c()
  for (i in 1:C){
    bet<-c()
    for(j in 1:C){
      if(i!=j){
        bet1<-mean(c(trop_bet_dist(A[which(A[,ncol(A)]==i),1:(ncol(A)-1)],A[which(A[,ncol(A)]==j),1:(ncol(A)-1)]),trop_bet_dist(A[which(A[,4]==j),1:(ncol(A)-1)],A[which(A[,4]==i),1:(ncol(A)-1)])))
        bet<-append(bet,bet1)
      }
    }
    mbet<-mean(bet)
    bets<-append(bets,mbet)
  }
  return(bets)
}
