#' Tropical cluster betweeness measure for each cluster in a set of hierarchical
#' clusters
#'
#' This function calculates an overall betweenness measure based on tropical
#' distance between a set of clusters derived from tropical hierarchical
#' clustering
#' @param A matrix of tropical points; rows are points with the last column
#'   representing a numbered cluster assignment
#' @param V list of clusters defined as matrices derived from agglomerative or
#'   divisive hierarchical clustering
#' @return vector of betweenness cluster measures
#' @references David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over
#'   the Tropically Convex Sets.
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' har<-rbind(Sim_points[1:20,],Sim_points[51:70,])
#'
#' V<-Tropical.HC.AGNES(har, method="average")
#' inds<-V[[2]][[38]]
#' over_bet_HC(har,inds)

over_bet_HC<-function(A,V){
  bets<-c()
  for (i in 1:length(V)){
    bet<-c()
    for(j in 1:length(V)){
      if(i!=j){
        bet1<-mean(c(trop_bet_dist(A[V[[i]],],A[V[[j]],]),trop_bet_dist(A[V[[j]],],A[V[[i]],])))
        bet<-append(bet,bet1)
      }
    }
    mbet<-mean(bet)
    bets<-append(bets,mbet)
  }
  return(bets)
}




