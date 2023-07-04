#' Tropical cluster betweeness measure for each cluster in a set of hierarchical clusters
#'
#' This function calculates an overall betweenness measure based on tropical distance between a set of clusters derived from tropical hierarchical clustering
#' @param A matrix of tropical points; rows are points with the last column representing a numbered cluster assignment
#' @param V list of clusters defined as matrices derived from agglomerative or divisive hierarchical clustering
#' @return vector of betweenness cluster measures
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
#' over_bet_HC(hars,inds)

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




