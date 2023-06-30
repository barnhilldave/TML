#' Divisive (DIANA) tropical hierarchical clustering
#'
#' This function performs divisive (DIANA) hierarchical clustering over the space of ultrametrics defining the space of equidistant trees.
#'
#' @param D Matrix of ultrametrics.  Rows are the tropical points.
#' @param D1 Subset of D
#' @param D2 Subset of D
#' @param n Number of leaves.
#' @param k Number of clusters to partition data.
#' @return Value
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' set.seed(123)
#' index.1 <- sample(nrow(Sim_Trees1),30)
#' index.2 <- sample(nrow(Sim_Trees2),30)
#' T1<-Sim_Trees1[index.1,]
#' T2<-Sim_Trees2[index.2,]
#' T<-rbind(T1,T2)
#' n=10
#' Tropical.HC.DIANA(T,T1,T2, n,k=2)

Tropical.HC.DIANA <- function(D, D1, D2, n, k=2){
  d <- dim(D)
  x <- 0
  M <- matrix(rep(0, d[1]*d[1]), d[1], d[1])
  for(i in 1:d[1])
    for(j in 1:d[1])
      M[i, j] <- trop.dist(D[i, ], D[j, ])
  cl <- diana(M, diss=TRUE)
  grp <- cutree(cl, k = k)
  x <- (sum(grp[1:nrow(D1)]==grp[1])+sum(grp[(nrow(D1)+1):nrow(D)]==grp[(nrow(D1)+1)]))/nrow(D)
  if(x < 0.5)
    x <- 1 - x
  return(x)
}
