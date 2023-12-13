#' Create a phylogenetic tree from an ultrametric
#'
#' This function constructs a phylogenetic tree from an ultrametric.
#'
#' @param u is an ultrametric
#' @param n is the number of leaves
#' @param L is a vector of labels (strings) of leaves
#' @return A phylogenetic tree of class \code{phylo}
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @export
#' @examples
#' um<-Sim_Trees21[1,]
#' ll <- 10
#' L <- LETTERS[1:10]
#' tr<-convert.to.tree(ll, L, um)
convert.to.tree <- function(n, L, u){
  DDD <- matrix(rep(0,n*n), nrow=n)
  DDD[lower.tri(DDD)] <- u
  DDD[upper.tri(DDD)]  <- t(DDD)[upper.tri(DDD)]
  colnames(DDD) <- L
  rownames(DDD) <- L
  tree <- upgma(DDD)
  return(tree)
}
