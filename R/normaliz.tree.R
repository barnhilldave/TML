#' Normalize a phylogenetic tree
#'
#' This function normalizes the height of a phylogenetic tree.
#'
#' @param D Phylogenetic tree
#' @param h Desired height.  Defaults to 1.
#' @return Normalized equidistant tree
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @export
#' @examples
#'D <-c(4,4,2)
#'normaliz.tree(D, h=1)

normaliz.tree <- function(D, h = 1){
  d <- length(D)
  a <- max(D) - h
  x <- D - rep(a, d)
  for(i in 1:d)
    if(x[i] < 0)
      x[i] <- 0
  return(x)
}
