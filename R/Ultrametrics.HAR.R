#' Hit-and-Run Sampler for the space of ultrametrics
#'
#' This sampler samples a point in the space of ultrametrics where each point represents an equidistant tree on n leaves
#'
#' @param x0 an equidistant tree defined as ultrametric
#' @param n number of leaves for the equidistant tree
#' @param I number of states in the Markov chain
#' @param h height of phylogenetic tree
#' @return point in the space of ultrametrics over n leaves
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @references Yoshida, Ruriko, Keiji Miura and David Barnhill (2022). Hit and Run Sampling from Tropically Convex Sets.
#' @export
#' @examples
#'x0 <-Sim_Trees1[1,]
#'n<-10
#'
#'Ultrametrics.HAR(x0, n, I = 50, h = 1)
#'

Ultrametrics.HAR <- function(x0, n, I = 1, h = 1){
  d <- length(x0)
  x <- normaliz.tree(x0, h)
  a <- h

  for(i in 1:I){
    x1 <- normaliz.tree(x, h)
    D0 <- symMatrix(runif(choose((n+1), 2), 0, a), n)
    for(k in 1:n)
      D0[k, k] <- 0
    tree <- upgma(D0)
    #tree <- rcoal(n)
    tree$edge.length <- tree$edge.length/max(tree$edge.length)
    D <- cophenetic(tree)
    v <- D[lower.tri(t(D))] #rep(0, d)
    x <- HAR.TLineSeg(normaliz.tree(x1, h), normaliz.tree(v, h))
  }

  return(normaliz.tree(x, h))
}


