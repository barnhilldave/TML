#' Tropical Kernel Density Estimation of Phylogenetic Trees.
#'
#' This function calculates a non-parametric density estimate of a tree over the space of phylogenetic trees on m leaves.
#' It mimics classical kernel density estimation by using a Gaussian kernel in conjunction with tropical distance.
#'
#' @param D Matrix of phylogenetic tree observations.
#' @param n Number of leaves for each tree.
#' @param sigma Bandwidth parameter based on tropical distance.
#' @param h Height of the tree
#' @return List containing center point and radius of minimum enclosing ball of P.
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @references Weyenberg, G., Huggins, P., Schardl, C., Howe, D. K., & Yoshida, R. (2014). kdetrees: Nonparametric Estimation of Phylogenetic Tree Distributions. In Bioinformatics.
#' @name tropical.KDE
#' @examples
#' ### bw.nn function: from archived KDETree package
#'bw.nn <- function(x,prop=0.2,tol=1e-6){
#'  out <- apply(x, 1, quantile, prop)
#'  is.zero <- out < tol
#'  if(sum(is.zero)>0)
#'    out[is.zero] <- apply(x[is.zero,],1,function(y) min(y[y>tol]))
#'  out
#'}
#'
#' ### Pairwise Distant between Ultrametrics.
#' pw.trop.dist <- function(D1, D2){
#' d1 <- dim(D1)
#' d2 <- dim(D2)
#' D <- matrix(rep(100000, d1[1]*d2[1]), d1[1], d2[1])
#' for(i in 1:d1[1])
#'   for(j in 1:d2[1])
#'     D[i, j] <- trop.dist(D1[i, ], D2[j, ])
#' return(D)
#' }

#' T1<-Sim_Trees1
#' T2<-Sim_Trees2
#' D <- rbind(T1, T2[1,])
#' T <- dim(D)[1]
#' X <- 1:T
#' M <- pw.trop.dist(D, D)

#' sigma <- bw.nn(M)

#' P_5 <- tropical.KDE(D, n, sigma, h = 2)

#' Q025 <- P_5[T]

#'@export
#'@rdname tropical.KDE
tropical.KDE <- function(D, n, sigma, h = 2){
  d <- dim(D)
  P <- rep(0, d[1]) ## estimated probability
  for(i in 1:d[1]){
    D[i, ] <- normaliz.tree(D[i, ], h)
    for(j in 1:d[1]){
      if(i != j)
        P[i] <- P[i] + kernel.ultrametric(D[i, ], D[j, ], sigma[j])
    }
    P[i] <- P[i]/(d[1] - 1)
  }
  return(P)
}





