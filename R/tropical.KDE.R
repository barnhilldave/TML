#' Tropical Kernel Density Estimation of Phylogenetic Trees
#'
#' This function calculates a non-parametric density estimate of a tree over the space of phylogenetic trees on m leaves.
#' It mimics classical kernel density estimation by using a Gaussian kernel in conjunction with tropical distance.
#'
#' @param D matrix of phylogenetic tree observations as ultrametrics
#' @param n number of leaves for each tree
#' @param sigma bandwidth parameter based on tropical distance
#' @param h height of the tree
#' @return list containing center point and radius of minimum enclosing ball of P
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @references Weyenberg, G., Huggins, P., Schardl, C., Howe, D. K., & Yoshida, R. (2014). kdetrees: Nonparametric Estimation of Phylogenetic Tree Distributions. In Bioinformatics.
#' @references Yoshida, Ruriko, David Barnhill, Keiji Miura and Daniel Howe (2022). Tropical Density Estimation of Phylogenetic Trees.
#' @name tropical.KDE
#' @examples
#' \donttest{
#' T1<-Sim_Trees1
#' T2<-Sim_Trees2
#' D <- rbind(T1, T2[1,])
#' T <- dim(D)[1]
#' X <- 1:T
#' M <- pw.trop.dist(D, D)
#' sigma <- bw.nn(M)
#' P_5 <- tropical.KDE(D, n, sigma, h = 2)
#' Q5 <- P_5[T]
#' }

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





