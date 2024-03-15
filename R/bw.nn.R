#' Nearest neighbor bandwidth calculation
#'
#' This function finds the bandwidth for an ultrametric based on the tropical
#' distance of the nearest point. The function provides the bandwidth input to
#' trop.KDE and was originally used in the KDETrees package.
#'
#' @param x matrix; dissimilarity matrix between points in a data set
#' @param prop proportion of observations that defines neighborhood of a point
#' @param tol tolerance for zero bandwidth check
#' @return a vector of bandwidths for each tree (row) in x
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @references Weyenberg, G., Huggins, P., Schardl, C., Howe, D. K., & Yoshida,
#'   R. (2014). kdetrees: Nonparametric Estimation of Phylogenetic Tree
#'   Distributions. In Bioinformatics.
#' @references \url{https://github.com/grady/kdetrees/blob/master/R/bw.R}
#' @export
#' @examples
#' \donttest{
#' T1<-Sim_Trees15
#' T2<-Sim_Trees25
#' D <- rbind(T1, T2[1,])
#' M <- pw.trop.dist(D, D)
#' bw.nn(M)
#' }
bw.nn <- function(x,prop=0.2,tol=1e-6){
    out <- apply(x, 1, quantile, prop)
    is.zero <- out < tol
    if(sum(is.zero)>0)
      out[is.zero] <- apply(x[is.zero,],1,function(y) min(y[y>tol]))
    out
  }






