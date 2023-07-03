#' Find the best pair of clusters
#'
#' Find the best pair of clusters to merge based on tropical linkage method
#'
#' @param D1 list of Matrices
#' @param method linkage method.
#' @return value of the associated linkage method
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @noRd

finding.pair <- function(D1, method="average"){
  ## D1 is a list of matrices.
  ### method is one of "average", "min", "max"
  d1 <- length(D1)
  best.pair.index <- c(1, 2)
  d.star <- tropical.average.linkage(D1[[1]], D1[[2]]) + 100000
  for(i in 1:d1)
    for(j in 1:d1){
      d <- d.star
      if(method == "average")
        if(i != j)
          d <- tropical.average.linkage(D1[[i]], D1[[j]])
      if(method == "min")
        if(i != j)
          d <- tropical.minimum.linkage(D1[[i]], D1[[j]])
      if(method == "max")
        if(i != j)
          d <- tropical.complete.linkage(D1[[i]], D1[[j]])
      if(d < d.star){
        d.star <- d
        best.pair.index[1] <- i
        best.pair.index[2] <- j
      }

    }
  return(list(best.pair.index, d.star))
}


