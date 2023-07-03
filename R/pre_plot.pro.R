#' Projections of points onto a tropical triangle
#'
#' This function produces the a matrix of points projected onto a tropical triangle defined by the column space of a matrix
#' @param S matrix of points representing a tropical polytope; rows are the vertices
#' @param D data points in the tropical projective torus
#' @return matrix of points representing projections of the points in D (row vectors) onto S
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @export
#' @examples
#' s <- 3 #number of vertices.  Here it is a tropical triangle
#' d <- 3 ## dimension
#' N <- 100 ## sample size
#' D <- matrix(rep(0, N*d), N, d)
#' D[, 1] <- rnorm(N, mean = 5, sd = 5)
#' D[, 2] <- rnorm(N, mean = -5, sd = 5)
#' D[, 3] <- rnorm(N, mean = 0, sd = 5)
#'
#' index <- sample(1:N, s)
#' S <- D[index,]
#'
#' DD <- pre.pplot.pro(S, D)

pre.pplot.pro <- function(S, D){
  d <- dim(D)
  s <- dim(S)

  DD <- matrix(rep(0, s[1]*d[1]), d[1], s[1])
  for(i in 1:d[1]){
    DD[i, ] <- normaliz.vector(polytope_iso(S, D[i, ]))
  }
  return(DD)
}
