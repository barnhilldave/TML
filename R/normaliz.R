#' Normalize a point in the tropical projective torus
#'
#' This function normalizes a point in the tropical projective torus by making the first coordinate zero.
#'
#' @param D Point in the tropical projective torus
#' @return A single or set of normalized points with the first coordinate zero.
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @name normalize
#' @examples
#'D <-c(8,4,2)
#'normaliz.vector(D)
#'
#' P <-matrix(c(8,4,2,10,1,3,7,2,1),3,3,TRUE)
#' normaliz.vectors(P)
#'
#' M<-matrix(c(2,2,2,3,6,4,2,4,7),3,3,TRUE)
#' normaliz.polytope(M)
#'
#' @export
#' @rdname normalize
normaliz.vector <- function(D){
  return(D - rep(D[1], length(D)))
}
#' @export
#' @rdname normalize
normaliz.vectors <- function(DD){ ## DD is a matrix
d <- dim(DD)
D1 <- DD
for(i in 1:d[1])
  D1[i, ] <- D1[i, ] - rep(D1[i, 1], d[2])
return(D1)
}

#' @export
#' @rdname normalize
normaliz.polytope <- function(M){
  d <- dim(M)
  for(i in 1:d[1])
    M[i, ] <- normaliz.vector(M[i,])
  return(M)
}
