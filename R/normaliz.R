#' Normalize a point or set of points in the tropical projective torus
#'
#' This function normalizes a point or set of points in the tropical projective torus by making the first coordinate zero.
#'
#' @param D Vector in the tropical projective torus or a matrix of points in the tropical projective torus.  For matrices, rows are the points.
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
normaliz.vectors <- function(D){ ## D is a matrix
d <- dim(D)
D1 <- D
for(i in 1:d[1])
  D1[i, ] <- D1[i, ] - rep(D1[i, 1], d[2])
return(D1)
}

#' @export
#' @rdname normalize
normaliz.polytope <- function(D){
  d <- dim(D)
  for(i in 1:d[1])
    D[i, ] <- normaliz.vector(D[i,])
  return(D)
}
