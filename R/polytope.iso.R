#' Project a point onto a tropical PCA-derived best fit tropical triangle
#'
#' These functions project points onto a best-fit tropical polytope derived
#' using tropical PCA.  Specifically, the points are projected onto the tropical
#' triangle defined by the column space of the PCA-derived tropical polytope.
#' @param S matrix of points representing a tropical polytope; rows are the
#'   vertices.
#' @param P a numeric vector representing a point in the tropical projective
#'   torus
#' @return numeric vector representing the projection of P onto S
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @noRd

polytope_iso <- function(D, P){
  e = length(P)
  s = dim(D)[[1]]
  Q = mat.or.vec(1, s)
  for (i in seq(s)){
    maxvalue = D[i,1] - P[[1]]
    for (j in seq(e)){
      maxvalue = max(maxvalue, D[i,j] - P[[j]])
    }
    Q[[i]]=maxvalue
  }
  return(Q)
}
