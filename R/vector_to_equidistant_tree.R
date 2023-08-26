#' Vector to equidistant tree
#'
#' A vector of pairwise distances is used to reconstruct the corresponding equidistant tree
#' @param omega vector of pairwise distances in R^(m choose 2), where m is the number of leaves
#' @return equidistant phylogenetic tree
#' @author Georgios Aliatimis \email{g.aliatimis@lancaster.ac.uk}
#' @references Aliatimis, Georgios, Ruriko Yoshida, Burak Boyaci and James A. Grant (2023). Tropical Logistic Regression on Space of Phylogenetic Trees
#' @export
#' @examples
#' omega = c(1/3,1,1,1,1,1/3)
#' tree = vector.to.equidistant.tree(omega)
#' plot(tree)

vector.to.equidistant.tree <- function(omega){
  e = length(omega)
  m = (1+sqrt(1+8*e))/2 # so that m choose 2 = e
  dist_mat = matrix(0,m,m)
  dist_mat[lower.tri(dist_mat)]= omega
  dist_mat = dist_mat + t(dist_mat)
  u = hclust(as.dist(dist_mat),method="complete")
  u = as.phylo(u)
  u
}
