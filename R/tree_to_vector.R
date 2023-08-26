#' Phylogenetic tree to vector
#' @description
#' A tree is converted to a vector of pairwise distances between leaves.
#' Distance between leaves is defined as the cophenetic distance between them.
#' Normalization is applied so that the maximum distance in the vector output is 1.
#' @param tree phylogenetic tree
#' @param normalization logical; normalize the tree if TRUE
#' @return vector of pairwise distances in R^(m choose 2), where m is the number of leaves
#' @author Georgios Aliatimis \email{g.aliatimis@lancaster.ac.uk}
#' @references Aliatimis, Georgios, Ruriko Yoshida, Burak Boyaci, James A. Grant (2023). Tropical Logistic Regression on Space of Phylogenetic Trees
#' @export
#' @examples
#' tree <- ape::read.tree(text='((A:1, B:1):2, (C:1, D:1):2);')
#' tree.to.vector(tree)

tree.to.vector <- function(tree,normalization=TRUE){
  n <- length(tree$tip.label)
  e <- choose(n,2)
  # If there is at least one tip label with a non-digit character,
  # we sort the labels lexicographically.
  # Otherwise, we sort them numerically.
  if(grepl("[^0-9]",paste(tree$tip.label,collapse = "")) ){
    labels <- str_sort(tree$tip.label)
  } else {
    labels <- sort(as.vector(sapply(tree$tip.label,strtoi)))
  }
  dist_mat <- cophenetic(tree)
  dist_mat <- dist_mat[labels,labels]
  if (normalization==TRUE) dist_mat <- dist_mat/max(dist_mat)
  dist_mat[lower.tri(t(dist_mat))]
}
