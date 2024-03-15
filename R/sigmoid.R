#' Sigmoid function
#'
#' Returns the sigmoid function valuation
#'
#' @param x real number
#' @return sigmoid function value at x
#' @author Georgios Aliatimis \email{g.aliatimis@lancaster.ac.uk}
#' @references Aliatimis, Georgios, Ruriko Yoshida, Burak Boyaci and James A.
#'   Grant (2023). Tropical Logistic Regression on Space of Phylogenetic Trees
#' @export
#' @examples
#' sigmoid(0) # 0.5

sigmoid <- function(x) {
  sig<-1/(1+exp(-x))
  return(sig)
  }
