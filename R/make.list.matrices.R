#' Make a list from matrices
#'
#'
#' @param D matrix of points defining a tropical polytope; rows are the tropical
#'   points
#' @param index list of indices separated by cluster assignment
#' @return list of matrices
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @noRd

make.list.matrices <- function(D, index){
  D1 <- list()
  for(j in 1:length(index)){
    D2 <- as.matrix(t(D[index[[j]][1],]))
    if(length(index[[j]]) > 1)
      for(k in 2:length(index[[j]]))
        D2 <- rbind(D2, D[index[[j]][k],])
    D1[[j]] <- D2
  }
  return(D1)
}
