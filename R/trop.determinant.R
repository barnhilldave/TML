#' Calculate the tropical determinant of a square matrix.
#'
#' This function calculates the tropical determinant (or singularity) of a square matrix
#'
#' @param P matrix of points defining a tropical polytope. Rows are the points
#' @return list containing the value of the determinant and reordered matrix P
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' P<-matrix(c(0,0,0,0,2,5,0,3,1),3,3,TRUE)
#' tdets(P)

tdets<-function(P){
  dd<-dim(P)
  B<-P
  if(dd[[1]]!=dd[[2]]){
    return(print("Not a Square Matrix!"))
  }
  else{
    tds<-c(0,0)
    perms<-permn(dd[[1]])
    tdet<-0
    i=1
    max_ind<-0
    while(i<=length(perms)){
      k<-perms[[i]]
      t<-0
      for(j in 1:length(k)){
        t<-t+P[j,k[j]]
      }
      if(t>tdet){
        tdet<-t
        tds<-c(tdet,0)
        max_ind<-perms[[i]]
      }
      else if (t==tdet){
        tds<-c(tdet,t)
      }
      i=i+1
    }
    if(tds[1]==tds[2]){
      return(print('Singular'))
    }
    else{
      for (i in 1:length(max_ind)){
        P[max_ind[i],]<-B[i,]
      }
      return(list(tdet,P))
    }
  }
}
