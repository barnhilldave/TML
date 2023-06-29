#' Calculate the center point and radius of the maximum inscribed ball for a tropical simplex.
#'
#' This function calculates the center point and radius of the maximum inscribed ball for a tropical simplex.
#'
#' @param A Matrix of points defining a tropical polytope. Rows are the points.
#' @return List containing the radius and center point of a maximum inscribed ball.
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' P<-matrix(c(0,0,0,0,2,5,0,3,1),3,3,TRUE)
#' max_ins_ball(P)

max_ins_ball<-function(A){
  n<-rep(0,(ncol(A)))
  A<-tdets(A)[[2]]
  for (i in 2:ncol(A)){
    if(min(A[,i])<0){
      n[i]<-min(A[,i])
      A[,i]<-A[,i]-min(A[,i])
    }
  }
  W<-matrix(0,0,ncol(A))
  P<-permutations((ncol(A)),2)
  bb<-c()
  for(i in 1:nrow(A)){
    A[i,]<-A[i,]-A[i,i]
  }
  A<-t(A)
  for (i in 1:nrow(P)){
    if(P[i,1]==1){
      a<-rep(0,ncol(A))
      a[P[i,2]-1]=1
      a[length(a)]=2
      W<-rbind(W,a)
      bb<-append(bb,A[P[i,1],P[i,2]])
    }
    else if(P[i,2]==1){
      a<-rep(0,ncol(A))
      a[P[i,1]-1]<--1
      W<-rbind(W,a)
      bb<-append(bb,A[P[i,1],P[i,2]])
    }
    else{
      a<-rep(0,ncol(A))
      a[P[i,1]-1]<--1
      a[P[i,2]-1]<-1
      a[length(a)]<-1
      W<-rbind(W,a)
      bb<-append(bb,A[P[i,1],P[i,2]])
    }
  }
  f.con<-W
  f.dir<-c(rep("<=",(ncol(A)^2-ncol(A))))
  f.rhs<--bb
  f.obj<-c(rep(0,(ncol(A)-1)),1)
  sol<-lp ("max", f.obj, f.con, f.dir, f.rhs)
  solu<-sol$solution
  rad<-solu[length(solu)]
  cent<-solu[1:(length(solu)-1)]+n[2:length(n)]+rad
  return(list(rad,c(0,cent)))
}
