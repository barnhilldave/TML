#' Calculate the tropical Fermat-Weber point
#'
#' This function calculates the Fermat-Weber point for a tropical polytope.
#'
#' @param A Matrix of points defining a tropical polytope.  Rows are the tropical points.
#' @return Numeric vector providing the tropical Fermat-Weber point for the tropical polytope.
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#'P <-matrix(c(0,0,0,0,2,5,0,3,1),3,3,TRUE)
#'
#'Trop_FW(P)
#'

Trop_FW<-function(A){
  perms<-permutations(ncol(A),2)
  mins<-apply(A,2,min)
  if(any(mins<0)){
    mins_ind<-which(mins<0)
    A[,mins_ind]<--rep(mins[mins_ind],each=nrow(A))+A[,mins_ind]
  }

  yys<-matrix(0,nrow(perms),ncol(A),TRUE)
  for(i in 1:nrow(yys)){
    yys[i,perms[i,1]]=1
    yys[i,perms[i,2]]=-1
  }
  L<-matrix(rep(t(yys),nrow(A)),ncol=ncol(yys),byrow=TRUE)

  R<-matrix(0,0,nrow(A),TRUE)
  for(j in 1:nrow(A)){
    rrs<-matrix(0,nrow(perms),nrow(A),TRUE)
    rrs[,j]=-1
    R<-rbind(R,rrs)
  }
  f.con<-cbind(L,R)
  zer<-c(1,rep(0,(ncol(f.con)-1)))
  f.con<-rbind(f.con,zer)

  f.rhs<-c()
  for(j in 1:nrow(A)){
    for(i in 1:nrow(perms)){
      diff<--A[j,perms[i,1]]+A[j,perms[i,2]]
      f.rhs<-append(f.rhs,-diff)
    }
  }
  f.rhs<-append(f.rhs,0)
  f.dir <- c(rep("<=",nrow(f.con)))
  f.obj<-c(rep(0,(ncol(f.con)-ncol(R))),rep(1,ncol(R)))
  res<-lp ("min", f.obj, f.con, f.dir, f.rhs)
  FW<-res$solution[1:ncol(A)]
  if(any(mins<0)){
    FW[mins_ind]<-FW[mins_ind]+mins[mins_ind]
  }
  return(FW)
}

########### Tropical K-Means Clustering #################
TKmeans<-function(A,C,M){
  or_cents<-matrix(0,C,dim(A)[2],TRUE)
  clus<-sample(1:C,nrow(A),TRUE)
  try<-cbind(A,clus)
  CL=FALSE
  ct<-0
  while(CL==FALSE|ct<=M){
    for (i in 1:C){
      c<-Trop_FW(try[which(try[,ncol(try)]==i),1:(ncol(try)-1)])
      or_cents[i,]<-c
    }
    try1<-try
    for (j in 1:nrow(try)){
      try1[j,ncol(try1)]<-which.min(apply(or_cents,1,function(x) trop.dist(x,A[j,])))
    }

    if(all(try[,ncol(try)]==try1[,ncol(try)])){
      try=try1
      CL=TRUE
      ct<-ct+1
    }
    else{
      try=try1
      ct<-ct+1
    }
    print(ct)
  }
  RES<-list(try,or_cents,ct)
  return(RES)
}
