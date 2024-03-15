
test_that("Get max-tropical ball vertices ", {
  k<-sample(3:9,1)
  v_star<-runif(k,-1e8,1e8)
  r<-runif(1,2,20)
  rvec<-rep(0,k)
  mat<-matrix(0,0,k,TRUE)
  combos<-t(as.matrix(combn(ncol(mat)-1,1)+1))
  for (j in nrow(combos):1){
    rv<-rvec
    rv[combos[j,]]<-r
    v<-v_star+rv
    mat<-rbind(mat,v)
  }
  rv<-c(0,rep(r,ncol(mat)-1))
  mat<-rbind(mat,v_star-rv)
  dimnames(mat)<-NULL
  V<-trop.bal.vert(v_star,r)
  mat1<-mat
  for (t in 1:nrow(mat)){
    for(n in 1:nrow(mat)){
    if(all(mat[t,]==V[n,])){
      mat1[n,]<-mat[t,]
    }
    }
  }
  expect_equal(V, mat1)
})

test_that("Get all tropical ball vertices ", {
  k<-sample(3:9,1)
  v_star<-runif(k,-1e8,1e8)
  r<-runif(1,2,20)
  rvec<-rep(0,k)
  mat1<-matrix(0,0,k,TRUE)
  mat2<-matrix(0,0,k,TRUE)
  for(i in 1:(k-1)){
    combos<-t(as.matrix(combn(ncol(mat1)-1,i)+1))
    for (j in nrow(combos):1){
      rv<-rvec
      rv[combos[j,]]<-r
      v<-v_star+rv
      u<-v_star-rv
      mat1<-rbind(mat1,v)
      mat2<-rbind(mat2,u)
    }
  }
  mate<-rbind(mat1,mat2)
  dimnames(mate)<-NULL
  V<-trop.bal.vert(v_star,r,add='all')
  dimnames(V)<-NULL
  inds<-c()
  mate1<-mate
  for (t in 1:nrow(mate)){
    for(n in 1:nrow(mate)){
      if(all(mate[t,]==V[n,])){
        inds<-append(inds,n)
        mate1[n,]<-mate[t,]
      }
    }
  }
  expect_equal(V, mate1)
})

