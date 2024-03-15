
test_that("max-tropical projection", {
  m<-sample(2:10,1)
  n<-sample(2:m,1)
  P<-matrix(sample(-30:30,n*m,replace=TRUE),m,n,TRUE)
  P<-P-P[,1]
  v<-sample(-30:30,n)
  v<-v-v[1]
  l<-apply(t(apply(P,1,function(x) v-x)),1,function(y) min(y))
  pro<-apply(l+P,2,function(x) max(x))
  npro<-pro-pro[1]
  expect_equal(project.pi(P,v), npro)
  })

test_that("min-tropical projection", {
  m<-sample(2:10,1)
  n<-sample(2:m,1)
  P<-matrix(sample(-30:30,n*m,replace=TRUE),m,n,TRUE)
  P<-P-P[,1]
  v<-sample(-30:30,n)
  v<-v-v[1]
  l<-apply(t(apply(P,1,function(x) v-x)),1,function(y) max(y))
  pro<-apply(l+P,2,function(x) min(x))
  npro<-pro-pro[1]
  expect_equal(project.pi(P,v,add='min'), npro)
})


