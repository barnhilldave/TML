test_that("2D Tropical Ball Plot ", {
  v_star<-runif(3,-1e8,1e8)
  v_s<-v_star-v_star[1]
  r<-runif(1,2,20)
  Trop_ball(v_s,r)
  expect_true(TRUE)
})

test_that("3D Tropical Ball Plot ", {
  v_star<-c(0,runif(3,-1e3,1e3))
  r<-runif(1,2,6)
  Trop_ball(v_star,r)
  expect_true(TRUE)
})

test_that("2D Max-Tropical Hyperplane Plot ", {
  v_star<-c(0,runif(2,-1e3,1e3))
  di<-runif(1,2,6)
  mi<-di
  ma<-di
  hyper3d(v_star,di,mi,ma,plot=TRUE)
  expect_true(TRUE)
})

test_that("2D Min-Tropical Hyperplane Plot ", {
  v_star<-c(0,runif(2,-1e3,1e3))
  di<-runif(1,2,6)
  mi<-di
  ma<-di
  hyper3d(v_star,di,mi,ma,plot=TRUE,add='min')
  expect_true(TRUE)
})

test_that("3D Max-Tropical Hyperplane Plot ", {
  v_star<-c(0,runif(3,-1e3,1e3))
  di<-runif(1,2,6)
  mi<-di
  ma<-di
  hyper3d(v_star,di,mi,ma,plot=TRUE)
  expect_true(TRUE)
})

test_that("3D Min-Tropical Hyperplane Plot ", {
  v_star<-c(0,runif(3,-1e3,1e3))
  di<-runif(1,2,6)
  mi<-di
  ma<-di
  hyper3d(v_star,di,mi,ma,plot=TRUE,add='min')
  expect_true(TRUE)
})

test_that("2D Max-Tropical Polytope Plot", {
  P<-matrix(c(rep(runif(3,-1e3,1e3)),rep(runif(3,-1e3,1e3)),rep(runif(3,-1e3,1e3))),3,3,TRUE)
  P_star<-P-P[,1]
  draw.tpolytope.2d(P_star,col_lines='red',col_verts='lightblue',plot=TRUE)
  expect_true(TRUE)
})

test_that("3D Max-Tropical Polytope Plot", {
  P<-matrix(c(rep(runif(4,-1e3,1e3)),rep(runif(4,-1e3,1e3)),rep(runif(4,-1e3,1e3)),rep(runif(4,-1e3,1e3))),4,4,TRUE)
  P_star<-P-P[,1]
  draw.tpolytope.3d(P_star,col_lines='red',col_verts='lightblue',plot=TRUE)
  expect_true(TRUE)
})

test_that("2D Min-Tropical Polytope Plot", {
  P<-matrix(c(rep(runif(3,-1e3,1e3)),rep(runif(3,-1e3,1e3)),rep(runif(3,-1e3,1e3))),3,3,TRUE)
  P_star<-P-P[,1]
  draw.tpolytope.2d(P_star,col_lines='red',col_verts='lightblue',plot=TRUE,add='min')
  expect_true(TRUE)
})

test_that("3D Min-Tropical Polytope Plot", {
  P<-matrix(c(rep(runif(4,-1e3,1e3)),rep(runif(4,-1e3,1e3)),rep(runif(4,-1e3,1e3)),rep(runif(4,-1e3,1e3))),4,4,TRUE)
  P_star<-P-P[,1]
  draw.tpolytope.3d(P_star,col_lines='red',col_verts='lightblue',plot=TRUE,add='min')
  expect_true(TRUE)
})
