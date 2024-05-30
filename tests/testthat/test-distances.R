test_that("tropical distance works", {
  k <- sample(1e8,1)
  x <- runif(k,-1e8,1e8)
  y <- runif(k,-1e8,1e8)
  expect_equal(trop.dist(x,y), max(x-y)-min(x-y))
})

test_that("max-tropical hyperplane distance", {
  k <- sample(1e8,1)
  x <- runif(k,-1e8,1e8)
  y <- runif(k,-1e8,1e8)
  dd<-y-x
  v<-dd[which.max(dd)]
  u<-max(dd[-which.max(dd)])
  expect_equal(trop.hyper.dist(-x,y), v-u)
})

test_that("min-tropical hyperplane distance", {
  k <- sample(1e8,1)
  x <- runif(k,-1e8,1e8)
  y <- runif(k,-1e8,1e8)
  dd<-y-x
  v<-dd[which.min(dd)]
  u<-min(dd[-which.min(dd)])
  expect_equal(trop.hyper.dist(-x,y,tadd=min), u-v)
})
