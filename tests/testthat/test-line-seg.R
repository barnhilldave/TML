test_that("max-tropical line segment", {
  x <- as.matrix(runif(4,-1e8,1e8))
  y <- runif(4,-1e8,1e8)
  d<-sort(x-y)
  L<-apply(as.matrix(d),1,function(z) y+z)
  xL1 <- apply(cbind(L[,1],x),1,function(z) max(z))
  xL2 <- apply(cbind(L[,2],x),1,function(z) max(z))
  xL3 <- apply(cbind(L[,3],x),1,function(z) max(z))
  xL4 <- apply(cbind(L[,4],x),1,function(z) max(z))
  expect_equal(TLineSeg(x,y),
               structure(
                 list(
                   xL4-xL4[1],
                   xL3-xL3[1],
                   xL2-xL2[1],
                   xL1-xL1[1]
                 )
                 )
               )
})

test_that("min-tropical line segment", {
  x <- as.matrix(runif(4,-1e8,1e8))
  y <- runif(4,-1e8,1e8)
  d<-sort(x-y,decreasing = FALSE)
  L<-apply(as.matrix(d),1,function(z) y+z)
  xL1 <- apply(cbind(L[,1],x),1,function(z) min(z))
  xL2 <- apply(cbind(L[,2],x),1,function(z) min(z))
  xL3 <- apply(cbind(L[,3],x),1,function(z) min(z))
  xL4 <- apply(cbind(L[,4],x),1,function(z) min(z))
  expect_equal(TLineSeg(x,y,add='min'),
               structure(
                 list(
                   xL1-xL1[1],
                   xL2-xL2[1],
                   xL3-xL3[1],
                   xL4-xL4[1]
                 )
               )
  )
})
