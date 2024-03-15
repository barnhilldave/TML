test_that("Singular Matrix",{
  P<-matrix(c(0,4,2,0,3,7,0,2,1,0,4,4),4,3,TRUE)
  expect_warning(tdets(P), "Not a Square Matrix!")
})

test_that("Max-Tropical Determinant",{
  P<-matrix(c(0,4,2,0,3,7,0,2,1),3,3,TRUE)
  D<-lp.assign(P,direction = 'max')
  expect_equal(tdets(P)[[1]], D$objval)
})

test_that("Min-Tropical Determinant",{
  P<-matrix(c(0,4,2,0,3,7,0,6,1),3,3,TRUE)
  D<-lp.assign(P)
  expect_equal(tdets(P,add='min')[[1]], D$objval)
})
