test_that("mean works",{
  list = myncurve(5,2,5)
  expect_equal(list[[1]], 5)
})
test_that("sigma works",{
  list = myncurve(5,2,5)
  expect_equal(list[[2]], 2)
})
test_that("probability works",{
  list = myncurve(5,2,5)
  expect_equal(list[[3]], 0.5)
})
