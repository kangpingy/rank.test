test_that("my_rank is equivalent to rank in base", {
  x <- rnorm(1000)
  expect_equal(my_rank(x), rank(x))
  x <- c(3,7,3,4,5,3,-1,4,3,3,5,7,7,0,23,2)
  expect_equal(my_rank(x), rank(x))
})
