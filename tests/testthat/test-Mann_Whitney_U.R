test_that("Two small unpaired samples rank test statistics and p value is correct", {
  x <- c(1:5)
  y <- rnorm(5)*2.5+2.5
  expect_equal(Mann_Whitney_U(x,y)[[1]], wilcox.test(x,y,correct = F)[[1]])
  expect_equal(Mann_Whitney_U(x,y)[[2]], wilcox.test(x,y,correct = F)[[3]])
})

test_that("Two large unpaired samples rank test statistics and p value is correct", {
  x <- runif(54)
  y <- rnorm(54)*2.5
  expect_equal(Mann_Whitney_U(x,y)[[1]], wilcox.test(x,y,correct = F)[[1]])
  expect_equal(Mann_Whitney_U(x,y)[[2]], wilcox.test(x,y,correct = F)[[3]])
})

test_that("One small unpaired samples rank test statistics and p value is correct", {
  x <- rnorm(20)+0.5
  expect_equal(Mann_Whitney_U(x)[[1]], wilcox.test(x,correct = F)[[1]])
  expect_equal(Mann_Whitney_U(x)[[2]], wilcox.test(x,correct = F)[[3]])
})

test_that("One small unpaired samples rank test statistics and p value is correct", {
  x <- rnorm(100)
  expect_equal(Mann_Whitney_U(x)[[1]], wilcox.test(x,correct = F)[[1]])
  expect_equal(Mann_Whitney_U(x)[[2]], wilcox.test(x,correct = F)[[3]])
})

test_that("my_rank is equivalent to rank in base", {
  x <- rnorm(1000)
  expect_equal(my_rank(x), rank(x))
})
