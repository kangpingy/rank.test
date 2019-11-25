test_that("Two small unpaired samples rank test statistics and p value is correct", {
  x <- c(1:5)
  y <- rnorm(5)*2
  expect_equal(Mann_Whitney_U(x,y)[[1]], wilcox.test(x,y,correct = F)[[1]])  ## Compare Test statistics correctness
  expect_equal(Mann_Whitney_U(x,y)[[2]], wilcox.test(x,y,correct = F)[[3]])  ## Compare p value correctness
  x <- rnorm(10)-2
  y <- rnorm(15)+2
  expect_equal(Mann_Whitney_U(x,y)[[1]], wilcox.test(x,y,correct = F)[[1]])  ## Compare Test statistics correctness
  expect_equal(Mann_Whitney_U(x,y)[[2]], wilcox.test(x,y,correct = F)[[3]])  ## Compare p value correctness
})

test_that("Two large unpaired samples rank test statistics and p value is correct", {
  x <- runif(60)
  y <- rnorm(54)*2.5
  expect_equal(Mann_Whitney_U(x,y)[[1]], wilcox.test(x,y,correct = F)[[1]])
  expect_equal(Mann_Whitney_U(x,y)[[2]], wilcox.test(x,y,correct = F)[[3]])
  x <- runif(54)
  y <- rnorm(54)*2.5
  expect_equal(Mann_Whitney_U(x,y,median_test = 1, paired = T)[[1]], wilcox.test(x,y,mu = 1, paired = T, correct = F)[[1]])
  expect_equal(Mann_Whitney_U(x,y,median_test = 1, paired = T)[[2]], wilcox.test(x,y,mu = 1, paired = T, correct = F)[[3]])
})

test_that("One small unpaired sample rank test statistics and p value is correct", {
  x <- rnorm(20)+0.5
  expect_equal(Mann_Whitney_U(x)[[1]], wilcox.test(x,correct = F)[[1]])
  expect_equal(Mann_Whitney_U(x)[[2]], wilcox.test(x,correct = F)[[3]])
})

test_that("One small unpaired sample rank test statistics at median = 1 and p value is correct", {
  x <- rnorm(20)
  expect_equal(Mann_Whitney_U(x,median_test = 1)[[1]], wilcox.test(x,mu = 1, correct = F)[[1]])
  expect_equal(Mann_Whitney_U(x, median_test = 1)[[2]], wilcox.test(x,mu = 1, correct = F)[[3]])
})

test_that("Two small unpaired samples rank test statistics with ties warning (p could not be caculated exactly)", {
  x <- c(rnorm(20)+0.5,2,2,2)
  y <- c(rnorm(12),1,1,1)
  expect_equal(Mann_Whitney_U(x,y)[[1]], wilcox.test(x,y,correct = F)[[1]])
  expect_warning(Mann_Whitney_U(x,y))
  expect_warning(wilcox.test(x,y,correct = F))
  ##expect_equal(Mann_Whitney_U(x)[[2]], wilcox.test(x,correct = F)[[3]]) Ti
})

test_that("One small sample rank test statistics with ties warning (p could not be caculated exactly)", {
  x <- c(rnorm(30)+1,2,2,2)
  expect_equal(Mann_Whitney_U(x)[[1]], wilcox.test(x,correct = F)[[1]])
  expect_warning(Mann_Whitney_U(x))
  expect_warning(wilcox.test(x,correct = F))
  ##expect_equal(Mann_Whitney_U(x)[[2]], wilcox.test(x,correct = F)[[3]]) Ti
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
