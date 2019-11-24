test_that("Two small unpaired samples rank test statistics and p value is correct", {
  x <- c(1:5)
  y <- rnorm(5)*2.5+2.5
  expect_equal(Mann_Whitney_U(x,y)[[1]], wilcox.test(x,y,correct = F)[[1]])
  expect_equal(Mann_Whitney_U(x,y)[[2]], wilcox.test(x,y,correct = F)[[3]])
})
