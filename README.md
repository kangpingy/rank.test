# rank.tests

## Overview
  The package name is 'rank.tests' (Sorry for the inconsistency as might happen in installing the package.) 

  There are two functions in the package one is my_rank which simulate rank() function in the base package, which is critical to return correctly with tied rank result in the following main rank test function.The 'Mann_Whitney_U' function is the main rank test function in the package, which could carry out two similar rank test through different input arguement.
  
## Install the package  (This part is added for graders' convenience for successful installation of this package)
library(devtools)/n
devtools::install_github("kangpiny/rank.test", build_vignettes = F , Force = T)/n
library(rank.tests)/n
# Edited on Dec 1st.

## Wilcoxon signed-rank test
  One of the rank test is the Wilcoxon signed-rank test. It is a non-parametric statistical hypothesis test used to compare two related samples, matched samples, or repeated measurements on a single sample to assess whether their population mean ranks differ (i.e. it is a paired difference test). It can be use as an alternative to the paired Student's t-test (also known as "t-test for matched pairs" or "t-test for dependent samples") when the distribution of the differences between the two samples cannot be assumed to be normally distributed. A Wilcoxon signed-rank test is a nonparametric test that can be used to determine whether two dependent samples were selected from populations having the same distribution.
## Mann-Whitney U test  
  The other rank test is Mann–Whitney U test. It is a nonparametric test of the null hypothesis that it is equally likely that a randomly selected value from one group will be less than or greater than a randomly selected value from a second group. It do not make any assumption on two groups' distribution to carry out the test. So the test is especially useful when the comparing groups does not follow a normal distribution. 
  These two rank tests share similar way in caculating test statistics that could merge two test in one function, detailed introduction to the function is given in the help page. 
## Comparing function in R 'stats' package -- wilcox.test()
  The 'Mann_Whitney_U' function will return the rank test statistics and p value based on the result. To ensure the correctness, 'wilcox.test()' function in 'stats' package is tested via testthat. The returned test statistics and p value is the same as that in 'wilcox.test()'. 
## Help
  You can use '?my_rank' and '?Mann_Whitney_U' to visit the functions' help page for more detailed information.
  <!-- badges: start -->
  [![Travis build status](https://travis-ci.org/kangpingy/rank.test.svg?branch=master)](https://travis-ci.org/kangpingy/rank.test)
  <!-- badges: end -->
  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/kangpingy/rank.test/branch/master/graph/badge.svg)](https://codecov.io/gh/kangpingy/rank.test?branch=master)
  <!-- badges: end -->
