#'my_rank
#'
#'Simulate the  function 'rank()' in the 'base' package to give the rank of each element in the vector
#'
#'@param x A numeric vector
#'
#'@return A vector with average rank of each element of input x
#'
#'@examples
#'my_rank(rnorm(100))
#'my_rank(c(4,3,3,8,-1,4,7,5,4,4,3,0))   ## Note that if there are ties in elements, the average rank of ties would return correspondingly to the tied elements.
#'
#'@export
#'
my_rank <- function(x){
  x_sort <-unique(sort(x))
  len <- length(x_sort)
  result <- rep(0,length(x))
  k<-0
  for (i in  1:len){
    position <- which(x==x_sort[i])
    equal_num <- length(position)
    k <- k + equal_num
    average_rank <- (k + k - equal_num + 1)*equal_num/2/equal_num
    result[position] <- average_rank
  }
  return(result)
}

#'Mann_Whitney_U
#'
#'Carry out the rank test.By default, y = NULL and carring out wilcox signed rank test on whether x sample (first arguement) median is equal to zero.
#'If y is given a vector , then Mann Whitney U rank test is carried out to test if there is significant average rank difference between x and y.
#'The return value contains the test statistics and p value. For small samples (x and y sample size smaller than 50) the W distribution is carried out in accordance with wilcox.test() in stats package to obtain the same result. For sample size greater than 50, an approximation of normal distribution is used to calculate p value.
#'If sample rank has ties, the p value could not be calculated exactly and a approximation is used. A warning message will be prompted and the comparing function wilcox.test() will do the same.
#'
#'@param x The first group of rank-test samples. Required
#'
#'@param y The second group of samples, optional. By default, y = NULL.
#'
#'@param median_test Compare the values of differences of x and y groups is equal to the given argument.(The default is 0)
#'
#'@param paired To indicate if X,Y are samples from a paired study, T means two vector input is a paired sets(i.e. The same group of patients' blood pressure before and after treatment), the defult is False.
#'
#'@return output of a list containg test statistics (V for single group sign test, W for paired group tests) and its p value. p<0.05 would indicate rejection of (x - median_test) is centered at 0 (for one sample test)or (x - median_test) and y having similar distribution(for two samples rank test).
#'
#'@examples
#'x <- rnorm(100) + 0.1
#'y <- rnorm(100)
#'Mann_Whitney_U(x)
#'Mann_Whitney_U(x,median_test = 0.1)
#'Mann_Whitney_U(x,y,median_test = 0.1, paired = TRUE)
#'Mann_Whitney_U(x,y,median_test = 0.1)
#'Mann_Whitney_U(x,y)
#'Mann_Whitney_U(x,y, paired = TRUE)
#'
#'x <- rnorm(150) + 0.1
#'y <- rnorm(80)
#'Mann_Whitney_U(x,y,median_test = 0.1)  ## for length(x) != length(y) 'paired' arguement could only be False. We could still test if x and y median's difference is 0.1.
#'Mann_Whitney_U(x,y)
#'
#'@export
#'
Mann_Whitney_U <- function(x,y = NULL, median_test = 0, paired = F){
  median_test <- median_test
  if (is.null(y) || paired == T){
    if (is.null(y)){
    x <- x - median_test
    }else{
      x <- x - median_test - y
    }
    x <- x[!is.na(x)]
    origin_len_x <- length(x)
    x_nozero <- x[x!=0]
    len_x <- length(x_nozero)
    sign_x <- sign(x_nozero)
    abs_x <- abs(x_nozero)
    rank_x <- rank(abs_x)
    w_oneside <- sum(rank_x[x>0])
    unique_rank <- length(unique(rank_x)) == length(rank_x)
    if (origin_len_x == len_x && len_x < 50 && unique_rank){
      if (w_oneside > len_x*(len_x+1)/4){
        p <- psignrank(w_oneside - 1, len_x, lower.tail = F)  ##The special distribution of rank statistics W at a low degree of freedom under null hypothesis
      }else{
        p <- psignrank(w_oneside, len_x)
      }
      p <- min(2*p,1)
    }else{
      unique_x <- unique(x_nozero)
      sum_correction <- 0
      for (a in unique_x){
        num_dup <- sum(unique_x == a)
        sum_correction <- num_dup^3 - num_dup + sum_correction
      }
      z <- w_oneside - len_x*(len_x+1)/4
      w_oneside_sigma <- sqrt(len_x*(len_x+1)*(2*len_x+1)/24 - sum_correction/48)
      z <- z / w_oneside_sigma
      p <- 2*min(pnorm(z),pnorm(z,lower.tail = F))     ## When sample size is large, under null hypothesis, W approximately follows a nomal distribution with mean and sigma given above.
      if (!unique_rank && len_x < 50){
        warning("cannot compute exact p-value with ties")  ## However, when ties of rank is in the sample, the sigma would be unaccrrate and p-value is not exact.
      }
    }
    names(w_oneside) <- "V"
    output <- list(w_oneside, p)
    return(output)
  }else{
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    len_x <- length(x)
    len_y <- length(y)
    whole_set <- c(x,y)
    len_whole <- length(whole_set)
    whole_rank <- rank(whole_set)
    rank_x <- sum(whole_rank[1:len_x])
    rank_y <- sum(whole_rank[(len_x + 1) : len_whole])
    u1 <- rank_x - (len_x+1)*len_x/2
    unique_number <- length(unique(whole_set)) == len_whole
    if (len_x <50 && len_y<50 && unique_number){
      if (u1 < len_x*len_y/2){
        p <- pwilcox(u1,len_x,len_y)  ##for small samples U statistics follows wilcox distribution.
      }else{
        p <- pwilcox(u1-1,len_x,len_y,lower.tail = F)
      }
      p <- min(2*p,1)
    }else{
      ##for large samples, u follows approximately a normal distribution
      uni_whole <- unique(whole_set)
      sum_correction <- 0
      for (a in uni_whole) {
        ab <- sum(whole_set == a)
        sum_correction <- ab^3 - ab + sum_correction
      }
      u_mean <- u1 - len_x * len_y / 2
      u_sigama <- sqrt((len_x*len_y/12)*(len_whole+ 1-sum_correction/len_whole/(len_whole - 1)))
      z_stat <- u_mean / u_sigama
      p1 <- pnorm(z_stat)
      p2 <- pnorm(z_stat,lower.tail = F)
      p <- 2*min(p1,p2)
      if(!unique_number && len_x <50 && len_y<50){
        warning("cannot compute exact p-value with ties")
      }
    }
    names(u1) <- "W"
    output <- list(u1,p)
    return(output)
  }
}
