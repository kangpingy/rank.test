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
        p <- psignrank(w_oneside - 1, len_x, lower.tail = F)
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
      p <- 2*min(pnorm(z),pnorm(z,lower.tail = F))
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
        p <- pwilcox(u1,len_x,len_y)
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
    }
    names(u1) <- "W"
    output <- list(u1,p)
    return(output)
  }
}

Kruskal_Wallis <- function(x,group){
  x <- 1
  group <-2
}
