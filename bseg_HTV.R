group <- function(x){
  y <- c()
  cnt <- 1
  for (i in 1:(length(x)-1)) {
    if (x[i]==x[i+1]){
      cnt <- cnt + 1
    }
    else{
      y <- append(y, cnt)
      cnt <- 1
    }
  }
  y <- append(y, cnt)
  return(y)
}

ungroup <- function(x,ini){
  y <- c()
  current <- ini
  for (i in x){
    y <- append(y, rep(current,i))
    current <- as.numeric(!current)
  }
  return (y)
}

# x:data, y:increase of loss data
merge <- function(x,y){
  i <- which.min(y[2:(length(y)-1)]) + 1
  loss <- y[i]
  x[i-1] <- x[i-1] + x[i+1] + x[i]
  y[i-1] <- y[i-1] + y[i+1] - y[i]
  x <- x[-i]
  x <- x[-i]
  y <- y[-i]
  y <- y[-i]
  return (list(x,y,loss))
}

denoise <- function(x,r=0){
  # outputs
  returnedx = list(x)
  returnedloss = c(0)
  
  initial <- c(x[1], as.numeric(!x[1]), x[1], as.numeric(!x[1]))
  x <- group(x)
  # l : # of groups left to flip
  l <- length(x) - (r+1)
  left <- c(l, l-1, l-1, l-2)
  loss <- list(c(0), c(x[1]), c(x[length(x)]), c(x[1]+x[length(x)]))
  
  # preprocess (dividing into 4 cases)
  x1 <- x
  x2 <- x
  x2[2] <- x2[1] + x2[2]
  x2 <- x2[-1]
  x3 <- x
  x3[length(x3)-1] <- x3[length(x3)-1] + x3[length(x3)]
  x3 <- x3[-length(x3)]
  x4 <- x3
  x4[2] <- x4[1] + x4[2]
  x4 <- x4[-1]
  
  results <- list(list(x1), list(x2), list(x3), list(x4))
  
  for (i in 1:4){
    cnt <- 1
    y <- results[[i]][[1]]
    while(left[i]>1){
      temp <- merge(results[[i]][[cnt]], y)
      y <- temp[[2]]
      results[[i]] <- append(results[[i]], temp[1])
      loss[[i]] <- append(loss[[i]], loss[[i]][cnt] + temp[[3]])
      left[i] <- left[i] - 2
      cnt <- cnt + 1
    }
  }
  
  for (i in 1:l){
    if (i%%2 == 0){
      j <- i/2 + 1
      mindex <- which.min(c(loss[[1]][j], loss[[2]][j], loss[[3]][j], loss[[4]][j-1]))
      if (mindex==4){
        mindex2 <- j-1
      }
      else{
        mindex2 <- j
      }
      mini <- loss[[mindex]][mindex2]
    }
    else{
      j <- (i+1)/2 + 1
      mindex <- which.min(c(loss[[1]][j], loss[[2]][j-1], loss[[3]][j-1], loss[[4]][j-1]))
      if (mindex==1){
        mindex2 <- j
      }
      else{
        mindex2 <- j-1
      }
      mini <- loss[[mindex]][mindex2]
    }
    returnedx <- append(returnedx, list(ungroup(results[[mindex]][[mindex2]], initial[mindex])))
    returnedloss <- append(returnedloss, mini)
  }
  return (list(returnedx, returnedloss))
}

# data example : Tiger Woods tournament data
# data preprocessing
tiger <- '0000101000100000110000100000000000000000100000000000100010000000101010111110110001000110101110000100000011101100'
tiger <- strsplit(tiger, split = "")[[1]]
tiger <- as.numeric(tiger)

# HTV algorithm
denoised_tiger <- denoise(tiger)

# regularization parameter (# of changepoints)
R <- 8

# denoised sequence
total_changepoints <- length(denoised_tiger[[2]])
idx <- total_changepoints - R
denoised_tiger[[1]][[idx]]

# loss
denoised_tiger[[2]][idx]