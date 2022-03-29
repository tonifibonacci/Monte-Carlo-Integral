#Problem 2

polar.normal <- function(nsize)
{
  z <- NULL
  i <- 0
  while(i < nsize){
    u1 <- runif(1)
    u2 <- runif(1)
    v1 <- 2 * u1 - 1
    v2 <- 2 * u2 - 1
    W <- v1^2 + v2^2
    if(W <= 1) {
      c <- sqrt(-2*log(W)/W)
      z[i+1] <- c*v1
      z[i+2] <- c*v2
      i <- i + 2
    }
  }
  z
}


GenChiSq <- function(df,nsize)
{
  y <- NULL
  i <- 0
  while(i < nsize) {
    x <- polar.normal(df)
    i <- i + 1
    y[i] <- sum(x^2)
  }
  y
}


GenF <- function(df1,df2,nsize)
{
  x1 <- GenChiSq(df1,nsize)
  x2 <- GenChiSq(df2,nsize)
  y <- (x1/df1)/(x2/df2)
  y
}

mean.estimated = function(df1, df2, nsize) {
  
  X = GenF(df1, df2, nsize)
  aux = sqrt(1/X)
  return(sum(aux)/nsize)
}


monte.carlo = function( df1, df2, nsize, reps) {
  estimativas = NULL
  for (i in 1:reps){
    estimativas[i] = mean.estimated(nsize, df1, df2)
  }
  return(estimativas)
}


df1 = 2 
df2 = 2 #degrees of freedom
nsize = 10000
reps = 200

simvalues = monte.carlo( df1, df2, nsize, reps)
hist(simvalues, breaks = 12, xlim=c(0,2), xlab = "X", main = "Monte Carlo method for 200 estimations with nsize = 10000")

sd(simvalues)
mean(simvalues)

