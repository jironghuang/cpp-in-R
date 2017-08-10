library(Rcpp)
library("microbenchmark")

cppFunction("NumericVector logmodc(int t, double yinit, double r,
double k, double thetasd){
            NumericVector y(t);
            y[0] = yinit;
      NumericVector theta = rnorm(t, 0, thetasd);
            for (int i = 1; i < t; ++i){
                y[i] = y[i-1]*(r - r*(y[i-1] / k)) * exp(theta[i]);
                }
            return y;
    }
")

#Using R::rnorm to interface with R
cppFunction('double scalarN(double m, double s) { 
            return R::rnorm(m, s); 
            }'
            )

logmodr <- function(t, yinit, r, k, thetasd){
  y <- numeric(t)
  y[1] <- yinit
  theta <- rnorm(t, 0, thetasd)
  for(i in 2:t){
    y[i] <- y[i-1]*(r - r*(y[i-1]/k)) * exp(theta[i])
  }
  return(y)
}

t <- 100
yinit <- 1
k <- 20
thetasd <- 0.1
r <- 0.2

#Writing numbers to a list
a = logmodc(t, yinit, 1.4, k, thetasd)
b = logmodr(t, yinit, 1.4, k, thetasd)
scalarN(0,1)

mb1 <- microbenchmark(
  logmodc(t, yinit, 1.4, k, thetasd),
  logmodr(t, yinit, 1.4, k, thetasd)
)
mb1