# Prob/Stat Functions

# Create Bivariate Normal

sim_bvn <- function(
    n = 1e4,
    sigma_x = 2,
    sigma_y = 1,
    mu_x = 1,
    mu_y = 0,
    p = .7
){
  "Create n-length vectors X and Y with bivariate normal distributions having 
  correlation p, means mu_x an mu_y, and sds sigma_x, sigma_y"
  
  z1 <- rnorm(n, mean = 0, sd = 1)
  z2 <- rnorm(n, mean = 0, sd = 1)
  
  ax <- sqrt((1 + p)/2) * sigma_x
  bx <- sqrt((1-p)/2) * sigma_x
  
  ay <- sqrt((1 + p)/2) * sigma_y
  by <- -sqrt((1-p)/2) * sigma_y
  
  X <-  ax * z1 + bx * z2 + mu_x
  Y <-  ay * z1 + by * z2 + mu_y
  
  return(data.frame(X = X, Y = Y))
}

# Chebyshev's Inequality

cheby_ss <- function(
    variance = 1, # Variance of Distribution
    t = 2, # Difference from true mean (default = 2 SDs)
    prob = .01 # Probability sample mean will be within t of true mean
){
  "Estimate required sample size to get within m standard deviations
  of the true mean using Chebyshev's Inequality"
  
  n <- variance^2 / (t^2 * prob)
  
  return(n)
}


# Stirling's Formula to approximate factorials

stirling <- function(n){
  "Approximates the value of large factorials"
  if (n - floor(n) != 0) {
    print('n should be an integer value')
  }
  else {
    return( sqrt(2 * pi) * n ^ (n + 1/2) * exp(-n) )
  }
}