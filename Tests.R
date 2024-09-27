# This is a script to save your own tests for the function
source("FunctionsLR.R")

data_generation <- function(N, P, K, beta) {
  X <- matrix(rnorm(N * P), N, P)
  intercept <- matrix(1, N, 1)  
  X <- cbind(intercept, X) 
  print(X)
  print(beta)
  predictors <- X %*% beta
  
  probabilities <- softmax_matrix(predictors, beta)
  
  Y <- apply(probabilities, 1, function(prob) {
    which.max(rmultinom(1, size = 1, prob = prob))
  })
  
  list(X = X, probabilities = probabilities, Y = Y)
}

N <- 1000  
P <- 3    
K <- 26

beta <- matrix(rnorm((P+1) * K), P+1, K)  

data <- data_generation(N, P, K, beta)

res = LRMultiClass(data$X, data$Y, data$X, data$Y, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL)
