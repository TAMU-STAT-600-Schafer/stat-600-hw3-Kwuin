# This is a script to save your own tests for the function
source("FunctionsLR.R")

N <- 1000  
P <- 30    
K <- 26

beta <- matrix(rnorm((P+1) * K), P+1, K)  


data_generation <- function(N, P, K, beta) {
  X <- matrix(rnorm(N * P,sd = 10), N, P)
  intercept <- matrix(1, N, 1)  
  X <- cbind(intercept, X) 
  print(dim(X))
  print(dim(beta))
  
  probabilities <- softmax_matrix(X, beta)
  
  Y <- apply(probabilities, 1, function(prob) {
    which.max(rmultinom(1, size = 1, prob = prob))
  })
  
  list(X = X, probabilities = probabilities, Y = Y)
}


data <- data_generation(N, P, K, beta)


res = LRMultiClass(data$X, data$Y, data$X, data$Y, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL)

# Error rate reduced to 3.4% which means the algorithm runs well with the synthetic data