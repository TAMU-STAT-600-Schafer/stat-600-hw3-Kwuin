# This is a script to save your own tests for the function
source("FunctionsLR.R")


data_generation <- function(N, P, K, beta) {
  X <- matrix(rnorm(N * P), N, P)
  X <- cbind(1, X)
  
  predictors <- X %*% beta
  
  probabilities <- softmax_matrix(predictors, beta)
  
  Y <- apply(probabilities, 1, function(prob) {
    which.max(rmultinom(1, size = 1, prob = prob))
  })
  
  list(X = X, probabilities = probabilities, Y = Y)
}


N <- 100  
P <- 3    
K <- 4    
beta <- matrix(rnorm((P+1) * K), P+1, K)  

data <- generate_logistic_regression_data(N, P, K, beta)


head(data$X)  
head(data$probabilities)  
head(data$Y)
