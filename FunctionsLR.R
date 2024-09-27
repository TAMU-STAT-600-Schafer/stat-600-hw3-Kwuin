# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1
# Xt - ntest x p testing data, 1st column should be 1s to account for intercept
# yt - a vector of size ntest of test class labels, from 0 to K-1
# numIter - number of FIXED iterations of the algorithm, default value is 50
# eta - learning rate, default value is 0.1
# lambda - ridge parameter, default value is 1
# beta_init - (optional) initial starting values of beta for the algorithm, should be p x K matrix 

## Return output
##########################################################################
# beta - p x K matrix of estimated beta values after numIter iterations
# error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
# error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
# objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)

softmax_matrix <- function(X, beta) {
  #X: n * p
  #beta: p * K
  Z <- X %*% beta # n * K
  #print(beta)
  # probability matrix: n * K probabilities for each K and x
  #Z_max <- apply(Z, 1, max)  
  
  # for loop to get the maximum
  Z_max <- numeric(nrow(Z))
  
  for (i in 1:nrow(Z)) {
    Z_max[i] <- max(Z[i, ])
  }
  
  
  Z_exp <- exp(Z - Z_max)   
  Z_sum <- rowSums(Z_exp)    
  return(Z_exp / Z_sum)
  #return(t(apply(Z, 1, function(row) {
  #  exp_row <- exp(row - max(row))  
  #  exp_row / sum(exp_row)
  #})))
  #print("Z X softmat")
  #exp_Z = exp(Z)
  #return(t(
  #  exp_Z/rowSums(exp_Z)
  #))
}


loss <- function(Y,P, beta){
  # beta p * K
  # P n * K

  sum1 = 0
  #print("ncolP")
  #print(ncol(P))
  #sum of log likelihood
  for (k in 1:ncol(P)){
    sum1 = sum1 + sum(log(P[,k][Y == k - 1]))
  }
  return(-sum1 + sum(beta^2))
}


result <- function(P){
  # P n * K
  # get the MLE estimates
  return(max.col(P))
}


LRMultiClass <- function(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if (!all(X[, 1] == 1)) {
    stop("The first column of X must be 1s.")
  }
  if (!all(Xt[, 1] == 1)) {
    stop("The first column of X must be 1s.")
  }
  
  # Check for compatibility of dimensions between X and Y
  if (nrow(X) != length(Y)) {
    stop("X number of rows doesn't match length of Y")
  }
  
  # Check for compatibility of dimensions between Xt and Yt
  if (nrow(Xt) != length(Yt)) {
    stop("Xt's number rows doesn't match the length of Yt")
  }
  
  # Check for compatibility of dimensions between X and Xt
  if (ncol(X) != ncol(Xt)) {
    stop("X and X's number of columns don't match")
  }
  
  # Check eta is positive
  if (eta <= 0) {
    stop("Eta is not positive")
  }
  
  # Check lambda is non-negative
  if (lambda < 0) {
    stop("Lambda is negative")
  }
  
  n = nrow(X)
  p = ncol(X)
  K = length(unique(Y))
  
  

  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  if(is.null(beta_init)){
    beta_init = matrix(0, nrow = p, ncol = K)
  }else{
    if(nrow(beta_init)!= ncol(X)){
      stop("Length of beta doesn't match number of columns of X")
    }
    if(ncol(beta_init)!= K){
      stop("Numbers of categories of beta and Y don't match")
    }
  }
  

  ## Calculate corresponding pk, objective value f(beta_init), training error and testing error given the starting point beta_init
  ##########################################################################
  #print("Beta init")
  #print(beta_init)
  P = softmax_matrix(X,beta_init) # n * K matrix P[,k] is for the kth category
  #print(P)
  f_beta_init = loss(Y,P, beta_init)
  train_res = result(P)
  test_res = result(softmax_matrix(Xt, beta_init))
  
  error_train = rep(NA, numIter + 1)
  error_test = rep(NA, numIter + 1)
  objective = rep(NA, numIter + 1)
  
  error_train[1] = sum(train_res != Y)/nrow(X) * 100
  error_test[1] = sum(test_res!=Yt)/nrow(Xt) * 100
  objective[1] = f_beta_init
  beta <- beta_init
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
  
  for (i in 1:numIter){
    for (k in 1:K){
      w = P[,k]*(1 - P[,k])
      X_weighted <- sweep(X, 1, w, "*")
      
      XtWX <- t(X) %*% X_weighted
      
      #print("solve")
      #print(solve(XtWX + diag(lambda,p,p), t(X) %*% (P[,k] - (Y== k-1)) + lambda * beta[,k]))
      # updating step of Damped Newton
      beta[,k] = beta[,k] - eta * solve(XtWX + diag(lambda,p,p), t(X) %*% (P[,k] - (Y== k-1)) + lambda * beta[,k])
    }
    P = softmax_matrix(X, beta)
    #print("currentloss beta")
    #print(dim(P))
    #get the MLE result and 
    current_loss = loss(Y,P, beta)
    test_P = softmax_matrix(Xt,beta)
    train_res = result(P) - 1
    test_res = result(test_P) - 1
    
    error_train[i + 1] = 100 * sum(train_res != Y)/nrow(X)
    error_test[i + 1] = 100 * sum(test_res!=Yt)/nrow(Xt)
    objective[i + 1] = current_loss
  }
  print(objective)
  print(error_train)
  print(error_test)
  # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
  
  ## Return output
  ##########################################################################
  
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}

