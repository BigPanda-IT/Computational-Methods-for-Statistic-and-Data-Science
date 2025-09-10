library(Rcpp)
sourceCpp("mySweep_cpp.cpp")

set.seed(123)
X <- matrix(rnorm(500*500), nrow = 500, ncol = 500)
A <- t(X) %*% X
n <- nrow(A)
p <- ncol(A)

# Measuring the execution time of the R function
start_time <- Sys.time()
result_R <- mySweep(A, p)
end_time <- Sys.time()
time_R <- end_time - start_time
print(paste("The execution time of the R function: ", time_R))


# Measuring the execution time of a C++ function
start_time <- Sys.time()
result_cpp <- mySweep_cpp(A, p)
end_time <- Sys.time()
time_cpp <- end_time - start_time
print(paste("The execution time of the C++ function: ", time_cpp))


# Checking for equality of results
all.equal(result_R, result_cpp)

mySweep <- function(A, m) {
  n <- dim(A)[1]
  for (k in 1:m) {
    for (i in 1:n)
      for (j in 1:n)
        if (i != k & j != k)
          A[i, j] <- A[i, j] - A[i, k] * A[k, j] / A[k, k]
    for (i in 1:n)
      if (i != k)
        A[i, k] <- A[i, k] / A[k, k]
    for (j in 1:n)
      if (j != k)
        A[k, j] <- A[k, j] / A[k, k]
    A[k, k] <- -1 / A[k, k]
  }
  return(A)
}
