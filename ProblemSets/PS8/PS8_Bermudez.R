library(nloptr)

# Set seed for reproducibility
set.seed(100)

# Define dimensions
N <- 100000
K <- 10

# Create the X matrix
X <- matrix(rnorm(N * (K - 1)), nrow = N, ncol = K - 1)  # K-1 normal columns
X <- cbind(1, X)  # Add a column of 1s as the first column

# Define beta (length 10)
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Generate epsilon ~ N(0, 0.25)
sigma <- 0.5
eps <- rnorm(N, mean = 0, sd = sigma)

# Generate Y = X * beta + eps
Y <- X %*% beta + eps

# Using OLS
## Compute beta_hat using the closed-form OLS solution
XtX_inv <- solve(t(X) %*% X)      # (X'X)^-1
XtY <- t(X) %*% Y                 # X'Y
beta_hat <- XtX_inv %*% XtY       # beta_hat = (X'X)^-1 X'Y

## Print beta_hat and compare with true beta
print(beta_hat)
print(beta)  # true values for comparison

## My estimates are very similar to the true values!

# Using Gradient Descent
## Our objective function
objfun <- function(beta, y, X) {
  return(sum((y - X %*% beta)^2))
  # or: return(crossprod(y - X %*% beta))
}

## Gradient of our objective function
gradient <- function(beta, y, X) {
  return(as.vector(-2 * t(X) %*% (y - X %*% beta)))
}

## Use the simulated Y and X
y <- Y
X <- X

## Initial values: uniform random vector of appropriate length
beta0 <- runif(ncol(X))  # Same length as number of coefficients

## Optimization parameters
options <- list("algorithm" = "NLOPT_LD_LBFGS",
                "xtol_rel" = 1.0e-6,
                "maxeval" = 1e3)

## Run the optimization
result <- nloptr(x0 = beta0,
                 eval_f = objfun,
                 eval_grad_f = gradient,
                 opts = options,
                 y = y,
                 X = X)

print(result$solution)

## Compare with the closed-form solution
beta_ols <- solve(t(X) %*% X) %*% t(X) %*% y
print(beta_ols)

## true beta
print(beta)

# Using Nelder-Mead algorithm
## Our objective function: negative log-likelihood of normal model
objfun  <- function(theta, y, X) {
  # Separate beta and sigma from parameter vector
  beta <- theta[1:(length(theta)-1)]
  sig  <- theta[length(theta)]
  
  # Negative log-likelihood (since nloptr minimizes)
  loglike <- -sum(-0.5 * (log(2 * pi * (sig^2)) + ((y - X %*% beta)/sig)^2)) 
  return(loglike)
}

## Use simulated data
y <- Y  # from your generated dataset
X <- X

## Initial parameter values: random beta and sigma
theta0 <- runif(ncol(X) + 1)

## Optimization options
options <- list("algorithm" = "NLOPT_LN_NELDERMEAD",
                "xtol_rel" = 1.0e-6,
                "maxeval" = 1e4)

## Optimize!
result <- nloptr(x0 = theta0,
                 eval_f = objfun,
                 opts = options,
                 y = y,
                 X = X)

## Extract results
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]

## Print estimates
cat("Estimated beta:", betahat, "\n")

cat("Estimated sigma:", sigmahat, "\n")

## Compare with true values
cat("True beta:", beta, "\n")

# My answers are still very close to the true values!

# Using L-BFGS-B algorithm
## Our objective function: negative log-likelihood of normal model
objfun  <- function(theta, y, X) {
  beta <- theta[1:(length(theta)-1)]
  sig  <- theta[length(theta)]
  loglike <- -sum(-0.5 * (log(2 * pi * (sig^2)) + ((y - X %*% beta) / sig)^2))
  return(loglike)
}

## Gradient of the objective function
gradient <- function(theta, y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig  <- theta[length(theta)]
  
  grad[1:(length(theta)-1)] <- -t(X) %*% (y - X %*% beta) / (sig^2)
  grad[length(theta)] <- length(y) / sig - crossprod(y - X %*% beta) / (sig^3)
  
  return(grad)
}

## Use simulated data
y <- Y  # from your simulation
X <- X

## Initial values: use closed-form beta + random sigma
ols_beta <- as.vector(solve(t(X) %*% X) %*% t(X) %*% y)
theta0 <- c(ols_beta, runif(1))  # append random sigma start

## Optimization options
options <- list("algorithm" = "NLOPT_LD_LBFGS",
                "xtol_rel" = 1.0e-6,
                "maxeval" = 1e4)

## Run optimization
result <- nloptr(x0 = theta0,
                 eval_f = objfun,
                 eval_grad_f = gradient,
                 opts = options,
                 y = y,
                 X = X)

## Extract results
betahat  <- result$solution[1:(length(result$solution) - 1)]
sigmahat <- result$solution[length(result$solution)]

## Print results
cat("Estimated beta:", betahat, "\n")

cat("Estimated sigma:", sigmahat, "\n")

cat("True beta:", beta, "\n")

# Using lm()
# Run regression without intercept (since X already includes a column of 1s)
ols_model <- lm(y ~ X - 1)

# Print summary in console
summary(ols_model)
