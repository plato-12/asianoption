am_pn_u <- function(u, d, r, n) {
  numerator <- (((u^n - 1) / (u - 1)) - (n / (n - 1)) * r * ((u^(n - 1) - 1) / (u - 1)))
  denominator <- u^(n - 2) * (u - d)
  p_n_u <- 1- (numerator / denominator)
  return(p_n_u)
}

am_pn_d <- function(u, d, r, n) {
  numerator <- ((n / (n - 1)) * r * ((d^(n - 1) - 1) / (d - 1))) - ((d^n - 1) / (d - 1))
  denominator <- d^(n - 2) * (u - d)
  p_n_d <- numerator / denominator
  return(p_n_d)
}

cal_am_asian_monte_carlo <- function(S0, u, d, r, n, K, n_samples = 100) {
  # Pre-allocate storage for results
  avg_prices <- numeric(n_samples)
  payoffs <- numeric(n_samples)
  final_probabilities <- numeric(n_samples)

  # Simulate n_samples paths
  for (i in 1:n_samples) {
    path <- numeric(n + 1)  # Store stock prices along the path
    path[1] <- S0          # Initial price

    prob <- 1  # Initialize path probability
    for (j in 1:n) {
      # Randomly decide an up or down move
      if (runif(1) < (r - d) / (u - d)) {  # Up move
        path[j + 1] <- path[j] * u
        if (j == 1) {
          prob <- prob * (r - d) / (u - d)  # Initial step probability
        } else {
          # Dynamic probability adjustment for up moves
          if (path[j] / path[j - 1] == u) {
            prob <- prob * am_pn_u(u, d, r, j)
          } else {
            prob <- prob * am_pn_d(u, d, r, j)
          }
        }
      } else {  # Down move
        path[j + 1] <- path[j] * d
        if (j == 1) {
          prob <- prob * (1 - (r - d) / (u - d))  # Initial step probability
        } else {
          # Dynamic probability adjustment for down moves
          if (path[j] / path[j - 1] == u) {
            prob <- prob * (1 - am_pn_u(u, d, r, j))
          } else {
            prob <- prob * (1 - am_pn_d(u, d, r, j))
          }
        }
      }
    }

    # Compute average price for the path
    avg_prices[i] <- mean(path)

    # Calculate payoff (Asian call)
    payoffs[i] <- max(0, avg_prices[i] - K)

    # Store final probability for the path
    final_probabilities[i] <- prob
  }

  # Calculate Asian option price
  sum_asian_call <- sum(payoffs * final_probabilities)
  asian_call_price <- sum_asian_call / r^n

  # Return the results
  list(asian_call_price = asian_call_price,
       avg_prices = avg_prices,
       payoffs = payoffs,
       final_probabilities = final_probabilities)
}

# Parameters
S0 <- 100    # Initial stock price
n <- 3     # Number of time steps
u <- 1.01    # Up factor
d <- 0.54    # Down factor
K <- 100     # Strike price
r <- 0.90    # Risk-free rate
n_samples <- 100  # Number of paths to sample

# Calculate Asian option price using Monte Carlo
data <- cal_am_asian_monte_carlo(S0, u, d, r, n, K, n_samples)

cat("The estimated price of the Asian Call Option is:", data$asian_call_price, "\n")







