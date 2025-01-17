#' Simulate Stock Price Paths and Calculate Asian Option Price
#'
#' This function uses Monte Carlo simulation to generate stock price paths,
#' calculate dynamic risk-neutral probabilities, and price the Asian option.
#'
#' @param S0 Numeric. The initial stock price
#' @param u Numeric. The up factor for stock price movement
#' @param d Numeric. The down factor for stock price movement
#' @param r Numeric. The risk-free rate
#' @param n_paths Integer. Number of paths to simulate
#' @param time_steps Integer. Number of time steps
#' @param K Numeric. Strike price for the Asian option
#'
#' @return A list containing price paths, probabilities, and option price

asian_price_simulate <- function(S0, u, d, r, n_paths, time_steps, K) {
  # Initialize matrices
  price_paths <- matrix(0, nrow = n_paths, ncol = time_steps + 1)
  prob_paths <- matrix(1, nrow = n_paths, ncol = time_steps + 1)
  price_paths[, 1] <- S0  # Set initial price
  prob_paths[, 1] <- 1    # Initial probability is 1

  # Generate paths using rbinom
  paths <- matrix(rbinom(n_paths * time_steps, size = 1, prob = 0.5),
                  nrow = n_paths, ncol = time_steps)

  # Calculate price paths and probabilities
  for(i in 1:n_paths) {
    for(j in 1:time_steps) {
      # Determine if it's an up or down movement
      is_up <- paths[i, j] == 1

      # Calculate price
      price_paths[i, j + 1] <- price_paths[i, j] * ifelse(is_up, u, d)

      # Calculate probability based on previous movement
      if (j == 1) {
        # First step probability
        prob_paths[i, j + 1] <- ifelse(is_up,
                                       (r - d) / (u - d),            # prob for up
                                       1 - (r - d) / (u - d))        # prob for down
      } else {
        # Previous movement
        prev_up <- paths[i, j-1] == 1

        if (is_up) {
          # Current up movement
          prob_paths[i, j + 1] <- ifelse(prev_up,
                                         am_pn_u(u, d, r, j),    # prev up, current up
                                         am_pn_d(u, d, r, j))    # prev down, current up
        } else {
          # Current down movement
          prob_paths[i, j + 1] <- ifelse(prev_up,
                                         1 - am_pn_u(u, d, r, j), # prev up, current down
                                         1 - am_pn_d(u, d, r, j)) # prev down, current down
        }
      }
    }
  }

  # Calculate final probabilities (product of probabilities along each path)
  final_probabilities <- apply(prob_paths[, 2:(time_steps + 1)], 1, prod)

  # Calculate average prices along each path
  avg_prices <- rowMeans(price_paths[, -1])

  # Calculate payoffs
  payoffs <- pmax(0, avg_prices - K)

  # Calculate the price of the Asian call option
  sum_asian_call <- sum(payoffs * final_probabilities)
  asian_call_price <- sum_asian_call / r^time_steps

  # Return results
  list(
    price_paths = price_paths,
    prob_paths = prob_paths,
    final_probabilities = final_probabilities,
    avg_prices = avg_prices,
    payoffs = payoffs,
    asian_call_price = asian_call_price,
    binary_paths = paths
  )
}

# Example usage:
S0 <- 100    # Initial stock price
u <- 1.01    # Up factor
d <- 0.54    # Down factor
r <- 0.90    # Risk-free rate
K <- 100     # Strike price
n_paths <- 1000  # Number of paths to simulate
time_steps <- 10 # Number of time steps

# Run simulation
set.seed(123)  # For reproducibility
result <- asian_price_simulate(S0, u, d, r, n_paths, time_steps, K)

# Print results
cat("First few price paths:\n")
print(head(result$price_paths))

cat("\nFirst few probability paths:\n")
print(head(result$prob_paths))

cat("\nFirst few payoffs:\n")
print(head(result$payoffs))

cat("\nThe Price of the Asian Call Option is:", result$asian_call_price, "\n")
