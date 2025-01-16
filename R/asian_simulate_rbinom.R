#' Simulate Stock Price Paths for Asian Option Pricing
#'
#' This function uses Monte Carlo simulation with rbinom to generate stock price paths
#' and calculate average prices for Asian option pricing.
#'
#' @param S0 Numeric. The initial stock price
#' @param u Numeric. The up factor for stock price movement
#' @param d Numeric. The down factor for stock price movement
#' @param n_paths Integer. Number of paths to simulate
#' @param time_steps Integer. Number of time steps
#'
#' @return A list containing:
#'   \item{price_paths}{Matrix of simulated stock price paths}
#'   \item{avg_prices}{Vector of average prices along each path}

simulate_asian_paths_rbinom <- function(S0, u, d, n_paths, time_steps) {
  # Initialize matrix to store price paths
  price_paths <- matrix(0, nrow = n_paths, ncol = time_steps + 1)
  price_paths[, 1] <- S0  # Set initial price for all paths

  # Generate matrix of binomial random variables
  # For each path at each time step, generate 0 or 1
  # Using probability 0.5 for up/down movements
  paths <- matrix(rbinom(n_paths * time_steps, size = 1, prob = 0.5),
                  nrow = n_paths, ncol = time_steps)

  # Calculate price paths
  for(i in 1:n_paths) {
    for(j in 1:time_steps) {
      # If paths[i,j] is 1, use u; if 0, use d
      price_paths[i, j + 1] <- price_paths[i, j] * ifelse(paths[i, j] == 1, u, d)
    }
  }

  # Calculate average price along each path (excluding initial price)
  avg_prices <- rowMeans(price_paths[, -1])

  # Return results
  list(
    price_paths = price_paths,
    avg_prices = avg_prices,
    binary_paths = paths  # Added to show the binary paths generated
  )
}

# Example usage:
S0 <- 100    # Initial stock price
u <- 1.5    # Up factor
d <- 0.54    # Down factor
n_paths <- 1000  # Number of paths to simulate
time_steps <- 10 # Number of time steps

# Run simulation
set.seed(12)  # For reproducibility
result <- simulate_asian_paths_rbinom(S0, u, d, n_paths, time_steps)

# Print first few rows of price paths
cat("First few price paths:\n")
print(head(result$price_paths))

cat("\nFirst few average prices:\n")
print(head(result$avg_prices))

cat("\nFirst few binary paths (1 for up, 0 for down):\n")
print(head(result$binary_paths))
