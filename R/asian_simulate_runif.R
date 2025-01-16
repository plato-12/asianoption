#' Simulate Stock Price Paths for Asian Option Pricing
#'
#' This function uses Monte Carlo simulation to generate stock price paths
#' and calculate average prices for Asian option pricing using rbinom.
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
#'
#' @examples
#' S0 <- 100    # Initial stock price
#' u <- 1.01    # Up factor
#' d <- 0.54    # Down factor
#' result <- simulate_asian_paths_ifelse(S0, u, d, n_paths = 1000, time_steps = 10)

simulate_asian_paths_ifelse <- function(S0, u, d, n_paths, time_steps) {
  # Initialize matrix to store price paths
  price_paths <- matrix(0, nrow = n_paths, ncol = time_steps + 1)
  price_paths[, 1] <- S0  # Set initial price for all paths

  # Use a normalized probability for rbinom
  # For each time step, generate binary outcome (0 or 1) for each path
  paths <- matrix(0, nrow = n_paths, ncol = time_steps)
  for(i in 1:n_paths) {
    for(j in 1:time_steps) {
      # Generate price movements step by step
      price_paths[i, j + 1] <- price_paths[i, j] * ifelse(runif(1) < 0.5, u, d)
    }
  }

  # Calculate average price along each path (excluding initial price)
  avg_prices <- rowMeans(price_paths[, -1])

  # Return results
  list(
    price_paths = price_paths,
    avg_prices = avg_prices
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
result <- simulate_asian_paths_ifelse(S0, u, d, n_paths, time_steps)

# Print first few rows of price paths
cat("First few price paths:\n")
print(head(result$price_paths))

cat("\nFirst few average prices:\n")
print(head(result$avg_prices))
