# First find valid n values
find_valid_n <- function(u, d, r, n_max = 20) {
  results <- data.frame(
    n = 2:n_max,
    p_up = NA,
    p_down = NA,
    valid = NA
  )

  for(n in 2:n_max) {
    p_up <- am_pn_u(u, d, r, n)
    p_down <- am_pn_d(u, d, r, n)

    results$p_up[n-1] <- p_up
    results$p_down[n-1] <- p_down
    results$valid[n-1] <- p_up > 0 && p_up < 1 && p_down > 0 && p_down < 1
  }

  return(results$n[results$valid])
}

# Calculate prices for different n values
analyze_prices <- function(S0, u, d, r, K, valid_n, n_paths = 1000) {
  prices <- numeric(length(valid_n))

  for(i in seq_along(valid_n)) {
    # Run simulation for each valid n
    result <- asian_price_simulate(S0, u, d, r, n_paths, valid_n[i], K)
    prices[i] <- result$asian_call_price
  }

  # Create data frame with results
  data.frame(
    n = valid_n,
    price = prices
  )
}

# Parameters
S0 <- 100    # Initial stock price
u <- 1.5    # Up factor
d <- 0.5   # Down factor
r <- 0.90    # Risk-free rate
K <- 100     # Strike price
n_paths <- 1000  # Number of paths to simulate

# Find valid n values
valid_n <- find_valid_n(u, d, r)
cat("Valid n values:", valid_n, "\n")

# Calculate prices for each valid n
set.seed(123)  # For reproducibility
price_analysis <- analyze_prices(S0, u, d, r, K, valid_n, n_paths)

# Print results
cat("\nPrices for different n values:\n")
print(price_analysis)

# Create plot
library(ggplot2)

ggplot(price_analysis, aes(x = n, y = price)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Asian Option Price vs Number of Time Steps (n)",
    x = "Number of Time Steps (n)",
    y = "Option Price",
    caption = paste("Parameters: S0 =", S0, ", K =", K,
                    ", u =", u, ", d =", d, ", r =", r)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0, size = 10)
  )

# Save numerical results
write.csv(price_analysis, "asian_option_prices.csv", row.names = FALSE)
