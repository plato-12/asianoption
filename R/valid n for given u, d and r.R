# Function to check probability validity for a range of n values
find_valid_n <- function(u, d, r, n_max = 20) {
  # Store results
  results <- data.frame(
    n = 2:n_max,  # Start from n=2 as per function requirement
    p_up = NA,
    p_down = NA,
    valid = NA
  )

  # Test each n value
  for(n in 2:n_max) {
    p_up <- am_pn_u(u, d, r, n)
    p_down <- am_pn_d(u, d, r, n)

    results$p_up[n-1] <- p_up
    results$p_down[n-1] <- p_down
    results$valid[n-1] <- p_up > 0 && p_up < 1 && p_down > 0 && p_down < 1
  }

  return(results)
}

# Test with given parameters
u <- 1.5
d <- 1/u
r <- 0.5

# Find valid n values
results <- find_valid_n(u, d, r)

# Print full results
cat("Full probability analysis for each n:\n")
print(results)

# Find which n values give valid probabilities
valid_n <- results$n[results$valid]
cat("\nValid n values (where both probabilities are between 0 and 1):\n")
print(valid_n)

# Print detailed probabilities for valid n values
cat("\nDetailed probabilities for valid n values:\n")
valid_results <- results[results$valid, ]
print(valid_results)
