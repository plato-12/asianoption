# Function to calculate probabilities for each period
calc_probabilities <- function(u, d, r, n) {
  # Predefined probabilities for the base cases
  p_0 <- 1
  p_1 <- (r - d) / (u - d)
  
  # Function to calculate p_n^u for n > 1
  calc_pn_u <- function(u, d, r, n) {
    numerator <- (((u^n - 1) / (u - 1)) - (n / (n - 1)) * r * ((u^(n - 1) - 1) / (u - 1)))
    denominator <- u^(n - 2) * (u - d)
    p_n_u <- 1 - (numerator / denominator)
    return(p_n_u)
  }
  
  # Function to calculate p_n^d for n > 1
  calc_pn_d <- function(u, d, r, n) {
    numerator <- ((n / (n - 1)) * r * ((d^(n - 1) - 1) / (d - 1))) - ((d^n - 1) / (d - 1))
    denominator <- d^(n - 2) * (u - d)
    p_n_d <- numerator / denominator
    return(p_n_d)
  }
  
  # Calculate probabilities for n-th period
  p_n_u <- calc_pn_u(u, d, r, n)
  p_n_d <- calc_pn_d(u, d, r, n)
  
  # Return the probabilities
  return(list(p_n_u = p_n_u, p_n_d = p_n_d))
}

# Function to find the valid ranges for u, d, and r
find_valid_ranges <- function(n, u_range = c(1, 1.5), d_range = c(0.5, 1), r_range = c(0.5, 0.7), step = 0.01) {
  valid_ranges <- data.frame(u = numeric(), d = numeric(), r = numeric())
  
  # Loop through all combinations of u, d, and r within the given ranges
  for (u in seq(u_range[1], u_range[2], by = step)) {
    for (d in seq(d_range[1], d_range[2], by = step)) {
      for (r in seq(r_range[1], r_range[2], by = step)) {
        # Skip if u <= d (invalid condition)
        if (u <= d) next
        
        # Calculate probabilities
        prob <- calc_probabilities(u, d, r, n)
        
        # Ensure probabilities are finite and check if they are within (0, 1)
        if (is.finite(prob$p_n_u) && is.finite(prob$p_n_d) &&
            prob$p_n_u > 0 && prob$p_n_u < 1 &&
            prob$p_n_d > 0 && prob$p_n_d < 1) {
          
          # Add the valid combination to the list
          valid_ranges <- rbind(valid_ranges, data.frame(u = u, d = d, r = r))
        }
      }
    }
  }
  
  return(valid_ranges)
}

# Example usage
n <- 3  # Number of time steps
valid_ranges <- find_valid_ranges(n)

# Print the valid ranges
if (nrow(valid_ranges) > 0) {
  cat("Valid ranges for u, d, and r where probabilities are within (0,1):\n")
  print(valid_ranges)
} else {
  cat("No valid ranges found for the given n.\n")
}
