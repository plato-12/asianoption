#' Calculate Risk-Neutral Probability for Up Movement (Geometric Mean)
#'
#' This function calculates the risk-neutral probability of an up movement
#' (\eqn{p_n^u}) in a binomial tree for geometric mean Asian option pricing.
#'
#' @param u Numeric. The up factor representing the relative increase in the stock price.
#' @param d Numeric. The down factor representing the relative decrease in the stock price.
#' @param r Numeric. The risk-free rate (expressed as a factor, e.g., 1.05 for 5%).
#' @param n Integer. The current step in the binomial tree (\code{n > 1}).
#'
#' @return A numeric value representing the risk-neutral probability of an up movement.
#' @export
#'
#' @examples
#' gm_pn_u(u = 1.01, d = 0.54, r = 0.90, n = 5)

gm_pn_u <- function(u, d, r, n) {
  numerator <- (r / sqrt(u)) - (d / u)^(1 / n)
  denominator <- 1 - (d / u)^(1 / n)
  p_n_u <- numerator / denominator
  return(p_n_u)
}


#' Calculate Risk-Neutral Probability for Down Movement (Geometric Mean)
#'
#' This function calculates the risk-neutral probability of a down movement
#' (\eqn{p_n^d}) in a binomial tree for geometric mean Asian option pricing.
#'
#' @param u Numeric. The up factor representing the relative increase in the stock price.
#' @param d Numeric. The down factor representing the relative decrease in the stock price.
#' @param r Numeric. The risk-free rate (expressed as a factor, e.g., 1.05 for 5%).
#' @param n Integer. The current step in the binomial tree (\code{n > 1}).
#'
#' @return A numeric value representing the risk-neutral probability of a down movement.
#' @export
#'
#' @examples
#' gm_pn_d(u = 1.01, d = 0.54, r = 0.90, n = 5)

gm_pn_d <- function(u, d, r, n) {
  numerator <- (r / sqrt(d)) - 1
  denominator <- (u / d)^(1 / n) - 1
  p_n_d <- numerator / denominator
  return(p_n_d)
}

#' Price a Geometric-Mean Asian Option Using a Binomial Tree
#'
#' This function prices a geometric-mean Asian call option using a binomial tree model.
#' It calculates all possible stock price paths, the geometric mean prices along each path,
#' and the option payoff. The final option price is determined as the discounted
#' expected value of the payoffs under the risk-neutral measure.
#'
#' @param S0 Numeric. The initial stock price.
#' @param u Numeric. The up factor representing the relative increase in the stock price.
#' @param d Numeric. The down factor representing the relative decrease in the stock price.
#' @param r Numeric. The risk-free rate (expressed as a factor, e.g., 1.05 for 5%).
#' @param n Integer. The number of steps in the binomial tree.
#' @param K Numeric. The strike price of the geometric-mean Asian call option.
#'
#' @return A list containing:
#' \describe{
#'   \item{price_paths}{Matrix. All possible stock price paths.}
#'   \item{prob_paths}{Matrix. Path probabilities at each step.}
#'   \item{geometricmean_prices}{Numeric vector. Geometric mean prices along each path.}
#'   \item{payoffs}{Numeric vector. Option payoffs for each path (\eqn{\max(0, \text{geom\_mean} - K)}).}
#'   \item{final_probabilities}{Numeric vector. The final probability for each path.}
#'   \item{asian_call_price}{Numeric. The price of the geometric-mean Asian call option.}
#' }
#' @export
#'
#' @examples
#' S0 <- 100    # Initial stock price
#' u <- 1.01    # Up factor
#' d <- 0.54    # Down factor
#' r <- 0.90    # Risk-free rate
#' n <- 3       # Number of time steps
#' K <- 100     # Strike price
#'
#' result <- cal_gm_asian(S0, u, d, r, n, K)
#' cat("The price of the Geometric-Mean Asian Call Option is:", result$asian_call_price, "\n")

cal_gm_asian <- function(S0, u, d, r, n, K) {

  n_paths <- 2^n

  price_paths <- matrix(0, nrow = n_paths, ncol = n + 1) # Create a matrix to store the prices
  prob_paths <- matrix(1, nrow = n_paths, ncol = n + 1)  # Matrix for path probabilities
  geometricmean_prices <- rep(0, n_paths)
  payoffs <- rep(0, n_paths)
  final_probabilities <- rep(1, n_paths)  # Store final probability for each path

  price_paths[, 1] <- S0  # Fill the matrix with the starting price for each path
  prob_paths[, 1] <- 1    # Initial probability is 1 for all paths

  # Generate all possible paths (binary representation of up/down moves)
  for (i in 1:n_paths) {

    # Binary representation of path (0 for down, 1 for up)
    path <- intToBits(i - 1)[1:n]
    for (j in 1:n) {

      # Up movement (u)
      if (path[j] == 1) {
        price_paths[i, j + 1] <- price_paths[i, j] * u

        if (j == 1) {
          prob_paths[i, j + 1] <- (r - d) / (u - d)
        }
        if (j == 2) {
          prob_paths[i, j + 1] <- (r - sqrt(d)) / (sqrt(u) - sqrt(d))
        }
        if(j>2) { # for j>2
          if (path[j - 1] == 1) {
            prob_paths[i, j + 1] <- gm_pn_u(u, d, r, j)
          } else {
            prob_paths[i, j + 1] <- gm_pn_d(u, d, r, j)
          }
        }
      }

      else {  # Down move
        price_paths[i, j + 1] <- price_paths[i, j] * d

        if (j == 1) {
          prob_paths[i, j + 1] <- 1 - (r - d) / (u - d)
        }
        if (j == 2) {
          prob_paths[i, j + 1] <- 1 - (r - sqrt(d)) / (sqrt(u) - sqrt(d))
        }
        if(j>2) {  # for j>2
          if (path[j - 1] == 1) {
            prob_paths[i, j + 1] <- 1 - gm_pn_u(u, d, r, j)
          } else {
            prob_paths[i, j + 1] <- 1 - gm_pn_d(u, d, r, j)
          }
        }
      }
    }

    # Compute the final probability for this path (product of probabilities along the path)
    final_probabilities[i] <- prod(prob_paths[i, 2:(n + 1)])

    # Compute the geometric mean along this path
    for (i in 1:n_paths) {
      geometricmean_prices[i] <- exp(mean(log(price_paths[i, 2:(n + 1)])))
    }

    # Calculate the payoff for each path (call option payoff: max(0, avg_price - K))
    payoffs[i] <- max(0, geometricmean_prices[i] - K)

    sum_asian_call <- sum(data$payoffs * data$final_probabilities)
    asian_call_price <- sum_asian_call / r^n
  }

  # Return the matrix of price paths, the matrix of probabilities, the vector of average prices, payoffs, and final probabilities
  list(price_paths = price_paths, prob_paths = prob_paths,
       geometricmean_prices = geometricmean_prices, payoffs = payoffs,
       final_probabilities = final_probabilities,
       asian_call_price = asian_call_price)
}

# Example usage
S0 <- 100    # Initial stock price
n <- 3      # Number of time steps
u <- 1.01     # Up factor
d <- 0.54     # Down factor
K <- 100     # Strike price
r <- 0.90    # Risk-free rate

# Generate the binomial paths, average prices, payoffs, and probabilities
data <- cal_gm_asian(S0, u, d, r, n, K)


cat("Price Paths:\n")
print(data$price_paths)

cat("\nProbability Paths (Matrix Form):\n")
print(data$prob_paths)

cat("\nAverage Prices of Each Path:\n")
print(data$geometricmean_prices)

cat("\nPayoffs for Each Path (max(0, avg_price - K)):\n")
print(data$payoffs)

cat("\nFinal Probabilities for Each Path:\n")
print(data$final_probabilities)

cat("\nThe Price of the Asian Call Option is:", data$asian_call_price, "\n")


