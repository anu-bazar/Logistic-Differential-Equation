# Set up the LDE function
logistic <- function(x, r) r * x * (1 - x)

# Set the range of r values to explore
r_range <- seq(2.5, 4, by = 0.001)

# Choose a lag interval to test
lag <- 5

# Initialize a vector to store the detected periodicities
periods <- numeric(length(r_range))

# Loop over each r value and detect periodicities
for (i in seq_along(r_range)) {
  # Generate a time series from the LDE
  set.seed(123)
  n <- 1000
  x <- numeric(n)
  x[1] <- runif(1)
  for (j in 2:n) x[j] <- logistic(x[j - 1], r_range[i])
  
  # Create a matrix of lagged copies of the time series
  X <- embed(x, lag)
  
  # Compare each row of the matrix to the first row
  matches <- apply(X[-1, ], 1, function(row) all(row == X[1, ]))
  
  # Look for repeating patterns with the chosen lag interval
  max_period <- max(which(matches)) + 1
  if (is.na(max_period) || is.infinite(max_period)) {
    # Periodicity detection failed
    warning(paste("Periodicity detection failed for r =", r_range[i]))
    periods[i] <- NA
  } else {
    # Store the detected periodicity
    periods[i] <- max_period
  }
}

# Plot the detected periodicities as a function of r
plot(r_range, periods, type = "l", xlab = "r", ylab = "Periodicity",
     main = "Periodicity of the Logistic Difference Equation",
     ylim = c(0, max(periods, na.rm = TRUE) + 1))
