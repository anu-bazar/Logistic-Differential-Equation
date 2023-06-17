# Set up the LDE function
logistic <- function(x, r) r * x * (1 - x)

# Set the parameter value
r <- 3

# Generate a time series from the LDE
set.seed(5000)
n <- 1000
x <- numeric(n)
x[1] <- runif(1)
for (i in 2:n) x[i] <- logistic(x[i - 1], r)

# Choose a lag interval to test
lag <- 5

# Create a matrix of lagged copies of the time series
X <- embed(x, lag)

# Compare each row of the matrix to the first row
matches <- apply(X[-1, ], 1, function(row) all(row == X[1, ]))

# Look for repeating patterns with the chosen lag interval
periods <- which(matches) + 1

# Print the detected periodicities
cat("Detected periodicities:", paste(periods, collapse = ", "))
print(matches)

