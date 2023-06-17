library(ggplot2)

# Defining our LDE
logistic.map <- function(x, r) {
  return(r * x * (1 - x))
}

# Set the value of r that we are analysing
r <- 4.0
# Iterations
n.iter <- 5000

# Set the threshold value for the Poincaré section
threshold <- 0.5

# Set the embedding dimension for the Poincaré section
embedding.dimension <- 2

# Initialize the time series with a random value between 0 and 1
x <- runif(1)

# Initialize an empty vector to store the threshold-crossing points
poincare.section <- c()

# Iterate the logistic map and store the threshold-crossing points
for (i in 1:n.iter) {
  x <- logistic.map(x, r)
  if (x > threshold) {
    poincare.section <- c(poincare.section, x, logistic.map(x, r))
  }
}

# Convert the threshold-crossing points into a matrix with the desired embedding dimension
poincare.section.matrix <- embed(poincare.section, embedding.dimension)

# Plot the Poincaré section
ggplot(data = data.frame(x = poincare.section.matrix[,1], y = poincare.section.matrix[,2]), aes(x, y)) +
  geom_point() +
  labs(title = "Poincaré Section Plot for r=4.0")

# Calculate the distances between repeating patterns in the Poincaré section
pattern.distances <- diff(which(poincare.section.matrix[,1] == min(poincare.section.matrix[,1])))
periodicity <- ifelse(length(unique(pattern.distances)) == 1, unique(pattern.distances), NA)

# Print the periodicity (if it exists)
if (!is.na(periodicity)) {
  print(paste("Periodicity:", periodicity))
} else {
  print("No periodicity detected.")
}
