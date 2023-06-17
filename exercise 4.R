# Define the parameters
r <- 0.5    # intrinsic growth rate
K <- 1      # carrying capacity
d <- 0.2    # death rate
m <- 0.01   # migration rate
size <- 100 # size of the spatial grid

# Initialize the population grid
N <- matrix(runif(size^2, 0, K), nrow = size, ncol = size)

# Define a function to calculate the migration term
migrate <- function(N, i, j, m) {
  neighbors <- cbind(i + c(-1, 1, 0, 0), j + c(0, 0, -1, 1))
  neighbors <- neighbors[(neighbors[,1] >= 1) & (neighbors[,1] <= size) &
                           (neighbors[,2] >= 1) & (neighbors[,2] <= size),]
  return(m * sum(N[neighbors[,1], neighbors[,2]] - N[i,j]))
}

# Define a function to simulate the dynamics
simulate <- function(N, r, K, d, m, size, t) {
  for (i in 1:t) {
    # Calculate the migration term for each field
    M <- matrix(0, nrow = size, ncol = size)
    for (j in 1:size) {
      for (k in 1:size) {
        M[j,k] <- migrate(N, j, k, m)
      }
    }
    
    # Update the population grid using the LDE dynamics
    N <- N + r * N * (1 - N / K) + M - d * N
    
    # Enforce the carrying capacity
    N[N < 0] <- 0
    N[N > K] <- K
    
    # Plot the population grid and save as a PNG file
    png(file = paste0("C:/Users/anuba/Downloads/plot_", i, ".png"))
    image(N, col = terrain.colors(10), main = paste("Time:", i))
    dev.off()
  }
  return(N)
}

# Run the simulation for 100 time steps
final_N <- simulate(N, r, K, d, m, size, 100)
