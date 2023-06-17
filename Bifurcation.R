# Generate bifurcation plot
n <- 500
m <- 300
rvals <- seq(3.8, 3.9, length.out = 5000)
xvals <- runif(length(rvals))
par(mar = c(2, 2, 1, 1), mgp = c(2, 0.7, 0))
plot(NA, xlim = c(3.8, 3.9), ylim = c(0, 1), xlab = "r", ylab = expression(x[t]), 
     main = "Bifurcation Diagram for the Logistic Map")
logistic <- function(r, x) {
  r * x * (1 - x)
}

for (i in 1:n) {
  xvals <- logistic(rvals, xvals)
  if (i > n - m) {
    points(rvals, xvals, col = "blue", pch = ".", cex = 0.2)
  }
}

plot_time_series <- function(r, n) {
  x <- runif(1)
  xvals <- numeric(n)
  rvals <- rep(r, n)
  for (i in 1:n) {
    x <- logistic(r, x)
    xvals[i] <- x
  }
  plot(xvals, type = "l", xlab = "Time", ylab = expression(x[t]), 
       main = paste("Time Series for r =", r))
}
