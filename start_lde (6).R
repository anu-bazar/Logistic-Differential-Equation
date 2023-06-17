model <- function(r,n0,time) {
         output <- numeric(time+1) # set up a vector to store the results
         output[1] <- n <- n0
         for (t in c(1:time)) { 
             n <- r * n 
             output[t+1] <- n
         }
         output
}


### Run the model
out  <- model(1.2,1,10)
### Plot the model
plot(1:length(out)-1,out,xlab="time",ylab="population size")
