# BIOS13 Exam - Lea Rachel Rieskamp
# 2.
rm(list=ls())

# g): Plot n1 and n2 against time ####

# Growth functions:
growth_func <- function(n1,n2,r1,r2,k,m) {
  dn1dt <- (r1 - k * n1^2) * n1 - m * n1
  dn2dt <- (r2 - k * n2^2) * n2 + m * n1
  return(c(dn1dt, dn2dt))
}

# Setting parameters:
r1 <- 1      # Intrinsic growth rate of population 1
r2 <- 1      # Intrinsic growth rate of population 2
k <- 0.0001  # Strength of density dependence
m <- 0.1     # Migration rate

# Creating a sequence of time steps:
timestep <- 0.001
time <- seq(0, 6, by = timestep)

# Setting the initial population sizes:
n1 <- 10
n2 <- 10

# Empty vectors for population sizes:
n1_vector <- numeric(length(time))
n2_vector <- numeric(length(time))

# Calculating and storing the population sizes for each time step:
# Loop through time steps
for(i in 1:length(time)) {
  # Storing current population sizes
  n1_vector[i] <- n1
  n2_vector[i] <- n2
  # Calculating the corresponding growth rates
  dndt <- growth_func(n1,n2,r1,r2,k,m)
  # Update population sizes based on growth rates
  n1 <- n1 + dndt[[1]] * timestep
  n2 <- n2 + dndt[[2]] * timestep
}

# Plotting the population sizes over time:
plot(time, n1_vector, 
     type = "l", bty = "l", lwd = 1.5, col = "cadetblue2", 
     xlab = "Time", ylab = "Population Size", 
     main = "Coupled dynamics of population 1 and 2 over time",
     xaxs="i",yaxs="i", xlim = c(0, 6), ylim = c(0, 120))
# Adding population 2
lines(time, n2_vector, lwd = 1.5, col = "dodgerblue3")
# Legend
legend(legend = c("Population 1", "Population 2"),
       "bottomright", inset = c(-0.15, 0.15),
       col = c("cadetblue2", "dodgerblue3"), lwd = 1.5, bty = "n")

