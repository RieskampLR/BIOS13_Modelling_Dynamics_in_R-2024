# BIOS13 Exam - Lea Rachel Rieskamp
# 4.
rm(list=ls())

# b): Plotting the possible equilibria ####

# y-function for equilibria:
equilibria_y_func <- function(x,k,mu){
  y <- k*x^2/mu
  return(y)
}

# Setting parameters:
k <- 0.5
mu <- 0.1

# Setting a sequence of x-values:
step <- 0.001
x_values <- seq(0, 3, by = step)

# Calculating the corresponding y-values:
equilibria_y <- equilibria_y_func(x_values,k,mu)

# Plotting the possible equilibria:
plot(x_values, equilibria_y, 
     type = "l", bty = "l", lwd = 1.5, col = "blue",
     xlab = "X-concentration", ylab = "Y-concentration", 
     main = "Equilibria function",
     xaxs="i", yaxs="i", ylim = c(0,20), xlim = c(0,2.5))


##### Or ##### :

# x-function for equilibria:
equilibria_x_func <- function(y,k,mu){
  x <- sqrt(mu*y/k)
  return(x)
}

# Setting a sequence of y-values:
y_values <- seq(0, 20, by = step)

# Calculating the corresponding x-values:
equilibria_x <- equilibria_x_func(y_values,k,mu)

# Plotting the possible equilibria:
plot(equilibria_x, y_values, 
     type = "l", bty = "l", lwd = 1.5, col = "blue",
     xlab = "X-concentration", ylab = "Y-concentration", 
     main = "Equilibria function",
     xaxs="i",yaxs="i", ylim = c(0,20), xlim = c(0,2.5))
# --> same graph
