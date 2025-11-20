# BIOS13 Exam - Lea Rachel Rieskamp
# 4.
rm(list=ls())

# c): Simulating X and Y concentrations ####

# Concentration functions:
diff_func <- function(x, y, k, mu) {
  dxdt <- -2*k*x^2 + 2*mu*y
  dydt <- k*x^2 - mu*y
  return(c(dxdt, dydt))
}

# Setting parameters:
k <- 0.5
mu <- 0.1

# Setting initial concentrations:
# X
x <- 5
x1 <- 2.5
x2 <- 0.1
x3 <- 5
x4 <- 0.9
# Y
y <- 0
y1 <- 5
y2 <- 10
y3 <- 11
y4 <- 18

# Setting a time sequence:
timestep <- 0.001
time <- seq(0, 3, by = timestep)

# Empty vectors for concentrations:
# X
x_vector <- numeric(length(time))
x1_vector <- numeric(length(time))
x2_vector <- numeric(length(time))
x3_vector <- numeric(length(time))
x4_vector <- numeric(length(time))
# Y
y_vector <- numeric(length(time))
y1_vector <- numeric(length(time))
y2_vector <- numeric(length(time))
y3_vector <- numeric(length(time))
y4_vector <- numeric(length(time))

# Calculating concentrations over time:
# Loop through time steps
for(i in 1:length(time)) {
  # Storing current concentration
  # X
  x_vector[i] <- x
  x1_vector[i] <- x1
  x2_vector[i] <- x2
  x3_vector[i] <- x3
  x4_vector[i] <- x4
  # Y
  y_vector[i] <- y
  y1_vector[i] <- y1
  y2_vector[i] <- y2
  y3_vector[i] <- y3
  y4_vector[i] <- y4
  # Calculating change in concentration
  change <- diff_func(x,y,k,mu)
  change1 <- diff_func(x1,y1,k,mu)
  change2 <- diff_func(x2,y2,k,mu)
  change3 <- diff_func(x3,y3,k,mu)
  change4 <- diff_func(x4,y4,k,mu)
  # Update concentrations based on change
  # X
  x <- x + change[[1]] * timestep
  x1 <- x1 + change1[[1]] * timestep
  x2 <- x2 + change2[[1]] * timestep
  x3 <- x3 + change3[[1]] * timestep
  x4 <- x4 + change4[[1]] * timestep
  # Y
  y <- y + change[[2]] * timestep
  y1 <- y1 + change1[[2]] * timestep
  y2 <- y2 + change2[[2]] * timestep
  y3 <- y3 + change3[[2]] * timestep
  y4 <- y4 + change4[[2]] * timestep
}



# i) Plot: Simulating X and Y concentrations over time ###
#    5 plots (5 different starting conditions)

# Initial condition: x=5, y=0
plot(time, x_vector, 
     type = "l", bty = "l", lwd = 1.5, col = "chartreuse2", 
     xlab = "Time", ylab = "Concentration", 
     main = "Concentrations of X and Y over time",
     xaxs="i", yaxs="i", ylim = c(0, max(x_vector, y_vector)))
lines(time, y_vector, lwd = 1.5, col = "green4")
mtext("Initial conditions: x = 5, y = 0", cex = 1.1)
legend(legend = c("X","Y"), 
       "topright", col = c("chartreuse2", "green4"), 
       lwd = 1.5, bty = "n")

# Initial condition: x=2.5, y=5
plot(time, x1_vector, 
     type = "l", bty = "l", lwd = 1.5, col = "chartreuse2", 
     xlab = "Time", ylab = "Concentration", 
     main = "Concentrations of X and Y over time",
     xaxs="i", yaxs="i", ylim = c(0, max(x1_vector, y1_vector) + 0.5))
lines(time, y1_vector, lwd = 1.5, col = "green4")
mtext("Initial conditions: x = 2.5, y = 5", cex = 1.1)
legend(legend = c("X","Y"), 
       "right", col = c("chartreuse2", "green4"), 
       lwd = 1.5, bty = "n")

# Initial condition: x=0.1, y=10
plot(time, x2_vector, 
     type = "l", bty = "l", lwd = 1.5, col = "chartreuse2", 
     xlab = "Time", ylab = "Concentration", 
     main = "Concentrations of X and Y over time",
     xaxs="i", yaxs="i", ylim = c(0, max(x2_vector, y2_vector) + 1))
lines(time, y2_vector, lwd = 1.5, col = "green4")
mtext("Initial conditions: x = 0.1, y = 10", cex = 1.1)
legend(legend = c("X","Y"), 
       "right", col = c("chartreuse2", "green4"), 
       lwd = 1.5, bty = "n")

# Initial condition: x=5, y=11
plot(time, x3_vector, 
     type = "l", bty = "l", lwd = 1.5, col = "chartreuse2", 
     xlab = "Time", ylab = "Concentration", 
     main = "Concentrations of X and Y over time",
     xaxs="i", yaxs="i", ylim = c(0, max(x3_vector, y3_vector) + 1))
lines(time, y3_vector, lwd = 1.5, col = "green4")
mtext("Initial conditions: x = 5, y = 11", cex = 1.1)
legend(legend = c("X","Y"), 
       "right", col = c("chartreuse2", "green4"), 
       lwd = 1.5, bty = "n")

# Initial condition: x=0.9, y=18
plot(time, x4_vector, 
     type = "l", bty = "l", lwd = 1.5, col = "chartreuse2", 
     xlab = "Time", ylab = "Concentration", 
     main = "Concentrations of X and Y over time",
     xaxs="i", yaxs="i", ylim = c(0, max(x4_vector, y4_vector) + 1))
lines(time, y4_vector, lwd = 1.5, col = "green4")
mtext("Initial conditions: x = 0.9, y = 18", cex = 1.1)
legend(legend = c("X","Y"), 
       "right", col = c("chartreuse2", "green4"), 
       lwd = 1.5, bty = "n")



# ii) Plot: Equilibria function with X and Y concentration simulations ###

# Calculating equilibria-values:
# y-function for equilibria
equilibria_y_func <- function(x,k,mu){
  y <- k*x^2/mu
  return(y)
}
# Setting a sequence of x-values:
x_values <- seq(0, 5, by = timestep)
# Calculating the corresponding y-values:
equilibria_y <- equilibria_y_func(x_values,k,mu)

# Plot:

# Equilibria
plot(x_values, equilibria_y, 
     type = "l", bty = "l", lwd = 1.7, col = "blue",
     xlab = "", ylab = "",
     xaxs="i",yaxs="i", ylim = c(0,20), xlim = c(0,5))
title(main = "X & Y concentration trajectories", line = 2.5)
mtext("X-concentration", side = 1, line = 2.5)
mtext("Y-concentration", side = 2, line = 2.5)
text(2.35, 19.8, "Equilibria", xpd = NA, cex = 0.9)

# Adding concentration trajectories
# Initial condition: x=5, y=0
 lines(x_vector, y_vector, type = "l", lwd = 1.4, col = "limegreen")
 text(x_vector[1], y_vector[1], "e", cex = 0.9, adj = c(1.7, -0.6))
# Initial condition: x=2.5, y=5
 lines(x1_vector, y1_vector, type = "l", lwd = 1.4, col = "limegreen")
 text(x1_vector[1], y1_vector[1], "d", cex = 0.9, adj = c(-1.1, 0.4))
# Initial condition: x=0.1, y=10
 lines(x2_vector, y2_vector, type = "l", lwd = 1.4, col = "limegreen")
 text(x2_vector[1], y2_vector[1], "c", cex = 0.9, adj = c(0, -0.5))
# Initial condition: x=5, y=11
 lines(x3_vector, y3_vector, type = "l", lwd = 1.4, col = "limegreen")
 text(x3_vector[10], y3_vector[10], "b", cex = 0.9, adj = c(-1.5, 1.5))
# Initial condition: x=0.9, y=18
 lines(x4_vector, y4_vector, type = "l", lwd = 1.4, col = "limegreen")
 text(x4_vector[1], y4_vector[1], "a", cex = 0.9, adj = c(1.5, 0.2))

# Legend
mtext("Initial conditions:", padj = 0.2, at = 4.5, cex = 0.9)
mtext("
      a: x = 0.9  y = 18
      b: x = 5     y = 11
      c: x = 0.1  y = 10
      d: x = 2.5  y =   5
      e: x = 5     y =   0", 
      adj = 0, padj = 1, at = 3.9, cex = 0.8)


