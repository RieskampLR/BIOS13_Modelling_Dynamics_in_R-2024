# BIOS13 Exam - Lea Rachel Rieskamp
# 2.
rm(list=ls())

# c): Plotting dn/dt of n ####

# Growth function dn/dt:
growth_function <- function(n,r) {
  (r - k*n^2) * n
}

# Setting parameters:
r1 <- 1      # Intrinsic growth rate
k <- 0.0001  # Strength of density dependence

# Creating a sequence of n1 values:
n1_values <- seq(0, 130, by = 0.1)

# Calculating the corresponding dn1/dt values:
dn1_dt_values <- growth_function(n1_values,r1)

# Assigning the (non-trivial) equilibrium point:
nt_equilibrium <- sqrt(r1/k)

# Plotting dn1/dt of n1 & the equilibrium point:
plot(n1_values, dn1_dt_values,
     type = "l", bty = "l", lwd = 1.5, col = "cadetblue2",
     xlab = "Population Size (n1)", ylab = "Growth rate (dn1/dt)",
     main = "Growth dynamics of population 1",
     xaxs = "i", yaxs = "i", xlim = c(0, 130), ylim = c(-10, 45))
# Line for growth rate of 0
abline(h = 0, col = "black")
# Equilibrium marker
points(nt_equilibrium, 0, pch = 1, cex = 1.3, col = "black")
text(nt_equilibrium, 0, "Non-trivial\nEquilibrium", adj = c(-0.1, -0.4), cex = 0.9)


