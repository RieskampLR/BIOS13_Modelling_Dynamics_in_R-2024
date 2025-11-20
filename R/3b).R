# BIOS13 Exam - Lea Rachel Rieskamp
# 3.
rm(list=ls())

# b): Function for recombined offspring genome ####

# User info:
# p1 and p2 are vectors for parent genomes and have to be of same length

# Recombined offspring genome function:
offspring_genome <- function(p1,p2) {
  # Empty vector for offspring genome
  offspring <- c()
  # p1 genome length
  L <- length(p1)
  # Random crossover point
  crossover <- sample.int(L-1, 1)
  # Recombining parent genomes to offspring genome
  offspring <- c(p1[1:crossover], p2[(crossover+1):L])
  return(offspring)
}

