# BIOS13 Exam - Lea Rachel Rieskamp
# 3.
rm(list=ls())

# c): Generations simulation ####

# Function from b):
offspring_genome <- function(p1,p2) {
  offspring <- c()
  L <- length(p1)
  crossover <- sample.int(L-1, 1)
  offspring <- c(p1[1:crossover], p2[(crossover+1):L])
  return(offspring)
}

# Setting parameters:
n <- 100  # Number of individuals
L <- 100  # Genome length
g <- 100  # Generations

# Initializing the starting population genomes:
# Empty list of vectors
genomes <- vector("list", n)
# Adding the individual with the new genome
genomes[[1]] <- rep(1, L)
# Adding the individuals with the old genome
for (i in 2:n) {
  genomes[[i]] <- rep(0, L)
}

# Initializing a list for storing all generations:
all_generations <- list()
# Adding the starting population
all_generations[[1]] <- genomes

# Simulating generations:
# Loop through number of generations
for (i in 1:g) {
  # Empty vector for new generation
  newgeneration <- vector("list")
  # Loop until new generation has n individuals
  while (length(newgeneration) < n) {
    # Random mating pairs
    randomnumber1 <- sample.int(n-1,1)
    randomnumber2 <- sample.int(n-1,1)
    # Ensuring parents are 2 different individuals
    while (randomnumber1 == randomnumber2) {
      randomnumber2 <- sample.int(n-1,1)
    }
    # Calling the parent´s genomes
    p1_genome <- genomes[[randomnumber1]]
    p2_genome <- genomes[[randomnumber2]]
    # Creating the recombined offspring genome
    offspring <- offspring_genome(p1_genome, p2_genome)
    # Adding the offspring to the new generation
    newgeneration[[length(newgeneration) + 1]] <- offspring
  }
  # Storing the current generation
  all_generations[[i+1]] <- newgeneration
  # Reassigning the genomes (new generation becomes parent generation)
  genomes <- newgeneration
}


