# BIOS13 Exam - Lea Rachel Rieskamp
# 3.
rm(list=ls())

# d): Plotting the frequencies of new genes over time (generations) ####
# Additions to the script from c) are marked with: ###### d) ######

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

# Empty vector for frequencies of new genes:                   ###### d) ######
newgene_freqs <- c()                                           ###### d) ######

# Simulating generations:
# Loop through number of generations
for (i in 1:g) {
  #                                                            ###### d) ######
  # Calculating the frequency of new genes in the current generation          #
  genes_total <- length(unlist(genomes))                                      #
  newgenes <- sum(unlist(genomes) == 1)                                       #
  newgene_freq <- newgenes/genes_total                                        #
  # Adding the current frequency to the vector                                #
  newgene_freqs <- c(newgene_freqs,newgene_freq)                              #
  #                                                            ###### d) ######
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

#                                                             ###### d) ######
# Plotting the new gene frequencies over time (generations)                  #
plot(newgene_freqs,                                                          #
     type="l", lwd = 1.5, col = "cadetblue3",                                #
     xlab="Time (generations)", ylab="Proportion of new genes",              #
     main="Proportion of new genes over generations",                        #
     xaxs="i",yaxs="i", xaxt = "n", yaxt = "n",                              #
     ylim = c(0, max(newgene_freqs) + 0.01))                                 #
# Axes adjustments                                                           #
axis(1, at = seq(0, g, by = 10),                                             #
     tck = 0.02, mgp = c(2, 0.5, 0), cex.axis = 0.8)                         #
axis(2, at = seq(0, max(newgene_freqs) + 0.01, by = 0.01), las = 2,          #
     tck = 0.02, mgp = c(2, 0.5, 0), cex.axis = 0.8)                         #
axis(3, at = seq(0, g, by = 10),                                             #
     tck = 0.02, labels = FALSE)                                             #
axis(4, at = seq(0, max(newgene_freqs)+0.01, by = 0.01),                     #
     tck = 0.02, labels = FALSE)                                             #
#                                                             ###### d) ######
