# Two species using the same resources but with different rates of efficiency

# A) Define the functions without competition

## Function for population growth of one species
growth_single_species <- function(growth.rate, K, num_gen = 50) {
  N <- rep(0, num_gen)
  N[1] <- 10
  for (i in 2:num_gen) {
    N[i] <- N[i-1] + (growth.rate * N[i-1] * (K - N[i-1]) / K)
  }
  return(N)
}

# Plotting single species growth
growth_rate <- 0.8
K1 <- 100  
K2 <- 100  
num_gen <- 50

# Species 1 and 2
N1_single <- growth_single_species(growth_rate, K1, num_gen)
N2_single <- growth_single_species(growth_rate, K2, num_gen)

# Plot

# Plot for Species 1 Growth
plot(N1_single, type = "b", col = "black", xlab = "Generation", ylab = "Population",
     main = "Species 1 Growth (alone)")

# Plot for Species 2 Growth
plot(N2_single, type = "b", col = "red", xlab = "Generation", ylab = "Population",
     main = "Species 2 Growth (alone)")

# B) Define the function with competition
growth_competing_species <- function(growth.rate, K1, K2, alpha12, alpha21, num_gen = 50) {
  N1 <- rep(0, num_gen)
  N2 <- rep(0, num_gen)
  N1[1] <- 10
  N2[1] <- 10
  for (i in 2:num_gen) {
    N1[i] <- N1[i-1] + (growth.rate * N1[i-1] * (K1 - N1[i-1] - alpha12 * N2[i-1]) / K1)
    N2[i] <- N2[i-1] + (growth.rate * N2[i-1] * (K2 - N2[i-1] - alpha21 * N1[i-1]) / K2)
  }
  return(list(N1 = N1, N2 = N2))
}

# Parameters for competition
alpha12 <- 1  
alpha21 <- 1

competing_result <- growth_competing_species(growth_rate, K1, K2, alpha12, alpha21, num_gen)

# Plot
plot(competing_result$N1, type = "b", col = "black", xlab = "Generation", ylab = "Population", 
     main = "Both Species Competing")
lines(competing_result$N2, col = "red")

#Create an animation
saveGIF({
  for (i in 1:num_gen) {
    plot(competing_result$N1[1:i], type = "b", col = "black", xlab = "Generation", ylab = "Population", 
         xlim = c(1, num_gen), ylim = c(0, max(K1, K2)), 
         main = paste("Generation", i))
    lines(competing_result$N2[1:i], col = "red")
    legend("topright", legend = c("Species 1", "Species 2"), col = c("black", "red"), lty = 1)
  }
}, movie.name = "population_growth.gif", interval = 0.1)







