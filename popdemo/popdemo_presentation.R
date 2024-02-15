# popdemo demonstration
# Steven Mitchell
# 15 Feb 2024
################################################################################

# package
library(popdemo)

# Deterministic Projection ----

# look at the tortoise data
Tort

# run a deterministic projection
tortoise_vector <- runif(8) # generating a random population vector
tortoise_vector <- tortoise_vector/sum(tortoise_vector) #scales the vector to sum to 1
tortoise_projection_100 <- project(Tort, tortoise_vector, time = 100)
print(tortoise_projection_100)

# extract the separate life-stage counts (just the first 10)
vec(tortoise_projection_100)[1:11, ]

# plot it using the built-in function
plot(tortoise_projection_100)




# Stochastic Projection ---

# look at the polar bear data
Pbear
Pbear_vector <- c(0.106, 0.068, 0.106, 0.461, 0.151, 0.108) # from paper

# run a 50 year stochastic projection with even probability of selecting each matrix
Pbear_projection_random <- project(Pbear, Pbear_vector, time = 50)
plot(Pbear_projection_random)

# look at the matrices chosen in the above model
Aseq(Pbear_projection_random)

# specify Aseq matrix with good and bad ice years with their probabilities
p3 <- 0.8 # probability of a bad ice year
bad_years_4x_likely <- matrix(
  rep(c((1-p3)/3, (1-p3)/3, (1-p3)/3, p3/2, p3/2), 5), 5, 5)
bad_years_4x_likely

# call this matrix into our project() function using Aseq argument
Pbear_projection_badyears4xlikely <- project(Pbear, Pbear_vector,
                                            Aseq = bad_years_4x_likely,
                                            time = 50)
Aseq(Pbear_projection_badyears4xlikely)
plot(Pbear_projection_badyears4xlikely)
