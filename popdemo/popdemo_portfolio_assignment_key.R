# popdemo portfolio assignment key
# Steven Mitchell
# 15 Feb 2024
################################################################################

# package
library(popdemo)

# look at the polar bear data
Pbear
Pbear_vector <- c(0.106, 0.068, 0.106, 0.461, 0.151, 0.108) # from paper

# Task 1:
# Define a new matrix to feed into Aseq that takes the previous year’s sea ice
# status into account.
# Make good years more likely if the previous year was good (use 20% likelihood),
# and conversely # makes bad ice years more likely if the previous year was
# bad (use 80% likelihood).

p4 <- 0.2 # probability next year is a bad year given that this year was a good year
q4 <- 0.8 # probability next year is a bad year given that this year was a bad year
Pbear_task1_matrix <- matrix(c(rep(c((1-p4)/3, (1-p4)/3, (1-p4)/3, p4/2, p4/2), 3),
                      rep(c((1-q4)/3, (1-q4)/3, (1-q4)/3, q4/2, q4/2), 2)), 5, 5)

Pbear_task1_projection <- project(Pbear, Pbear_vector, Aseq = Pbear_task1_matrix, time = 50)
plot(Pbear_task1_projection)


# Task 2:
# Define 2 new parameters which describe the probability next year is a bad year
# given that this year was a good  (use 50% likelihood), and (b) describe the
# probability next year is a bad year given that this year was a bad year (use
# 80% likelihood again).

p5 <- 0.5 # probability next year is a bad year given that this year was a good year
q5 <- 0.8 # arbitrary object names
Pbear_task2_matrix <- matrix(c(rep(c((1-p5)/3, (1-p5)/3, (1-p5)/3, p5/2, p5/2), 3),
                      rep(c((1-q5)/3, (1-q5)/3, (1-q5)/3, q5/2, q5/2), 2)), 5, 5)

Pbear_task2_projection <- project(Pbear, Pbear_vector, Aseq = Pbear_task2_matrix, time = 50)
plot(Pbear_task1_projection)
