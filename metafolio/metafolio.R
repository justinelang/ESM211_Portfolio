library(metafolio)

## These are the references for the package! When you run these three lines of code the vignette will pop up in another window and the help window window in R will pop up in the bottom right corner.
vignette("metafolio")
?metafolio
help(package = "metafolio")


##.......................Part 1 - Creating Baseline Portfolios..........................................
arma_env_params <- list(mean_value = 16, # mean
                        ar = 0.1, # auto regressive parameter
                        sigma_env = 2, # std deviation of env signal
                        ma = 0) # moving average parameter
## We are simulating portfolios so there's no data needed for this! This function is helping create a base case scenario based on environmental parameters from the literature.

base1 <- meta_sim(n_pop = 10, # simulation with 10 populations
                  env_params = arma_env_params, # input environmental parameters that were created above
                  env_type = "arma", # use an arima model; run '?arima' in the console to learn more about arima models
                  assess_freq = 5) ## re-assess the fishery every five years.
## This is generating a predictive time series of the status of the simulated portfolio based on previous data (Which is why the fishery is being reassessed periodically).  

plot_sim_ts(base1, years_to_show = 70, burn = 30) ## Plotting the time series of the simulated metapopulations and their predictive environmental parameters over 70 years


##.......................Part 2 - Exploring Prioritization Strategies...................................
w_plans <- list() ## Making this a list to run a for loop over all our portfolios 

## We are going to manipulate the investment weights in each stream by changing the b_i parameter in the ricker model. Since b_i is the carrying capacity of each population, we will use the maximum value 1000.
w_plans[["balanced"]] <- c(5, 1000, 5, 1000, 5, 5, 1000, 5,1000, 5) ## Here we have 10 populations. 1000 means we want to conserve that population. 5 means we do not want to conserve the population. We are then choosing to conserve a balance metapopulation
                                                                    ## (Hint: Play around with these numbers to see how the portfolios change in the end)

w_plans[["one_half"]] <- c(rep(1000, 4), rep(5, 6)) ## Here we are conserving the first 4 populations. We are not conserving the other 6 populations.

w <- list() ## Making a list of stream capacities 
for(i in 1:2) { # loop over plans
  w[[i]] <- list()
  for(j in 1:80) { # loop over iterations
    w[[i]][[j]] <- matrix(w_plans[[i]], nrow = 1)
  }
}

set.seed(1)
arma_sp <- run_cons_plans(w, env_type = "arma", env_params = arma_env_params) ## Running the simulated portfolios using the different prioritization weights from above


plot_cons_plans(arma_sp$plans_mv,
                plans_name = c("Balanced", "One half"),
                cols = c("#E41A1C", "#377EB8"), xlab = "Variance of growth rate",
                ylab = "Mean growth rate") ## Plotting the simulated the portfolios


w_plans2 <- list()

w_plans2[["balanced"]] <- c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000) ## Here we have 10 populations. 1000 means we want to conserve that population. 5 means we do not want to conserve the population. We are then choosing to conserve a balance metapopulation
## (Hint: Play around with these numbers to see how the portfolios change in the end)

w_plans2[["one_half"]] <- c(rep(1000, 4), rep(5, 6))

w2 <- list() ## Making a list of stream capacities 
for(i in 1:2) { # loop over plans
  w2[[i]] <- list()
  for(j in 1:80) { # loop over iterations
    w2[[i]][[j]] <- matrix(w_plans2[[i]], nrow = 1)
  }
}

set.seed(1)
arma_sp2 <- run_cons_plans(w, env_type = "arma", env_params = arma_env_params)

plot_cons_plans(arma_sp2$plans_mv,
                plans_name = c("Balanced", "One half"),
                cols = c("#E41A1C", "#377EB8"), xlab = "Variance of growth rate",
                ylab = "Mean growth rate")


##.......................Part 3 - Optimizing metapopulation portfolios..................................

## Woohoo it's time to optimize those portfolios!!
set.seed(1)
weights_matrix <- create_asset_weights(n_pop = 10, n_sims = 3000, ## you can play around with the number of populations 
                                       weight_lower_limit = 0.001) ## generating weights matrix using 3000 simulations on 10 populations. The smallest weight that will be given is .001 

mc_ports <- monte_carlo_portfolios(weights_matrix = weights_matrix,
                                   n_sims = 3000, mean_b = 1000) ## the monte carlo sampling from possible investment weights based off the weights matrix from above to generate the most optimal portfolios.

col_pal <- rev(gg_color_hue(6)) ## this is just making a custom color palette
ef_dat <- plot_efficient_portfolios(port_vals = mc_ports$port_vals,
                                    pal = col_pal, weights_matrix = weights_matrix) ## plotting all simulated portfolios. The red portfolios are the optimal portfolios based on the monte carlo function.

ef_dat ## this contains more information on each optimal portfolio 
ef_dat$ef_weights ## the weights given for each portfolio







##.......................Extra - The code for the Ricker plots shown in class...................................
plot_rickers(base1, pal = rep("black", 10)) ## Spawners on x axis and Returns on y axis. Each panel represents 1 portfolio with their thermal tolerances.


