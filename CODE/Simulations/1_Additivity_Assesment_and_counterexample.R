# Load necessary libraries
library(gam)
library(ICEbox)

# Function that generates simulated data:
additivity_ex_sim = function(n, seednum = NULL) {
  if (!is.null(seednum)) {
    set.seed(seednum)
  }
  p = 2
  X = as.data.frame(matrix(runif(n * p, -1, 1), ncol = p))	
  colnames(X) = paste("x_", 1 : p, sep = "")
  bbeta = c(1, 1)
  
  y = bbeta[1] * X[,1]^2 + bbeta[2] * X[,2]
  y = y + rnorm(n)
  Xy = as.data.frame(cbind(X, y))
  return(list(Xy = Xy, X = X, y = y))
}

# Generate data:
additivity_ex_data = additivity_ex_sim(1000)
Xy = additivity_ex_data$Xy
X  = additivity_ex_data$X
y  = additivity_ex_data$y

# Build GAM with possible interactions:
gam_mod = gam(y ~ s(x_1) + s(x_2) + s(x_1 * x_2), data = Xy)

# Build ICE and d-ICE:
gam.ice = ice(gam_mod, X, predictor = 1, frac_to_build = 1)
gam.dice = dice(gam.ice)

# Plot the ICE and d-ICE with pdp:
plot(gam.ice, x_quantile = F, plot_pdp = T, frac_to_plot = 0.1)  
plot(gam.dice, x_quantile = F, plot_dpdp = T, frac_to_plot = 0.1)


########## Counterexample ################
# How does the plot look like if the model isn't additive?

# Function that generates simulated data:
non_additivity_ex_sim = function(n, seednum = NULL) {
  if (!is.null(seednum)) {
    set.seed(seednum)
  }
  p = 2
  X = as.data.frame(matrix(runif(n * p, -1, 1), ncol = p))	
  colnames(X) = paste("x_", 1 : p, sep = "")
  bbeta = c(1, 1)
  
  non_additive_y = bbeta[1] * X[,1]^2 + bbeta[2] * (X[,2]*X[,1]) # the function isn't additive anymore
  non_additive_y = non_additive_y + rnorm(n)
  Xnon_additive_y = as.data.frame(cbind(X, non_additive_y))
  return(list(Xnon_additive_y = Xnon_additive_y, X = X, non_additive_y = non_additive_y))
}

# Generate data:
non_additivity_ex_data = non_additivity_ex_sim(1000)
Xnon_additive_y = non_additivity_ex_data$Xnon_additive_y
X  = non_additivity_ex_data$X
non_additive_y = non_additivity_ex_data$non_additive_y

# Build GAM with possible interactions:
gam_mod = gam(non_additive_y ~ s(x_1) + s(x_2) + s(x_1 * x_2), data = Xnon_additive_y)

# Build ICE and d-ICE:
gam.ice = ice(gam_mod, X, predictor = 1, frac_to_build = 1)
gam.dice = dice(gam.ice)

# Plot the ICE and d-ICE with pdp:
plot(gam.ice, x_quantile = F, plot_pdp = T, frac_to_plot = 0.1)  
plot(gam.dice, x_quantile = F, plot_dpdp = T, frac_to_plot = 0.1)