library(gbm)
library(ICEbox)
library(knitr)
library(caret)

############ SIMULATED DATA ############

# Function to generate simulated data
sim_data_genf = function(n, seednum=NULL){
  if(!is.null(seednum)){
    set.seed(seednum)
  }
  
  p = 3 # Define the number of features (predictors)
  X = matrix(runif(n * p, -1, 1), ncol = p) # Generate a matrix of uniformly distributed random numbers between -1 and 1 with dimensions (n x p)
  colnames(X) = paste("x_", 1 : p , sep = "") # Name the columns of matrix as "x_1", "x_2", ..., "x_p" using column index and paste function
  
  coeffmat_pos = as.matrix(c(0.2, 5, 0)) # Coefficient matrix for when X_3 >= 0
  coeffmat_neg = as.matrix(c(0.2, -5, 0)) # Coefficients matrix for when X_3 < 0
  
  y = array(NA, n) # Create an empty array of size n to store the response variable
  
  # Loop over each observation in the dataset (i.e., each row in 'X'):
  # - if X_3>=0, computes the response variable as the dot product of the 
  #   coefficient matrix for X_3 >= 0 (coeffmat_pos) and the corresponding 
  #   row of the feature matrix X, plus a random noise term
  # - if X_3<0, computes the response variable as the dot product of the
  #   coefficient matrix for X_3 < 0 (coeffmat_neg) and the corresponding
  #   row of the feature matrix X, plus a random noise term
  for (i in 1 : n){
    if (X[i, 3] >= 0 ){
      y[i] = X[i, ] %*% coeffmat_pos + rnorm(n, mean=0, sd=1)
    } else {
      y[i] = X[i, ] %*% coeffmat_neg + rnorm(n, mean=0, sd=1)
    }
  }
  
  # Combine feature matrix and response variable into a single data frame with proper column names
  Xy = as.data.frame(cbind(X, y)) 
  colnames(Xy) = c(paste("x_", 1 : p , sep = ""), "y")
  
  X = as.data.frame(X)
  colnames(X) = paste("x_", 1 : p , sep = "")
  
  return(list(Xy=Xy, X=X, y=y)) # Return the data frame, feature matrix (as df), and response variable
}

# Generate simulated data
sim_data = sim_data_genf(1000, seednum=123) # Generate 1000 observations with seed number 123
df = sim_data$Xy
X = sim_data$X
y = sim_data$y

############ MODEL ############
# Set up a grid of hyperparameters for tuning the GBM model
grid <- expand.grid(
  n.trees = c(50, 500, 1000),          # Number of trees
  interaction.depth = c(1, 3, 5),      # Depth of trees
  shrinkage = c(0.01, 0.1),            # Learning rate
  n.minobsinnode = c(10, 20, 30)       # Minimum number of observations in the terminal node
)


# Set up trainControl for cross-validation
train_control <- trainControl(
  method = "cv",       # Cross-validation
  number = 5,          # 5-fold cross-validation
  verboseIter = TRUE   # Show training progress
)

gbm_model <- train(
  y ~ .,                     # Formula (predict y using all other variables)
  data = df,                 # Data frame containing the features and target variable
  method = "gbm",            # GBM method from caret
  trControl = train_control, # Use cross-validation settings
  tuneGrid = grid,           # Provide the grid of hyperparameters to search over
  verbose = FALSE            # Suppress gbm's own output
)

# Print the best hyperparameters
print(gbm_model$bestTune)

# Summary of the final model
print(gbm_model)

# Plot performance across the grid of hyperparameters
plot(gbm_model)

# Define a model using the best hyperparameters
gbm <- gbm_model$finalModel

############ ICE Plots ############
# Create and plot ICE object:
gbm.ice <- ice(gbm, X, predictor = "x_3")
plot(gbm.ice, x_quantile = FALSE, plot_orig_pts_deriv = FALSE)
# add title
title("ICE Plot for x_3")

# Create and plot d-ICE object:
gbm.dice = dice(gbm.ice)
plot(gbm.dice, x_quantile = FALSE, plot_orig_pts_deriv = FALSE)
# add title
title("d-ICE Plot for x_3")

