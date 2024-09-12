library(MASS)           # Loads Pima dataset
library(xgboost)        # Builds and trains the XGBoost model
library(ggplot2)        # Creates visualizations
library(RColorBrewer)   # Provides color palettes for plots
library(lime)           # Explains model predictions with LIME
library(reshape2)       # Reshapes data for ICE plots
library(ICEbox)         # ICE Toolbox library for ICE plots
library(SHAPforxgboost) # Generates SHAP values for XGBoost models

####### Diabetes Classification in Pima Indians ###########
data <- Pima.te

# Split the data into training and testing sets
set.seed(17)
train_indices <- sample(1:nrow(data), nrow(data)*0.8) # 80% of the data in the training set
data_train <- data[train_indices, ]
data_test <- data[-train_indices, ]
y_train <- data_train$type
y_test <- data_test$type

# Global color palette for features
num_predictors <- length(colnames(data_train)) - 1  # Exclude the target variable
palette <- brewer.pal(n = num_predictors, name = "Paired")
# Assign the palette to each predictor for consistent color use across all plots
feature_colors <- setNames(palette, colnames(data_train)[-which(names(data_train) == "type")])
# create a named vector of colors for the features
colors_to_use <- setNames(palette, c("glu", "age", "ped", "bmi", "skin", "bp", "npreg"))

# Create XGBoost model
xgb_model <- xgboost(data = as.matrix(data_train[, -which(names(data_train) == "type")]),
                     label = as.numeric(data_train$type) - 1,
                     nrounds = 100, # Number of boosting rounds
                     objective = "binary:logistic")

# Make predictions for xgbost model on test data
predictions <- predict(xgb_model, as.matrix(data_test[, -which(names(data_test) == "type")]))
# since target variable is binary, we need to convert the predicted probabilities to class labels
predictions <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the model
accuracy <- mean(predictions == y_test)
print(paste("Accuracy:", accuracy))

# Plot of feature importance of the XGBoost model
xgb.importance <- xgb.importance(model = xgb_model)
sorted_features_xgb <- sort(xgb.importance$Feature, decreasing = TRUE)

# Ensure the 'Feature' column is a factor with levels ordered by 'Gain'
xgb.importance$Feature <- factor(xgb.importance$Feature, levels = xgb.importance$Feature[order(xgb.importance$Gain, decreasing = FALSE)])

# Plotting feature importance using ggplot2
ggplot(xgb.importance, aes(x = Feature, y = Gain, fill = Feature)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = colors_to_use) +
  theme_minimal() +
  coord_flip() +
  ggtitle("XGBoost Model Feature Importance")

# Save the plot
ggsave("Pima_XGB_Feature_Importance.png", width = 10, height = 6, dpi = 300)

####### LIME ########
# Custom prediction function for LIME
predict_fn <- function(model, newdata) {
  predictions <- predict(model, as.matrix(newdata[, -which(names(newdata) == "type")]))
  return(as.data.frame(predictions))
}

# Wrapper for the Random Forest model
classifier_wrapper <- as_classifier(xgb_model, predict_fn)

explainer <- lime(data_train[, -which(names(data_train) == "type")], 
                  model = classifier_wrapper, 
                  bin_continuous = TRUE)

explanations <- lime::explain(data_test[, -which(names(data_test) == "type")], 
                              explainer, 
                              n_labels = 1, 
                              n_features = ncol(data_test) - 1)

# Create a data frame to store the aggregated results
aggregated_contributions <- data.frame(matrix(ncol = ncol(data_test) - 1, nrow = nrow(data_test)))
colnames(aggregated_contributions) <- colnames(data_test)[-which(names(data_test) == "type")]

# Initialize the contributions
aggregated_contributions[] <- 0

# Loop through each row in the test set
for (i in 1:nrow(data_test)) {
  # Extract the explanation for the current instance
  instance_explanation <- explanations[explanations$case == i, ]
  
  # Loop through each feature
  for (feature in colnames(aggregated_contributions)) {
    # Find the contribution for this feature
    feature_contribution <- instance_explanation$feature_weight[instance_explanation$feature == feature]
    
    # If the feature wasn't in the top features explained by LIME, set its contribution to 0
    if (length(feature_contribution) == 0) {
      feature_contribution <- 0
    } else {
      feature_contribution <- feature_contribution
    }
    
    # Store the contribution in the aggregated_contributions data frame
    aggregated_contributions[i, feature] <- feature_contribution
  }
}

# Calculate the mean absolute contribution for each feature
mean_abs_contributions <- colMeans(abs(aggregated_contributions))

# Sort features by their mean absolute contribution
sorted_features <- sort(mean_abs_contributions, decreasing = TRUE)

# Print the sorted features and their mean absolute contributions
print(sorted_features)

# create a data frame for plotting sorted by Score
sorted_features_df <- data.frame(Feature = names(sorted_features), Score = sorted_features)
# Ensure the 'Feature' column is a factor with levels ordered by 'Score'
sorted_features_df$Feature <- factor(sorted_features_df$Feature, levels = sorted_features_df$Feature[order(sorted_features_df$Score, decreasing = FALSE)])

ggplot(sorted_features_df, aes(x = Feature, y = Score, fill = Feature)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = colors_to_use) +
  theme_minimal() +
  coord_flip() +
  ggtitle("Mean Absolute LIME Feature Contributions")

# Save the plot
ggsave("Pima_LIME_Feature_Contributions.png", width = 10, height = 6, dpi = 300)

######## SHAP ########
X_shap <- as.matrix(data_train[, -which(names(data_train) == "type")])
shap_values <- shap.values(xgb_model = xgb_model, 
                           X_train = X_shap)
shap_values$mean_shap_score

shap_values_pima <- shap_values$shap_score
# shap.prep() returns the long-format SHAP data from either model or
shap_long_pima <- shap.prep(xgb_model = xgb_model, X_train = X_shap)
# is the same as: using given shap_contrib
shap_long_pima <- shap.prep(shap_contrib = shap_values_pima, X_train = X_shap)

# SUMMARY PLOT
shap.plot.summary(shap_long_pima, min_color_bound = "brown2", max_color_bound = "turquoise2")
# Save plot
ggsave("Pima_SHAP_Summary_Plot.png", width = 10, height = 6, dpi = 300)

# SHAP FORCE PLOT
plot_data_forceshap <- shap.prep.stack.data(shap_contrib = shap_values_pima,
                                            n_groups = 4)
shap.plot.force_plot(plot_data_forceshap)
# Save plot
ggsave("Pima_SHAP_Force_Plot.png", width = 10, height = 6, dpi = 300)


######### ICEbox #########
# For classification we plot the centered log-odds. If we pass a predict
# function that returns fitted probabilities, setting logodds = TRUE instructs
# the function to set each ice curve to the centered log-odds of the fitted 
# probability.
predictors <- colnames(data_train)[1:7]
# Determine the number of rows and columns for the grid
num_plots <- length(predictors)
grid_rows <- ceiling(sqrt(num_plots))
grid_cols <- ceiling(num_plots / grid_rows)

# Set up plotting for ICE plots
par(mfrow = c(grid_rows, grid_cols), mar = c(2, 2, 2, 2))

# Loop over predictors for ICE plots
for (predictor in predictors) {
  rf.ice = ice(xgb_model, X = data_test[1:7], 
               predictor = predictor, frac_to_build = 1,
               predictfcn = function(object, newdata) {
                 newdata_dmatrix = xgb.DMatrix(as.matrix(newdata))
                 pred_output = predict(object, newdata_dmatrix, type = "prob")
                 if (is.matrix(pred_output)) {
                   pred_output[, 2]
                 } else {
                   pred_output
                 }
               })
  plot(rf.ice, 
       main = paste("ICE for", predictor), 
       x_quantile = TRUE, 
       centered = TRUE,
       plot_sd = FALSE) 
}

par(mfrow = c(grid_rows, grid_cols), mar = c(2, 2, 2, 2))

# Similarly for DICE plots:
for (predictor in predictors) {
  rf.ice = ice(xgb_model, X = data_test[1:7], 
               predictor = predictor, frac_to_build = 1,
               predictfcn = function(object, newdata) {
                 newdata_dmatrix = xgb.DMatrix(as.matrix(newdata))
                 pred_output = predict(object, newdata_dmatrix, type = "prob")
                 if (is.matrix(pred_output)) {
                   pred_output[, 2]
                 } else {
                   pred_output
                 }
               })
  rf.dice = dice(rf.ice)
  plot(rf.dice, main = paste("DICE for", predictor), x_quantile = TRUE)  # Apply color
}

# Reset plotting layout
par(mfrow = c(1, 1))

####### ICE-based feature importance ########
feature_names <- colnames(data_test)[1:7]  # Adjust as needed
X <- as.matrix(data_test[, feature_names])

# Function to compute ICE-based feature importance
compute_feature_importance <- function(feature_name) {
  X_matrix <- as.matrix(X)
  
  # Create ICE object for the feature in input
  pima.ice <- ice(xgb_model, X = X_matrix, predictor = feature_name, logodds = TRUE,
                  predictfcn = function(object, newdata){
                    newdata_matrix <- as.matrix(newdata)
                    pred <- predict(object, newdata_matrix, type = "prob")
                    if (is.vector(pred)) {
                      return(pred)
                    }
                    # Probability of the positive class as response
                    return(pred[, 2])
                  })
  
  # Manipulate the ICE data for feature importance calculation
  ice_data <- as.data.frame(pima.ice$ice_curves)
  ice_data_long <- melt(ice_data, id.vars = NULL, variable.name = "grid_point", value.name = "value")
  ice_data_long$predictor <- rep(pima.ice$grid, each = nrow(X_matrix)) # Adds values of the predictor for each observation
  ice_data_long$id <- as.factor(rep(1:nrow(X_matrix), times = length(pima.ice$grid))) # Adds an ID for each observation
  
  # Compute density of the feature values
  feature_values <- X_matrix[, feature_name]
  feature_density <- density(feature_values) # compute density of the feature values
  density_scaled <- scales::rescale(feature_density$y, to = c(0, 1)) # scale densities to [0, 1]
  density_at_values <- approx(x = feature_density$x, y = density_scaled, xout = feature_values)$y # search for density at real feature values
  ice_data_long$density <- rep(density_at_values, times = length(pima.ice$grid)) # add density column
  
  high_density_threshold <- 0.8 # we consider high-density regions those with density > 0.8
  
  # Compute the mean variability of the feature in high-density regions using the standard deviation
  mean_variability <- aggregate(value ~ id, data = ice_data_long[ice_data_long$density > high_density_threshold, ], FUN = sd)
  mean_variability_score <- mean(mean_variability$value, na.rm = TRUE) # mean of the standard deviations
  
  return(mean_variability_score)
}

# Compute ICE-based importance using the function above
importance_scores <- sapply(feature_names, compute_feature_importance)

# Create a data frame for plotting
plot_data <- data.frame(
  Feature = names(importance_scores),
  Importance = importance_scores
)

# Sort by importance
plot_data <- plot_data[order(-plot_data$Importance), ]

# Create the bar plot with colors from the feature_colors palette
ggplot(plot_data, aes(x = reorder(Feature, Importance), y = Importance, fill = Feature)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_to_use) +  # Apply custom colors
  coord_flip() +  # Flip coordinates for horizontal bars
  labs(title = "ICE-based Feature Importance",
       x = "Features",
       y = "Importance Score") +
  theme_minimal()

# Save the plot
ggsave("Pima_ICE_Feature_Importance.png", width = 10, height = 6, dpi = 300)