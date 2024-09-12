library(ICEbox)
library(randomForest)
library(MASS) # Includes the Pima dataset

# Load the Pima dataset and split into predictors and response
df <- Pima.te
X <- df[, !(names(df) %in% "type")]  # All columns except "type"
y <- df[, "type"]  # Response variable

# Random Forest model
rf <- randomForest(x = X, y = y, importance = TRUE, ntree = 1000, mtry = 2)

# Compute the AGE threshold as median age
age_threshold <- median(X$age, na.rm = TRUE)
# Create a new indicator variable based on the threshold -> use in color_by
X$age_ind <- ifelse(X$age > age_threshold, 1, 0)

# Create ICE object
pima_ice <- ice(
  object = rf, X = X, 
  predictor = "skin", 
  logodds = TRUE,
  predictfcn = function(object, newdata) {
    predict(object, newdata, type = "prob")[, 2]
    }
  )

par(mfrow = c(1, 2)) # Set up a 1x2 grid for plotting

# Plot centered ICE (c-ICE) curves with quantile-based x-axis scaling
plot(pima_ice, x_quantile = TRUE, centered = TRUE)
title(main = "Centered ICE Curves (c-ICE) - Pima dataset")

# Create and plot the derivative ICE (d-ICE) curves
pima_dice <- dice(pima_ice)
plot(pima_dice, x_quantile = TRUE, color_by = "age_ind")
# add legend for age_ind
legend("topright", legend = c(paste("age < ", age_threshold), paste("age >= ", age_threshold)), fill = c("red", "blue"))
title(main = "Derivative ICE Curves (d-ICE) - Pima dataset")

