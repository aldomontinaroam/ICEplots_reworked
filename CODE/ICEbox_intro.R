library(ICEbox)
library(randomForest)
library(MASS)

# ----------------- Load BHD data -----------------
# CRIM - per capita crime rate by town
# ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS - proportion of non-retail business acres per town.
# CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
# NOX - nitric oxides concentration (parts per 10 million)
# RM - average number of rooms per dwelling
# AGE - proportion of owner-occupied units built prior to 1940
# DIS - weighted distances to five Boston employment centres
# RAD - index of accessibility to radial highways
# TAX - full-value property-tax rate per $10,000
# PTRATIO - pupil-teacher ratio by town
# B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# LSTAT - % lower status of the population
# MEDV - Median value of owner-occupied homes in $1000's
data(Boston)
X = Boston 
y = X$medv # target variable
X$medv = NULL #X contains only the independent variables

predictors <- names(X)
# Set up the plotting area for a grid layout
num_predictors = length(predictors)
num_cols = 3 # number of columns in the layout
num_rows = ceiling(num_predictors / num_cols)

# RF MODEL with default settings
rf_mod = randomForest(X, y)

# ----------------- ICE and c-ICE Plots -----------------

# creating the .ice object
rf.ice = ice(rf_mod, X, y, predictor = "age", frac_to_build = 1)

par(mar = c(5.0, 5, 1.4, 2.5), mgp = c(3, 1.3, 0), pty= "s")
plot(rf.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = .2,
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.6,
     main = "ICE Plot")

# plot c-ICE: 'centered=TRUE'
par(mar = c(5.0, 5, 1.4, 2.5), mgp = c(3, 1.3, 0), pty = "s")
plot(rf.ice, x_quantile = TRUE, plot_pdp = TRUE, centered = TRUE,frac_to_plot=.2,
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.6,
     main = "c-ICE Plot")

rf.ice$Xice$I_rm = ifelse(rf.ice$Xice$rm > 6.2, 1, 0) 

# then plot using 'color_by'.
par(mar = c(4.3, 4.5, 0.5, 2.5), mgp = c(3, 1.3, 0))
plot(rf.ice, frac_to_plot = .3, centered = TRUE, prop_range_y = TRUE,  
     x_quantile = TRUE, plot_orig_pts_preds = T, color_by = "I_rm",
     cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0)

# ----------------- d-ICE Plots -----------------

par(mfrow = c(num_rows, num_cols), mar = c(2, 2, 2, 2))
# create the d-ICE object and plot for each predictor:
for (predictor in predictors) {
  rf.ice = ice(rf_mod, X, y, predictor = predictor, frac_to_build = 1)
  rf.dice = dice(rf.ice)
  plot(rf.dice, main = paste("DICE for", predictor))
}

# ----------------- Visualizing a second feature -----------------

# We investigate the c-ICE  by coloring it by the "rm" variable. 
# First create an indicator variable based on "rm":
rm_threshold <- median(X$rm, na.rm = TRUE)
rf.ice$Xice$I_rm = ifelse(rf.ice$Xice$rm > rm_threshold, 1, 0)  

# Create a color vector that matches the number of rows in Xice:
colorvec = ifelse(rf.ice$Xice$I_rm == 1, "red", "blue")

par(mfrow = c(num_rows, num_cols), mar = c(2, 2, 2, 2)) # setting margins to fit all plots
# create the c-ICE object and plot for each predictor coloring by "I_rm"
for (predictor in predictors) {
  rf.ice = ice(rf_mod, X, y, predictor = predictor, frac_to_build = 1)
  rf.ice$Xice$I_rm = ifelse(rf.ice$Xice$rm > rm_threshold, 1, 0)  
  plot(rf.ice, main = paste("ICE for", predictor), frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE, plot_orig_pts_preds = TRUE, color_by = "I_rm", colorvec = colorvec)
}

par(mfrow = c(num_rows, num_cols), mar = c(2, 2, 2, 2)) # setting margins to fit all plots
# create the d-ICE object and plot for each predictor:
for (predictor in predictors) {
  rf.ice = ice(rf_mod, X, y, predictor = predictor, frac_to_build = 1)
  rf.ice$Xice$I_rm = ifelse(rf.ice$Xice$rm > rm_threshold, 1, 0)  
  rf.dice = dice(rf.ice)
  plot(rf.dice, main = paste("DICE for", predictor), frac_to_plot = 1, prop_range_y = TRUE, plot_orig_pts_preds = TRUE, color_by = "I_rm", colorvec = colorvec)
}