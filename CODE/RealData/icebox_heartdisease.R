################################################################################
############### My experiments with the Heart Disease Dataset ##################
################################################################################

library(ICEbox)

# loading dataset
library(kmed)
data(heart)
# View(heart)
summary(heart)

# converting into binary classification
heart$class <- ifelse(heart$class == 0, 0, 1)
unique(heart$class)

# classification using random forest
library(randomForest)
X = heart
y <- as.factor(X$class) # Ensure target variable is a factor
X$class = NULL

# converting into binary classification
X$cp <- ifelse(X$cp == 4, 0, 1)

h_rf = randomForest(X, y)

################# Using Age and Sex as predictors #############################

# For classification we plot the centered log-odds. If we pass a predict
# function that returns fitted probabilities, setting logodds = TRUE instructs
# the function to set each ice curve to the centered log-odds of the fitted 
# probability.

## Create an ICE object for the predictor "age":
h.ice = ice(h_rf, X = X, predictor = "age", logodds = TRUE,
               predictfcn = function(object, newdata){
                 predict(object, newdata, type = "prob")[, 2]
               }
)

# make a c-ICE plot:
plot(h.ice, x_quantile = FALSE, centered = TRUE)

# Set up 'color_by' using the 'sex' feature
h.ice$Xice$I_sex <- ifelse(h.ice$Xice$sex == 1, "orange", "grey")  # assuming 1 = male, 0 = female

# Full ICE plot, colored by the 'sex' feature
plot(h.ice, plot_pdp = TRUE, color_by = "I_sex", x_quantile = FALSE, centered = TRUE)

####################### Using chest pain (cp) as second feature ##############

cp.ice = ice(h_rf, X = X, predictor = "age", logodds = TRUE,
            predictfcn = function(object, newdata){
              predict(object, newdata, type = "prob")[, 2]
            }
)

# Create a color vector based on the 'cp' feature
# Assigning a distinct color to each chest pain type:
cp.ice$Xice$color_cp <- as.factor(X$cp)

# make a c-ICE plot:
plot(cp.ice, plot_pdp = TRUE, x_quantile = FALSE, centered = TRUE, color_by = "color_cp")

## make a d-ICE object and plot it.
cp.dice = dice(cp.ice)
plot(cp.dice, x_quantile = FALSE, color_by = "color_cp")



############################ Using thalac ################################

# thalac = Maximum heart rate achieved (numerical)

## Create an ICE object for the predictor "thalac":
t.ice = ice(h_rf, X = X, predictor = "thalach", logodds = TRUE,
            predictfcn = function(object, newdata){
              predict(object, newdata, type = "prob")[, 2]
            }
)

# make a c-ICE plot:
plot(t.ice, x_quantile = FALSE, centered = TRUE)

# Set up 'color_by' using the 'sex' feature
t.ice$Xice$I_sex <- ifelse(t.ice$Xice$sex == 1, "orange", "grey")  # assuming 1 = male, 0 = female

# Full ICE plot, colored by the 'sex' feature
plot(t.ice, plot_pdp = FALSE, color_by = "I_sex", centered = TRUE)

# d-ice
h.dice = dice(h.ice)
plot(h.dice, plot_pdp = FALSE, color_by = "I_sex")


##################### Grid #####################

predictor_list <- names(X)
# Filter numeric (non-factor) predictors
# numeric_predictors <- names(X)[sapply(X, is.numeric)]
numeric_predictors <- c("age","trestbps","chol","thalach","oldpeak")

# Fraction of data to plot
N = nrow(X)
frac_to_plot = 300/N; frac_to_plot = min(frac_to_plot, 1)   #suitable frac_to_plot

# Determine the number of rows and columns for the grid
num_plots <- length(numeric_predictors)
grid_rows <- ceiling(sqrt(num_plots))
grid_cols <- ceiling(num_plots / grid_rows)

# ICE Plots: Single file for all ICE plots
png(file = "cICE_Plots_HEART.png", width = 800 * grid_cols, height = 600 * grid_rows)
par(mfrow = c(grid_rows, grid_cols))  # Arrange plots in a grid

# ICE Plots: Create a separate file for each predictor
for (predictor in numeric_predictors) {
  pred.ice <- ice(h_rf, X = X, predictor = predictor, logodds = TRUE,
                   predictfcn = function(object, newdata) {
                     predict(object, newdata, type = "prob")[, 2]
                   })
  # Set up 'color_by' using the 'sex' feature
  pred.ice$Xice$I_sex <- ifelse(pred.ice$Xice$sex == 1, "orange", "grey")  # assuming 1 = male, 0 = female
  
  plot_title_ice <- paste("ICE Plot for", predictor)
  plot(pred.ice, centered = TRUE, centered_percentile = 0.01, 
       frac_to_plot = frac_to_plot, x_quantile = TRUE,
       main = plot_title_ice, color_by = "I_sex")
}

dev.off()

















