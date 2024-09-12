library(nnet) 
library(sfsmisc)
library(ICEbox)
library(corrplot)
library(lime)
library(caret) # Used for evaluate the model 
library(DataExplorer) # Data exploration and reporting 

## Features Explanation ###
  # - Country: Name of the country
  # - Year: Year of data collection
  # - Status: Country status (1 = Developed, 2 = Developing)
  # - Life Expectancy: Life expectancy in years
  # - Adult Mortality: Mortality rate (15-60 years) per 1000 population
  # - Infant Deaths: Number of infant deaths per 1000 population
  # - Alcohol: Per capita alcohol consumption (in liters)
  # - Percentage Expenditure: Health expenditure as a percentage of GDP
  # - Hepatitis B: Immunization coverage among 1-year-olds (%)
  # - Measles: Number of reported measles cases per 1000 population
  # - BMI: Average Body Mass Index of the population
  # - Under-Five Deaths: Number of deaths of children under five per 1000 population
  # - Polio: Polio immunization coverage among 1-year-olds (%)
  # - Total Expenditure: Government expenditure on health as a percentage of total expenditure
  # - Diphtheria: Immunization coverage for diphtheria, tetanus, and pertussis among 1-year-olds (%)
  # - HIV/AIDS: Deaths per 1000 live births due to HIV/AIDS (0-4 years)
  # - GDP: Gross Domestic Product per capita (USD)
  # - Population: Population size of the country
  # - Thinness 1-19 Years: Prevalence of thinness among children and adolescents (10-19 years)
  # - Thinness 5-9 Years: Prevalence of thinness among children (5-9 years)
  # - Income Composition of Resources: Human Development Index (HDI) income composition (0 to 1)
  # - Schooling: Number of years of schooling

############################################
########### Data Preparation ###############
############################################
data <- read.csv("Life Expectancy Data.csv")
#create_report(data) # Generates HTML report

#all column names to lower case
colnames(data) <- tolower(colnames(data))
# check for missing values and in which columns
missing_values <- colSums(is.na(data))

# replaces any missing values with the mean of that column
for (i in 1:ncol(data)) {
  if (missing_values[i] > 0) {
    data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE) # it selects "NA" in col i
  }
}

X <- data[, -which(names(data) == "life.expectancy")] # Features vector 
y <- data$life.expectancy # target variable

# Identify the numeric columns for determining correlation
numeric_cols <- sapply(X, is.numeric)
# Calculate the correlation matrix
cor_matrix <- cor(X[numeric_cols])
corrplot(cor_matrix, method = "number", diag = FALSE,
         type = "lower",
         tl.cex = .5, 
         cl.cex = .6, 
         number.cex = .4, addCoef.col = "black", 
         mar = c(1, 1, 1, 1))

# Remove highly correleted features 
columns_to_remove <- c("thinness.5.9.years", "income.composition.of.resources", 
                       "percentage.expenditure", "infant.deaths")

X <- X[, !(names(X) %in% columns_to_remove)]
numeric_cols <- sapply(X, is.numeric)
numeric_cols <- numeric_cols & !names(X) %in% "year"

# Standardize the numeric columns except for 'year'
X[numeric_cols] <- scale(X[numeric_cols], center = TRUE, scale = TRUE)

# Extract the scaling parameters (mean and standard deviation)
X_center <- attr(scale(X[numeric_cols], center = TRUE, scale = TRUE), 'scaled:center')
X_scale <- attr(scale(X[numeric_cols], center = TRUE, scale = TRUE), 'scaled:scale')

# Convert year and character columns to factors and then to numeric labels
X$year <- as.numeric(as.factor(X$year))

# Apply label encoding to all character columns
char_cols <- sapply(X, is.character)
X[char_cols] <- lapply(X[char_cols], function(x) as.numeric(as.factor(x)))

set.seed(123)
# Split X and y into training and testing sets
trainIndex <- createDataPartition(y, p = 0.98, list = FALSE)

X_train <- X[trainIndex, ]
X_val  <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_val  <- y[-trainIndex]


####################################################
######### Model Building and Explanation ###########
####################################################


set.seed(38)
nnet_mod = nnet(x = X_train, y = as.matrix(y_train), size = 4, 
                maxit = 1000, decay = 6e-4, linout = TRUE)
y_pred <- predict(nnet_mod, newdata = X_val)

# Evaluation Metrics: postResample provided by CARET
results <- postResample(y_pred, y_val)
print(results)

######### LIME Explanation ##########

# Define model_type function for LIME
model_type.nn <- function(x, ...) {
  return("regression")
}
assignInNamespace("model_type", model_type.nn, ns = "lime")

# Create the explainer object
explainer <- lime(X_train, nnet_mod, bin_continuous = TRUE)
# Select an instance to explain
instances_to_explain <- X_train[numeric_cols][1262,] #1262, 2456 instances used for plots
# Generate explanations
explanations <- explain(
  x = instances_to_explain,
  explainer = explainer,
  n_features = length(X_train[numeric_cols]),
  n_permutations = 200
)

#print(explanations)
plot_features(explanations)

######## ICE Toolbox #########

# Compute ICE plots

nn.ice = ice(nnet_mod, X = X_train, predictor = "hiv.aids", #bmi; hiv.aids
             predictfcn = function(object, newdata){
               newdata[numeric_cols] 
               newdata$year <- as.numeric(as.factor(newdata$year))
               newdata[char_cols] <- lapply(newdata[char_cols], 
                                            function(x) as.numeric(as.factor(x)))
               predict(object, newdata)
             }, 
             y = y_train)

# Plot c-ICE
par(mar = c(4.3, 4.5, 0.5, 2.5), mgp = c(3, 1.3, 0), pty = "s")
plot(nn.ice, x_quantile = T, centered=TRUE,
     centered_percentile=0, frac_to_plot=.3, color_by = "status",
     cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0)

# Plot d-ICE
nn.dice = dice(nn.ice)
par(mar = c(4.3, 4.5, 0.5, 2.5), mgp = c(3, 1.3, 0))
plot(nn.dice, x_quantile=T, frac_to_plot =.3, 
     plot_sd = TRUE, plot_dpdp = TRUE,
     cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0)