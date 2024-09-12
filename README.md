# Peeking Inside the Black Box
Project conducted for the Statistics for Data Science course (A.Y. 2023/24): reproduction and extension of the paper “Peeking Inside the Black Box: Visualizing Statistical Learning With Plots of Individual Conditional Expectation” (Goldstein et al., 2014, https://arxiv.org/abs/1309.6392).
## Overview
This article introduces Individual Conditional Expectation (ICE) plots as a visualization tool for models produced by supervised learning algorithms. Unlike partial dependence plots (PDPs), which show the average effect of features on predictions, ICE plots focus on individual observations to reveal heterogeneity in the model's predictions. By displaying the functional relationship between predictions and a feature for each observation, ICE plots highlight variation and interaction effects that PDPs may obscure. We divided the work by focusing on reproducing and validating the results described by the authors in the paper, and then using the tool on other real-world data in comparison with other explainability techniques.
## Files and folders
```
CODE
├── ICEbox_intro.R
├── RealData/
│   ├── icebox_heartdisease.R
│   ├── life_expectancy_LIME_ICE.R
│   ├── pima_XAI.R
│   └── pima_reproduced.R
└── Simulations/
│   ├── 1_Additivity_Assesment_and_counterexample.R
│   ├── 2_finding_interactions.R
│   └── 3_icebox_extrapolation.R
```
## Summary
The output of the ICE plots, c-ICE plots, and DICE plots provides visual insights into the relationships between model predictions and individual features. Each script generates specific visualizations that can be used to understand model behavior and interactions:
- ICE plots show individual conditional expectations for each observation.
- c-ICE plots are centered versions that help detect differences among curves.
- d-ICE plots are derivative-based plots that show the rate of change of predictions with respect to each feature.
![ICEplot](https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSbeW2Tx1dVjpS5kHduVO0sJaOEDaSP17L3uQ&s)

## Requirements
R Studio 2024.04.2+764 and R 4.4.0 "Puppy Cup"

Packages:
- **ICEbox** – For generating ICE plots.
- **randomForest** – For building Random Forest models.
- **MASS** – Provides datasets used in the project.
- **gam** – Builds Generalized Additive Models (GAMs).
- **gbm** – Implements Gradient Boosting Machines.
- **caret** – For training and tuning machine learning models.
- **knitr** – For report generation.
- **kmed** – Performs k-medoids clustering, provides Heart Disease Dataset.
- **nnet** – Fits neural network models.
- **sfsmisc** – Provides additional statistical functions.
- **corrplot** – Visualizes feature correlations.
- **lime** – Generates LIME explanations.
- **DataExplorer** – For exploratory data analysis.
- **xgboost** – Implements gradient boosting.
- **ggplot2** – Creates advanced visualizations.
- **RColorBrewer** – Supplies color palettes.
- **reshape2** – Reshapes data between wide and long formats.
- **SHAPforxgboost** – Computes SHAP values for XGBoost models.

## Results
Despite the limitations of being able to effectively visualize only one feature at a time and the poor performance in the case where the feature under consideration is highly correlated with others, we have shown that ICE plots prove to be an effective visualization method capable of providing explanations of Black Box patterns, with results comparable to other types of explainability techniques as well.

## Team
[Pietro Argento](https://github.com/p-argento)

[Aldo Montinaro](https://github.com/aldomontinaroam)

[Marco Poiani](https://github.com/MarcoPoiani00)
