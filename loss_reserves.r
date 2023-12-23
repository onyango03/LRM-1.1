# Load the required libraries
library(randomForest)
library(forecast)
library(xgboost)
library(dplyr)
library(caret)
library(tidyverse)
library(tseries)
library(ggplot2)
# Read the claims data 
claims_data <- read.csv("C:/Users/USER/Downloads/ARIMA CLAIMS/claimsdataset1.csv")
# Explore the data
View(claims_data)
str(claims_data)
summary(claims_data)
# Feature Engineering 
claims_data$month <- format(as.Date(paste0("01-", claims_data$Date), format = "%d-%b-%Y"), "%m")
claims_data$year <- format(as.Date(paste0("01-", claims_data$Date), format = "%d-%b-%Y"), "%Y")
View(claims_data)
# Remove the DATE column 
claims_data <- claims_data[, !(names(claims_data) %in% c("Date"))]
View(claims_data)
# Data Visualization - Initial Outlier Plot 
boxplot(claims_data$Claim.Incurred, col = "green", main = "Claims Amount Distribution - Before Log Transformation")
# Log Transformation 
claims_data$log_transformed <- log(claims_data$Claim.Incurred)
# Data Visualization - After Log Transformation 
boxplot(claims_data$log_transformed, col = "blue", main = "Claims Amount Distribution - After Log Transformation")
# Feature Engineering: Add additional relevant features if available
# Split the data into training and testing sets 
set.seed(123) # For reproducibility
train_indices <- sample(1:nrow(claims_data), 0.9 * nrow(claims_data))
train_data <- claims_data[train_indices, ]
test_data <- claims_data[-train_indices, ]
# Random Forest Model
# Model Training
model_rf <- randomForest(log_transformed ~ ., data = train_data)
# Predictions
predictions_rf <- exp(predict(model_rf, newdata = test_data))
# XGBoost Model
# Convert relevant columns to numeric numeric_cols <- !(names(train_data) %in% c("log_transformed"))
train_data[numeric_cols] <- lapply(train_data[numeric_cols], as.numeric)
test_data[numeric_cols] <- lapply(test_data[numeric_cols], as.numeric)
# Create DMatrix for training data
dtrain <- xgb.DMatrix(as.matrix(train_data[, numeric_cols]), label = train_data$log_transformed)
# Create DMatrix for testing data
dtest <- xgb.DMatrix(as.matrix(test_data[, numeric_cols]), label = test_data$log_transformed)
# Set XGBoost parameters xgb_params <- list(objective = "reg:squarederror", booster = "gbtree",
eta = 0.1,
max_depth = 6,
nrounds = 100)
# Train the XGBoost model
model_xgb <- xgboost(data = dtrain, params = xgb_params, nrounds = xgb_params$nrounds)
# Make predictions using the XGBoost model
predictions_xgb <- exp(predict(model_xgb, newdata = dtest))
# ARIMA Model
# Check for stationarity using augmented Dickey-Fuller test adf_test <- adf.test(train_data$Claim.Incurred)
# Extract the p-value from the ADF test p_value <- adf_test$p.value
# Print the p-value cat("ADF Test - p-value:", p_value, "\n")
# Interpretation of stationarity test
if (p_value < 0.05) { stationarity <- "Stationary"} else { stationarity <- "Non-stationary"}
# ACF and PACF plots acf(train_data$Claim.Incurred, main = "ACF Plot") # ACF plot pacf(train_data$Claim.Incurred, main = "PACF Plot") # PACF plot
# Identify optimal ARIMA parameters using auto.arima arima_model <- auto.arima(train_data$Claim.Incurred)
# Train the ARIMA model arima_fit <- arima(train_data$Claim.Incurred, order = arima_model$arma[1:3])
# Forecast using the ARIMA model
arima_forecast <- forecast(arima_fit, h = nrow(test_data))
# Extract the point forecasts from the ARIMA forecast arima_predictions <- arima_forecast$mean
# Model Comparison comparison_data <- data.frame(Original_Claims = test_data$Claim.Incurred,
ARIMA_Predictions = arima_predictions,
RF_Predictions = predictions_rf,
XGB_Predictions = predictions_xgb)
# View the comparison data
print(comparison_data)
# Calculate MSE for ARIMA model mse_arima <- mean((test_data$Claim.Incurred - arima_predictions)^2)
# Calculate MSE for random forest model mse_rf <- mean((test_data$Claim.Incurred - predictions_rf)^2)
# Calculate MSE for XGBoost model mse_xgb <- mean((test_data$Claim.Incurred - predictions_xgb)^2)
# Calculate MAE for ARIMA model mae_arima <- mean(abs(test_data$Claim.Incurred - arima_predictions))
# Calculate MAE for random forest model mae_rf <- mean(abs(test_data$Claim.Incurred - predictions_rf))
# Calculate MAE for XGBoost model mae_xgb <- mean(abs(test_data$Claim.Incurred - predictions_xgb))
# Print the MSE and MAE for all models cat("MSE (ARIMA):", mse_arima, "\n") cat("MSE (Random Forest):", mse_rf, "\n") cat("MSE (XGBoost :", mse_xgb, "\n")
cat("\n") cat("MAE (ARIMA):", mae_arima, "\n") cat("MAE (Random Forest):", mae_rf, "\n") cat("MAE (XGBoost):", mae_xgb, "\n")
# Residual analysis for ARIMA model
arima_residuals <- residuals(arima_fit)
# Residual analysis for random forest model rf_residuals <- test_data$Claim.Incurred - predictions_rf
# Residual analysis for XGBoost model xgb_residuals <- test_data$Claim.Incurred - predictions_xgb
# Plot residuals
par(mfrow = c(3, 1)) plot(arima_residuals, type = 'l', main = 'ARIMA Residuals') plot(rf_residuals, type = 'l', main = 'Random Forest Residuals') plot(xgb_residuals, type = 'l', main = 'XGBoost Residuals')
# Reshape the data to long format for plotting
comparison_data_long <- comparison_data %>%
pivot_longer(cols = c(RF_Predictions, XGB_Predictions), names_to = "Model", values_to = "Predictions")
# Plot Original Claims vs. Predictions
ggplot(comparison_data_long, aes(x = Original_Claims, y = Predictions, color = Model)) +
geom_point() + geom_abline(color = "gray") + labs(x = "Original Claims", y = "Predictions", color = "Model") + ggtitle("Original Claims vs. Predictions") + scale_color_manual(values = c("red", "blue"))