# Load necessary library
library(caret)

# Set seed for reproducibility
set.seed(123)

# Create 5-fold cross-validation indices
folds <- createFolds(RASE_data_Norrland$AntalRASEHa_mean, k = 5)

# Cross-validation loop
cv_results <- sapply(folds, function(test_indices) {
  # Split data into training and testing sets
  train_data <- RASE_data_Norrland[-test_indices, ]
  test_data  <- RASE_data_Norrland[test_indices, ]
  
  # Fit the Gamma GLM on training data
  model_cv <- glm_RASE_Ha <- glm(AAntalRASEHa_mean ~ scale(ungulate_index_mean) + 
                                   scale(AntalBjorkarHa_mean), family = Gamma(link = "log"), 
                                 data = RASE_data_Norrland)
  
  summary(glm_RASE_Ha)
  
  # Predict on test data
  preds <- predict(model_cv, newdata = test_data, type = "response")
  
  # Compute Mean Squared Error (MSE)
  mse <- mean((test_data$AntalRASEHa_mean - preds)^2)
  
  return(mse)
})

# Compute the average cross-validated MSE
mean_cv_mse <- mean(cv_results)
print(mean_cv_mse)

# Compute training set residual error
train_mse <- mean(residuals(glm_RASE_Ha, type = "response")^2)
print(train_mse)

# Check for overfitting
if (mean_cv_mse > train_mse * 1.5) {
  print("Potential overfitting: CV error is much higher than training error.")
} else {
  print("No strong evidence of overfitting.")
}

