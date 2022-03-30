################################################################################
# PURPOSE
################################################################################
'
The purpose of this scrip is to estimate heterogenous treatment effects in a quick and easy way
Use this to take a first stab at a project converning heterogeneity in treatment effects

Author: Nicolaj
Date: December, 2021
'
################################################################################
# MAIn
################################################################################
autoCATE <- function(df, Y_col, W_col, X_cols,
                     num_trees_regression=2000,
                     num_trees_causal=5000,
                     tune_parameters="all",
                     seed=1991){
  # Load library
  library(grf)
  
  # Check version of packages
  if (packageVersion("grf") != "2.1.0") {
    stop("We require 'grf' to be version 2.1.0, but it is ", packageVersion("grf"))
  }
  
  # ------------------------------
  # Residualize W
  # ------------------------------
  # Train a standard regression forest.
  model_W <- regression_forest(X = df[,X_cols],
                               Y = df[,W_col],
                               num.trees = num_trees_regression,
                               tune.parameters = tune_parameters,
                               seed = seed)
  
  # Predict on out-of-bag training samples.
  W_hat <- predict(model_W)$predictions
  
  message("Check overlap assumption visually by plotting histogram")
  hist(W_hat)
  # ------------------------------
  # Residualize Y
  # ------------------------------
  # Train a standard regression forest.
  model_Y <- regression_forest(X = df[,X_cols],
                               Y = df[,Y_col],
                               num.trees = num_trees_regression,
                               tune.parameters = tune_parameters,
                               seed = seed)
  
  # Predict on out-of-bag training samples.
  Y_hat <- predict(model_Y)$predictions
  
  # ------------------------------
  # Train model
  # ------------------------------
  model_tau <- causal_forest(X = df[,X_cols],
                             Y = df[,Y_col],
                             W = df[,W_col],
                             Y.hat = Y_hat,
                             W.hat = W_hat,
                             num.trees = num_trees_causal,
                             tune.parameters = tune_parameters,
                             seed = seed)
  
  # Predict CATE
  tau_result <- predict(model_tau)
  tau_hat <- tau_result$predictions
  
  # ------------------------------
  # Post-analyses
  # ------------------------------
  # Check whether causal forest predictions are well calibrated.
  calibration_tau <- test_calibration(model_tau)
  
  message("Calibration results")
  print(calibration_tau)
  
  ATE_calibration <- calibration_tau[1,]
  CATE_calibration <- calibration_tau[2,]
  
  message("The forest summary function 'test_calibration' can be used to asses a forest’s goodness of fit.",
          "\nA coefficient of 1 for 'mean.forest.prediction' suggests that the mean forest prediction is correct.",
          "\n\tThe estimated coefficient is ", round(ATE_calibration[1],3), " with a p-value of ", round(ATE_calibration[4], 5),
          "\nA coefficient of 1 for 'differential.forest.prediction' suggests that the forest has captured heterogeneity in the underlying signal.",
          "\n\tThe estimated coefficient is ", round(CATE_calibration[1],3), " with a p-value of ", round(CATE_calibration[4], 5)
  )
  # ------------------------------
  # ATE
  # ------------------------------
  # Estimate the conditional average treatment effect on the full sample (CATE).
  ate <- average_treatment_effect(model_tau, target.sample = "all")
  
  message("Average treatment effects")
  print(ate)
  
  # ------------------------------
  # Return
  # ------------------------------
  results <- list("model_W" = model_W,
                  "model_Y" = model_Y,
                  "model_tau" = model_tau,
                  "ate" = ate,
                  "calibration" = calibration_tau)
  
  return(results)
}



