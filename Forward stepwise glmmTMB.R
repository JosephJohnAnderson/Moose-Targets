# Start with the null model
model_current <- glmmTMB(RASEAndelGynnsam_mean ~ 1, 
                         family = beta_family(link = "logit"),
                         data = RASE_data_abin_3_point_avg)

# Define candidate predictors
candidates <- c("scale(Älgtäthet.i.vinterstam_mean)", 
                "scale(ungulate_index_mean)", 
                "scale(AntalTallarHa_mean)", 
                "scale(AntalBjorkarHa_mean)", 
                "scale(AntalOvrigtHa_mean)", 
                "scale(proportion_young_forest_mean)", 
                "scale(BestHojdAbinArealV_mean)", 
                "scale(`Mean_seasonal_precipitation[mm]_imputed_mean`)")

# Forward selection
repeat {
  best_aic <- AIC(model_current)  # Store current best AIC
  best_model <- model_current
  best_var <- NULL  # Track which variable improves AIC the most
  
  for (var in candidates) {
    model_new <- update(model_current, as.formula(paste(". ~ . +", var)))
    
    cat("\nTrying to add:", var, "| AIC:", AIC(model_new), "\n")
    
    if (AIC(model_new) < best_aic) {  
      best_aic <- AIC(model_new)
      best_model <- model_new
      best_var <- var
    }
  }
  
  # If no improvement, stop
  if (is.null(best_var)) {
    cat("No more variables improve AIC. Stopping.\n")
    break
  }
  
  # Otherwise, add the best variable
  cat("Adding:", best_var, "New AIC:", best_aic, "\n")
  model_current <- best_model
  
  # Remove selected variable from candidates list
  candidates <- setdiff(candidates, best_var)
}

summary(model_current)  # Final model
