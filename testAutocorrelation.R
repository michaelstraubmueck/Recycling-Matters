testAutocorrelation <- function(var_model, data, exogen_data, p_value, info_crit, 
                                lag_test, p_max, var_type){
  
  # Perform Breusch-Godfrey Test for autocorrelation
  # H_0: No autocorrelation (if p-value < alpha, reject H_0)
  # If rejected, adjust VAR model by increasing lag length (p = p + 1)
  bgTest <- serial.test(var_model, lags.bg = lag_test, type = "BG")
  
  # Initialize variables for Durbin-Watson test results
  dw_all <- c()
  dw_p <- FALSE
  
  # Perform Durbin-Watson Test for each variable in the VAR model to check for autocorrelation
  for(nVariableVar in 1 : length(var_model$varresult)){
    tmp <- dwtest(var_model$varresult[[nVariableVar]])
    dw_all[[nVariableVar]] <- dwtest(var_model$varresult[[nVariableVar]])
    
    # If the p-value of Durbin-Watson test is below the threshold, autocorrelation might be present
    if(tmp$p.value < p_value){
      print(paste("ATTENTION in Variable ", nVariableVar, 
                  "the Durbin-Watson test p-value is too small!"))
      dw_p <- TRUE
    }
  }
  
  # Check if the Breusch-Godfrey test indicates autocorrelation
  if(bgTest$serial$p.value < p_value){
    
    # Increase lag length by 1
    p_var <- var_model$p[[1]] + 1
    
    # Ensure lag length does not exceed the maximum allowed (p_max)
    if(p_var <= p_max){ 
      
      # Re-estimate VAR model with increased lag length
      var_model <- VAR(data, type = var_type, exogen = exogen_data,
                       p = p_var, ic = info_crit)
      
      # Recursively test for autocorrelation with the updated model
      return(testAutocorrelation(var_model, data, exogen_data, p_value, info_crit,
                                 lag_test, p_max, var_type))
    }else{
      # If lag length exceeds maximum, return model and a warning flag
      returnList <- list("var_model" = var_model, "warning_lag_length" = TRUE)
      return(returnList)
    }
    
  }else{
    # If no autocorrelation is found, return model and no warning
    returnList <- list("var_model" = var_model, "warning_lag_length" = FALSE)
    return(returnList)
  }
  
}