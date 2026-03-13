
testArch <- function(var_model, data, exogen_data, p_value, info_crit,
                     lag_test, p_max, var_type){
  
# Perform ARCH LM Test for heteroscedasticity (check for non-constant variance)
# H_0: No heteroscedasticity (if p-value < alpha, reject H_0)
# If rejected, adjust VAR model by increasing lag length (p = p + 1)
  archTest <- vars::arch.test(var_model, lags.multi = lag_test, multivariate.only = TRUE)
  
  # Check if the p-value indicates significant heteroscedasticity  
  if(archTest$arch.mul$p.value < p_value){
    
    # Increase lag length by 1    
    p_var <- var_model$p[[1]] + 1
    
    # Ensure lag length does not exceed the maximum allowed (p_max)    
    if(p_var <= p_max ){
      
      # Re-estimate VAR model with increased lag length      
      var_model <- VAR(data, type = var_type, exogen = exogen_data, 
                       p = p_var, ic = info_crit)
      
      # Recursively test for heteroscedasticity again      
      return(testArch(var_model, data, exogen_data, p_value, info_crit, 
                               lag_test, p_max, var_type))
    }else{
      
      # If lag length exceeds maximum, return model and a warning flag
      returnList <- list("var_model" = var_model, "warning_lag_length" = TRUE)
      return(returnList)
    }
    
  }else{
    
    # If no heteroscedasticity is found, return model else warning no warning
    returnList <- list("var_model" = var_model, "warning_lag_length" = FALSE)
    return(returnList)
  }
  
}
