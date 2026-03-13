calculateVARModel <- function(data, exogen_data, 
                              p_value, info_crit, 
                              lag_test, lag_start, p_max, var_type, calc_test){
  
  # Estimate initial VAR model
  var_model <- VAR(data, type = var_type,
                   exogen = exogen_data, p = lag_start,
                   ic = info_crit)
  
  # Test for autocorrelation, adjust VAR model if needed
  if(calc_test$Auto){
    testResultsAuto <- testAutocorrelation(var_model, data, exogen_data, p_value, info_crit,
                                            lag_test, p_max, var_type)
    var_model <- testResultsAuto$var_model
  } else {
    testResultsAuto <- NULL
  }
  
  # Test for heteroscedasticity (ARCH test), adjust VAR model if needed
  if(calc_test$Arch){
    testResultsArch <- testArch(var_model, data, exogen_data, p_value, info_crit,
                                lag_test, p_max, var_type)
    var_model <- testResultsArch$var_model
  } else {
    testResultsArch <- NULL
  }
  
  # OLS-CUSUM test for model stability
  if(calc_test$OLSCUSUM){
    testProcessOLSCUSUM <- stability(var_model, type = "OLS-CUSUM", dynamic = TRUE)
    
    # Check stability for each variable
    for(nColOLSCUSUM in 1 : length(testProcessOLSCUSUM$stability)){
      testSignificanceOLSCUSUM <- sctest(testProcessOLSCUSUM$stability[[nColOLSCUSUM]])
      if(testSignificanceOLSCUSUM$p.value < p_value){
        testResultsOLSCUSUM <- TRUE 
      } else {
        testResultsOLSCUSUM <- FALSE
      }
    }
  } else {
    testResultsOLSCUSUM <- NULL
  }
  
  # Collect warnings from tests
  warn_tests <- list("Autocorrelation" = testResultsAuto$warning_lag_length,
                     "ARCH" = testResultsArch$warning_lag_length,
                     "OLS-CUSUM" = testResultsOLSCUSUM)
  
  # Return the updated VAR model and test warnings
  returnList <- list("data" = data, "var_model" = var_model, "warn_tests" = warn_tests)
  return(returnList)
}
