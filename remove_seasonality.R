remove_seasonality <- function(time_series, freq) {
  library(seastests)
  library(tsapp)
  # Create an empty data frame to store results
  result_data <- data.frame(matrix(NA, nrow = nrow(time_series), ncol = ncol(time_series)))
  saisonality_found <-data.frame(matrix(NA, nrow = 1, ncol = ncol(time_series)))
  
  # Iterate over rows
  for (n in 1:ncol(time_series)) {
    # Check if the time series is seasonal
    if (isSeasonal(time_series[,n], freq = freq)) {
      # Decompose the time series
      decomposition_result <- simpledecomp(time_series[,n], season = freq, Plot = FALSE, trend=0)
      
      # Extract the residual component (without trend and seasonality)
      residual_component <- decomposition_result[,3]
      
      # Store the residual component in the result_data
      result_data[,n] <- residual_component
      saisonality_found[,n] <- TRUE
    } else {
      # If not seasonal, copy the original row to result_data
      result_data[,n] <- time_series[,n]
      colnames(result_data) <- colnames(time_series)
      saisonality_found[,n] <- FALSE
    }
  }
  
  colnames(saisonality_found) <- colnames(time_series)
  # Return the data frame with processed rows
  return(list(result_data = result_data,  saisonality_found =  saisonality_found))
}
