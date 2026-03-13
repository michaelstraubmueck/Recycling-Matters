
stationary_test <- function(time_series, p_value, diff_type) {
  library(tseries)
  # Create an empty data frame to store results
  result_data <- data.frame(matrix(NA, nrow = nrow(time_series), ncol = ncol(time_series)))
  difference_taken <-data.frame(matrix(NA, nrow = 1, ncol = ncol(time_series)))
  
  # Iterate over rows
  for (n in 1:ncol(time_series)) {
    # Check if the time series is stationary
    if (adf.test(time_series[,n])$p.value>p_value) {
      difference_taken[,n] <- TRUE
      # calculate log return or discrete return
      if (diff_type[n] =="logarithm"){
        #log return
        time_series[,n]<-log(time_series[,n])
        result_data[,n] <- c(NA, diff(time_series[,n]))
      }else if (diff_type[n] =="absolute") {
        #absolute difference
        result_data[,n] <- c(NA, diff(time_series[,n]))
        
      } else if (diff_type[n] =="discrete"){ 
        #discrete return
        result_data[,n] <- c(NA, diff(time_series[,n]))/lag(time_series[,n])
      } else { 
        print("unkown diff_type")
      }
      
      
    } else {
      # If stationary, copy the original row to result_data
      result_data[,n] <- time_series[,n]
      difference_taken[,n] <- FALSE
      
    }
  }
  # if any difference was taken, remove the first row of the dataframe
  if (any(is.na(result_data[1, ]))) {
    result_data <- result_data[-1, , drop = FALSE]
  }
  
  colnames(result_data) <- colnames(time_series)
  colnames(difference_taken) <- colnames(time_series)
  # Return the data frame with processed rows
  return(list(result_data = result_data, difference_taken = difference_taken))
}
