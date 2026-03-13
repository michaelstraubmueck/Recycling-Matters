analyzeVAR <- function(var_model, exogen_data, var_type,
                                        n_ahead_irf, ortho_irf, cumulative_irf, boot_irf, 
                                        conf_int, runs_boot_irf, path_plot, plot_name,
                                        plot_irf, dpi, width, height, unit){
  
  # Extract VAR model lag order for IRF and set seed for reproducibility
  p_var <<- as.numeric(var_model$p)
  exogen_data <<- exogen_data
  set.seed(123456789)
  
  # Initialize empty list for IRF results and significance table  
  analyze_irf_all <- c()
  significance_table <- as.data.frame(matrix(NA, nrow = 1, ncol = length(var_model$varresult) * length(var_model$varresult)))
  colnames(significance_table) <- paste("Impulse_from_", rep(names(var_model$varresult), each = length(var_model$varresult)),
                                        "_to_", rep(names(var_model$varresult), times = length(var_model$varresult)), sep = "")
  
  # Loop through each variable as the impulse and response
  for(nImpulse in 1 : length(var_model$varresult)){
    analyze_irf <- c()
    for(nResponses in 1 : length(var_model$varresult)){
      
      # Compute impulse response function (IRF) for each impulse-response pair
      analyze_irf[[nResponses]] <- irf(var_model, impulse = names(var_model$varresult)[nImpulse], 
                                       response = names(var_model$varresult)[nResponses],  
                                       n.ahead = n_ahead_irf,
                                       ortho = ortho_irf, cumulative = cumulative_irf, 
                                       boot = boot_irf,
                                       ci = conf_int, runs = runs_boot_irf, 
                                       seed = 987654321,
                                       dumvar = exogen_data, exogen = exogen_data )
      
      # Check for significance in all periods and find the first period of significance
      significance_found <- FALSE
      for (period in 1:n_ahead_irf) {  
        if (!is.na(analyze_irf[[nResponses]]$Lower[[1]][period]) & !is.na(analyze_irf[[nResponses]]$Upper[[1]][period])) {
          if (as.numeric(analyze_irf[[nResponses]]$Lower[[1]][period,]) * as.numeric(analyze_irf[[nResponses]]$Upper[[1]][period,]) > 0) {
            if (period == 1) {  
              significance_table[1, (nImpulse - 1) * length(var_model$varresult) + nResponses] <- 
                ifelse(as.numeric(analyze_irf[[nResponses]]$Lower[[1]][period,]) >= 0, "+0", "-0")
            } else {  
              significance_table[1, (nImpulse - 1) * length(var_model$varresult) + nResponses] <- 
                ifelse(as.numeric(analyze_irf[[nResponses]]$Lower[[1]][period,]) >= 0, period - 1, -(period - 1))
            }
            significance_found <- TRUE
            break  
          }
        }
      }
      
      # If no significance is found, set to NA
      if (!significance_found) {
        significance_table[1, (nImpulse - 1) * length(var_model$varresult) + nResponses] <- NA
      }
      
      # Plot IRF if required
      if (plot_irf) {
        jpeg(filename = paste(path_plot, "irf_", plot_name, "_",
                              "impulse_from_", names(var_model$varresult)[nImpulse], "_to_",
                              colnames(var_model$y)[nResponses], ".jpg", sep = ""),
             width = width, height = height, units = unit, res = dpi)
        
        plot(analyze_irf[[nResponses]], plot.type = "single", ci = FALSE,
             main = paste("Orthogonal Impulse Response from \nimpulse ", 
                          colnames(var_model$y)[nImpulse], " \nto Response ",
                          colnames(var_model$y)[nResponses], sep = ""), 
             ylab = "", xlab = "")
        
        dev.off()
      }
      
    }
    # Store IRF results for each impulse
    analyze_irf_all[[nImpulse]] <- analyze_irf[[nResponses]]
  }
  
  # Return results
  return(list("irf" = analyze_irf_all, "significance_table" = significance_table))
}
