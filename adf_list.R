
adf_list<- function(time_series) {

  Return_list <-data.frame(matrix(NA, nrow = 2, ncol = ncol(time_series)))

  
  for (n in 1:ncol(time_series)) {
Return_list[1,n]<-adf.test(time_series[,n])$p.value
Return_list[2,n]<-kpss.test(time_series[,n])$p.value
  }
  colnames(Return_list) <- colnames(time_series)
  # Return List
  return(Return_list)
}