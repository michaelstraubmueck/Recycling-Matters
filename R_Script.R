rm(list=ls())
setwd("enter working directory here")
library(dplyr) 
library(readxl) 
library(tseries) 
library(tsapp) 
library(vars) 
library(seastests) 
library(tibble) 
library(writexl) 
source("remove_seasonality.R")
source("stationary_test.R")
source("calculateVARModel.R")
source("testAutocorrelation.R")
source("testArch.R")
source("adf_list.R")
source("analyzeVAR.R")

# metal to be analyzed
Metal<-"Al"

#import data
data<-read_excel("Endogenous_variables.xlsx", col_names = TRUE, sheet = Metal)
data_exogen<-read_excel("Exogenous_variables.xlsx", col_names = TRUE, sheet ="Exogenous_variables")


#calculate convenience_yield
data$convenience_yield<-(log(data$LME_3m_future/data$LME_spot)-0.25*log(data$ThreeMonth_Market_Yield+1)*0.25)/(-0.25)
data_exogen$Interest<-data_exogen$ThreeMonth_Market_Yield


#define time frame 
data$Date <- as.Date(data$Date)
data_exogen$Date <- as.Date(data_exogen$Date)
date_start<-as.Date("2010-01-29") 
date_end<-as.Date("2023-12-31") 


data <- subset(data, Date >= date_start & Date <= date_end)
data_exogen <- subset(data_exogen, Date >= date_start & Date <= date_end)


#define endogenous variables
VAR_data<-as.data.frame(cbind(primary_production=data$primary_production,
                             secondary_production=data$secondary_production,
                             consumption=data$total_consumption,
                             convenience_yield=data$convenience_yield,
                             futures_price=data$LME_3m_future))


diff_type<- c("logarithm",
              "logarithm",
              "logarithm",
              "absolute",
              "logarithm")


#define exogenous variables
VAR_data_exogen<-as.data.frame(cbind(USD_Index=data_exogen$USD_Index,
                                    Interest=data_exogen$Interest,
                                    Industrial_Production=data_exogen$Industrial_Production,
                                    Energy_Index=data_exogen$Energy_Index))

diff_type_exogen<- c("logarithm",
                     "absolute",
                     "logarithm",
                     "logarithm")


#set parameters for VAR
start_date_ts<-c(as.numeric(format(date_start, "%Y")),as.numeric(format(date_start, "%m"))+1)
frequency<-12         
info_crit="AIC"        
var_type ="none"      
calc_test<-data.frame(Auto="TRUE",
                      Arch="TRUE",
                      OLSCUSUM="TRUE")
p_value<-0.05 
p_max<-12   
lag_test<-1 
lag_start<-1      

#set parameters for IRF
n_ahead_irf= 10
ortho_irf= TRUE
cumulative_irf = FALSE
boot_irf = TRUE
conf_int = 0.9
runs_boot_irf = 1000 
path_plot = paste("enter path here",Metal, sep = "")
plot_name = paste(Metal,"_World", sep="")
plot_irf = FALSE
dpi = 300 
width= 4.8 
height= 3.6  
unit= "in"

#check for stationarity and take difference
#endogenous
stationarity_results<-stationary_test(VAR_data, 0.005, diff_type)
VAR_data<-stationarity_results$result_data
difference_taken<-stationarity_results$difference_taken
print(adf_list(VAR_data))

#exogenous
stationarity_results_exogen<-stationary_test(VAR_data_exogen, 0.005, diff_type_exogen)
VAR_data_exogen<-stationarity_results_exogen$result_data
difference_taken_exogen<-stationarity_results_exogen$difference_taken
print(adf_list(VAR_data_exogen))


#check for seasonality and remove
saisonality_results <- remove_seasonality(VAR_data, freq = frequency)
VAR_data<-saisonality_results$result_data
saisonality_found<-saisonality_results$saisonality_found

saisonality_results_exogen<-remove_seasonality(VAR_data_exogen, freq = frequency)
VAR_data_exogen<-saisonality_results_exogen$result_data
saisonality_found_exogen<-saisonality_results_exogen$saisonality_found

#create time series
VAR_data<-ts(VAR_data, start =start_date_ts, freq = frequency)
VAR_data_exogen<-ts(VAR_data_exogen, start =start_date_ts, freq = frequency)

#calculate VAR
VAR_Result<-calculateVARModel(VAR_data, VAR_data_exogen, 
                              p_value, info_crit,
                              lag_test, lag_start, p_max, var_type, calc_test)
print(VAR_Result$warn_tests)

#analyze VAR
IRF_Results<-analyzeVAR(VAR_Result$var_model, VAR_data_exogen, var_type,
                              n_ahead_irf, ortho_irf, cumulative_irf, boot_irf,
                              conf_int, runs_boot_irf, path_plot, plot_name,
                              plot_irf, dpi, width, height, unit)

#create IRF table
IRF_table<-data.frame(t(IRF_Results$significance_table))
IRF_table<-rownames_to_column(IRF_table)
colnames(IRF_table)<-(c("relation","effect"))
write_xlsx(IRF_table, path = paste(path_plot,"IRF_table.xlsx",sep = ""))

