#' Reads in the data excel files as provided by Eirgrid
#' install.packages("readxl")
library(readxl)
qtr_hourly_2024.df<-read_excel("data_raw/System_Data_Qtr_Hourly_2024.xlsx")
qtr_hourly_20142015.df<-read_excel("data_raw/System-Data-Qtr-Hourly-2014-2015.xlsx")
qtr_hourly_20162017.df<-read_excel("data_raw/System-Data-Qtr-Hourly-2016-2017.xlsx")
qtr_hourly_20182019.df<-read_excel("data_raw/System-Data-Qtr-Hourly-2018-2019.xlsx")
qtr_hourly_20202021.df<-read_excel("data_raw/System-Data-Qtr-Hourly-2020-2021.xlsx")
qtr_hourly_20222023.df<-read_excel("data_raw/System-Data-Qtr-Hourly-2022-2023.xlsx")
#' Isolated the date time and wind generation columns
qtr_hourly_WG_2024.df<-data.frame(qtr_hourly_2024.df["DateTime"],qtr_hourly_2024.df["IE Wind Generation"])
qtr_hourly_WG_20142015.df<-data.frame(qtr_hourly_20142015.df["DateTime"],qtr_hourly_20142015.df["IE Wind Generation"])
qtr_hourly_WG_20162017.df<-data.frame(qtr_hourly_20162017.df["DateTime"],qtr_hourly_20162017.df["IE Wind Generation"])
qtr_hourly_WG_20182019.df<-data.frame(qtr_hourly_20182019.df["DateTime"],qtr_hourly_20182019.df["IE Wind Generation"])
qtr_hourly_WG_20202021.df<-data.frame(qtr_hourly_20202021.df["DateTime"],qtr_hourly_20202021.df["IE Wind Generation"])
qtr_hourly_WG_20222023.df<-data.frame(qtr_hourly_20222023.df["DateTime"],qtr_hourly_20222023.df["IE Wind Generation"])

averageHourlyData<-function(data){
  # function that averages the four quarterly time stamps wind generation data points for each hour
  # and returns a data frame of single average value for each hour
  i<-1 #sets counter to row 1
  out_put_data.df<-data.frame("DateTime"=as.Date(character()),"IE_Wind_Generation"=numeric()) # creates an empty data frame
  while (i+3<=length(data[1])) # checks is gone over the lenght of the list of data points
    {
    hourly_average<-(data.df[j,2]+data.df[j+1,2]+data.df[j+2,2]+data.df[j+3,2])/4 # averages the four points
    new_row<-c(data.df[j,1],hourly_average) #makes a new row
    out_put_data.df<-rbind(out_put_data.df,new_row)
    j<=j+4 #jumps to next hour
    }
  return(out_put_data.df) #returns the new data frame
}

wind_generation_data<-averageHourlyData(qtr_hourly_WG_20142015.df) # changes to hourly values for 2014 and 2015 data
wind_generation_data<-rbind(wind_generation_data,averageHourlyData(qtr_hourly_WG_20162017.df)) #same for 2016 an 2017 and adds to dataframe
wind_generation_data<-rbind(wind_generation_data,averageHourlyData(qtr_hourly_WG_20182019.df)) #as above
wind_generation_data<-rbind(wind_generation_data,averageHourlyData(qtr_hourly_WG_20202021.df)) #as above
wind_generation_data<-rbind(wind_generation_data,averageHourlyData(qtr_hourly_WG_20222023.df)) #as above
wind_generation_data<-rbind(wind_generation_data,averageHourlyData(qtr_hourly_WG_2024.df)) #as above but only 2024

saveRDS(wind_generation_data,"data/WindPower.rdata") #saves the data to the ~/data to be available to package users
