#' Reads in the data excel files as provided by Eirgrid
install.packages("readxl")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("readr")
library(readxl)
library(readr)
library(lubridate)
library(tidyverse)
library(devtools)
setwd("C:/Users/10972490/OneDrive - National University of Ireland, Galway/Rpackages/Rpackages/WindPower2024")

PrepareHourlyData<-function(data,file){
  qtr_hourly<-read_excel(file)
  qtr_hourly_WG<-data.frame(qtr_hourly["DateTime"],qtr_hourly["IE Wind Generation"])

  # function that averages the four quarterly time stamps wind generation data points for each hour
  # and returns a data frame of single average value for each hour
  i<-1 #sets counter to row 1
  hourly_WG<-data.frame("DateTime"=as.Date(character()),"IE_Wind_Generation"=numeric()) # creates an empty data frame
  while (dim(qtr_hourly_WG)[1]>=i+3) # checks is gone over the lenght of the list of data points
  {
    hourly_average<-round((qtr_hourly_WG[i,2]+qtr_hourly_WG[i+1,2]+qtr_hourly_WG[i+2,2]+qtr_hourly_WG[i+3,2])/4,4) # averages the four points
    new_row<-c(qtr_hourly_WG[i,1],hourly_average) #makes a new row
    hourly_WG<-rbind(hourly_WG,new_row)
    i<-i+4 #jumps to next hour
  }
  colnames(hourly_WG)<-c("DateTime","IE_Wind_Generation")
  hourly_WG<-rbind(data,hourly_WG)
  colnames(hourly_WG)<-c("DateTime","IE_Wind_Generation")
  return(hourly_WG) #returns the new data frame
}

wind_generation_data<-data.frame("DateTime"=as.Date(character()),"IE_Wind_Generation"=numeric())
wind_generation_data<-PrepareHourlyData(wind_generation_data,"data_raw/System-Data-Qtr-Hourly-2014-2015.xlsx") # changes to hourly values for 2014 and 2015 data
wind_generation_data<-PrepareHourlyData(wind_generation_data,"data_raw/System-Data-Qtr-Hourly-2016-2017.xlsx")#same for 2016 an 2017 and adds to dataframe
wind_generation_data<-PrepareHourlyData(wind_generation_data,"data_raw/System-Data-Qtr-Hourly-2018-2019.xlsx") #as above
wind_generation_data<-PrepareHourlyData(wind_generation_data,"data_raw/System-Data-Qtr-Hourly-2020-2021.xlsx") #as above
wind_generation_data<-PrepareHourlyData(wind_generation_data,"data_raw/System-Data-Qtr-Hourly-2022-2023.xlsx") #as above
wind_generation_data<-PrepareHourlyData(wind_generation_data,"data_raw/System_Data_Qtr_Hourly_2024.xlsx") #as above but only 2024


wind_generation_data <-wind_generation_data %>%
  mutate(DateTime=as_datetime(DateTime),
         year=year(DateTime),
         month=month(DateTime),
         day=day(DateTime),
         hour=hour(DateTime))    %>%
  select(year, month, day, hour, everything()) %>%filter(year >=2014, year <=2024)

#write_csv(wind_generation_data,file="Windpower2024/data/WindPower2024.csv")
usethis::use_data(wind_generation_data) #saves the data to the ~/data to be available to package users
