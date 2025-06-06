 #' Reads in the data excel files as provided by Eirgrid
#' install.packages("readxl")
library(readxl)
install.packages("lubridate")
install.packages("tidyverse")
library(lubridate)
library(tidyverse)

qtr_hourly_2024.df<-read_excel("C:/Users/10972490/OneDrive - National University of Ireland, Galway/Rpackages/Rpackages/Windpower2024/data_raw/System_Data_Qtr_Hourly_2024.xlsx")
qtr_hourly_20142015.df<-read_excel("C:/Users/10972490/OneDrive - National University of Ireland, Galway/Rpackages/Rpackages/Windpower2024/data_raw/System-Data-Qtr-Hourly-2014-2015.xlsx")
qtr_hourly_20162017.df<-read_excel("C:/Users/10972490/OneDrive - National University of Ireland, Galway/Rpackages/Rpackages/Windpower2024/data_raw/System-Data-Qtr-Hourly-2016-2017.xlsx")
qtr_hourly_20182019.df<-read_excel("C:/Users/10972490/OneDrive - National University of Ireland, Galway/Rpackages/Rpackages/Windpower2024/data_raw/System-Data-Qtr-Hourly-2018-2019.xlsx")
qtr_hourly_20202021.df<-read_excel("C:/Users/10972490/OneDrive - National University of Ireland, Galway/Rpackages/Rpackages/Windpower2024/data_raw/System-Data-Qtr-Hourly-2020-2021.xlsx")
qtr_hourly_20222023.df<-read_excel("C:/Users/10972490/OneDrive - National University of Ireland, Galway/Rpackages/Rpackages/Windpower2024/data_raw/System-Data-Qtr-Hourly-2022-2023.xlsx")
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

  while (dim(data)[1]>=i+3) # checks is gone over the lenght of the list of data points
    {
    hourly_average<-(data[i,2]+data[i+1,2]+data[i+2,2]+data[i+3,2])/4 # averages the four points
    new_row<-c(data[i,1],hourly_average) #makes a new row
    out_put_data.df<-rbind(out_put_data.df,new_row)
    i<-i+4 #jumps to next hour
    }
  return(out_put_data.df) #returns the new data frame
}

wind_generation_data_20142015<-averageHourlyData(qtr_hourly_WG_20142015.df) # changes to hourly values for 2014 and 2015 data
colnames(wind_generation_data_20142015)<-c("DateTime","IE_Wind_Generation")
head(wind_generation_data_20142015)
wind_generation_data_20162017<-averageHourlyData(qtr_hourly_WG_20162017.df)#same for 2016 an 2017 and adds to dataframe
colnames(wind_generation_data_20162017)<-c("DateTime","IE_Wind_Generation")

wind_generation_data_20182019<-averageHourlyData(qtr_hourly_WG_20182019.df) #as above
colnames(wind_generation_data_20182019)<-c("DateTime","IE_Wind_Generation")

wind_generation_data_20202021<-averageHourlyData(qtr_hourly_WG_20202021.df) #as above
colnames(wind_generation_data_20202021)<-c("DateTime","IE_Wind_Generation")

wind_generation_data_20222023<-averageHourlyData(qtr_hourly_WG_20222023.df) #as above
colnames(wind_generation_data_20222023)<-c("DateTime","IE_Wind_Generation")

wind_generation_data_2024<-averageHourlyData(qtr_hourly_WG_2024.df) #as above but only 2024
colnames(wind_generation_data_2024)<-c("DateTime","IE_Wind_Generation")

wind_generation_data<-wind_generation_data_20142015
wind_generation_data<-rbind(wind_generation_data,wind_generation_data_20162017)
wind_generation_data<-rbind(wind_generation_data,wind_generation_data_20182019)
wind_generation_data<-rbind(wind_generation_data,wind_generation_data_20202021)
wind_generation_data<-rbind(wind_generation_data,wind_generation_data_20222023)
wind_generation_data<-rbind(wind_generation_data,wind_generation_data_2024)
head(wind_generation_data)
START_YEAR<-2015
FINISH_YEAR<-2024
wind_generation_data %>% select(DateTime,IE_Wind_Generation) %>%
  mutate(DateTime=as_datetime(DateTime),
         year=year(DateTime),
         month=month(DateTime),
         day=day(DateTime),
         hour=hour(DateTime))    %>%
  select(year, month, day, hour, everything()) %>%
  filter(year >= START_YEAR, year <= FINISH_YEAR)

saveRDS(wind_generation_data,file = "C:/Users/10972490/OneDrive - National University of Ireland, Galway/Rpackages/Rpackages/Windpower2024/data/WindPower2024.rda") #saves the data to the ~/data to be available to package users
