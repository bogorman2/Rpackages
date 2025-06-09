install.packages("readr")
install.packages("lubridate")
install.packages("tidyverse")
library(readr)
library(lubridate)
library(tidyverse)

# Met Éireann retains Intellectual Property Rights and copyright over this data.
# Met Éireann is acknowledged as the source of this data available at Met.ie.
# Met Éireann does not accept any liability whatsoever for any error or omission in the data series, their availability, or for any loss or damage arising from their use.

START_YEAR=2015
FINISH_YEAR=2024

prepare_2 <- function(file,region,l_skip=17){
  #for most weather station files the header is 17 lines long
  f_df <- read_csv(file,skip=l_skip) # reads in csv file skipping the first l_skip lines. This removes the header so that we get a proper dataframe
  # add a column for wind speed if it is missing
  if(!("wdsp" %in% names(f_df))){
    f_df <- mutate (f_df,wdsp=NA)
  }
  # add a column for wind direction if it is missing
  if(!("wddir" %in% names(f_df))){
    f_df <- mutate (f_df,wddir=NA)
  }
  # change date to date field and add year, month,day, hour, and minute columns then select everything
  # and then filter to only between choosen years
  f_df %>% select(date,rain,temp,-wetb,-dewpt,-vappr,rhum,msl,wdsp,wddir,-starts_with("ind")) %>%
    mutate(date=dmy_hm(date),
           year=year(date),
           month=month(date),
           day=day(date),
           hour=hour(date),
           region=region)    %>%
    select(region, year, month, day, hour, everything()) %>%
    filter(year >= START_YEAR, year <= FINISH_YEAR)
}

setwd("C:/Users/10972490/OneDrive - National University of Ireland, Galway/Rpackages/Rpackages")

East_Region_Weather<-prepare_2("IrishRegionalWeatherData2024/data_raw/DublinAirport.csv","East",23)
saveRDS(East_Region_Weather,"IrishRegionalWeatherData2024/data/East_Region_Weather.rdata")
write_csv(East_Region_Weather,"IrishRegionalWeatherData2024/data/East_Region_Weather.csv")

West_Region_Weather<-prepare_2("IrishRegionalWeatherData2024/data_raw/MaceHead.csv","West",)
saveRDS(West_Region_Weather,"IrishRegionalWeatherData2024/data/West_Region_Weather.rdata")
write_csv(West_Region_Weather,"IrishRegionalWeatherData2024/data/West_Region_Weather.csv")

Central_Region_Weather<-prepare_2("IrishRegionalWeatherData2024/data_raw/Mullingar.csv","Central")
saveRDS(Central_Region_Weather,"IrishRegionalWeatherData2024/data/Central_Region_Weather.rdata")
write_csv(Central_Region_Weather,"IrishRegionalWeatherData2024/data/Central_Region_Weather.csv")

South_Region_Weather<-prepare_2("IrishRegionalWeatherData2024/data_raw/Valentia.csv","South",23)
saveRDS(South_Region_Weather,"IrishRegionalWeatherData2024/data/South_Region_Weather.rdata")
write_csv(South_Region_Weather,"IrishRegionalWeatherData2024/data/South_Region_Weather.csv")

North_Region_Weather<-prepare_2("IrishRegionalWeatherData2024/data_raw/MalinHead.csv","North",23)
saveRDS(North_Region_Weather,"IrishRegionalWeatherData2024/data/North_Region_Weather.rdata")
write_csv(North_Region_Weather,"IrishRegionalWeatherData2024/data/North_Region_Weather.csv")
