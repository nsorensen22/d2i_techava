#libraries
library(dplyr)
library(tidyverse)
library(lubridate)

#Import dataset
flights_c <- rio::import("./data/Techava Data For Analysis.xlsx", which = "Flights_C")
employee <- rio::import("./data/Techava Data For Analysis.xlsx", which = "Employees")
creditcard <- rio::import("./data/Techava Data For Analysis.xlsx", which = "CreditCard")
region <- rio::import("./data/Techava Data For Analysis.xlsx", which = "Regions")




#version 01 - staff travelling from location other than office
names(c(flights_c, employee))
tmp <- flights_c %>% dplyr::rename("EID" = "EmployeeID") %>%  dplyr::left_join(employee) %>%
  dplyr::mutate(location_country = ifelse(Location=="New York", "USA", 
                                      ifelse(Location=="London", "England", "Singapore"))) %>%
  dplyr::mutate(Origin_Country = location_country) %>%
  #NG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #Need to update the airport to match this change!


rio::export(tmp, "./data/version_01.xlsx", which = "Flights_C")



#version 02 - extending length of transcontinental trip
tmp <- rio::import("./data/version_01.xlsx", which = "Flights_C") %>%
  dplyr::left_join(region, by = c("location_country"="Country")) %>% dplyr::rename("Origin_region" = "Region") %>%
  dplyr::left_join(region, by = c("Destination_Country"="Country")) %>% dplyr::rename("Destination_region" = "Region") %>%
  dplyr::mutate(intraregion = ifelse(Destination_region==Origin_region, 1, 0))
tmp$length <- as.numeric(tmp$Return_Date-tmp$Departure_Date)
for (i in nrow(tmp)){
  tmp$length_new <- ifelse(tmp$intraregion==1,
         ifelse(tmp$length<3, tmp$`length`+2, tmp$`length`*1),
         tmp$length*1)
}
hist(tmp$length)
hist(tmp$length_new)
tmp$Return_Date_new <- as.Date(tmp$Departure_Date)+tmp$length_new

rio::export(tmp, "./data/version_02.xlsx", which = "Flights_C")



#version 03 - no NY/Christmas flying
tmp <- rio::import("./data/version_02.xlsx", which = "Flights_C") 
tmp <- tmp %>% dplyr::mutate(holiday = ifelse(c(tmp$Departure_Date > "2017-12-25" & tmp$Departure_Date <= "2018-01-02" |
                             tmp$Departure_Date > "2018-12-25" & tmp$Departure_Date <= "2019-01-02" |
                             tmp$Departure_Date > "2019-12-25" & tmp$Departure_Date <= "2020-01-02"), T, F))
tmp_dates <- tmp %>% dplyr::filter(holiday==T) %>%
  dplyr::mutate(year = lubridate::year(Departure_Date))
tmp_dates$Departure_Date <- sample(seq(as.POSIXct('2017-01-03'), as.POSIXct('2017-12-25'), by = "sec"), 24)
tmp_dates <- tmp_dates %>%  dplyr::mutate(fake_year = lubridate::year(Departure_Date),
                                          month = lubridate::month(Departure_Date),
                                          day = lubridate::day(Departure_Date))

tmp_dates$Departure_Date <- lubridate::make_date(year = tmp_dates$year, month = tmp_dates$month, day = tmp_dates$day) 
tmp_dates <- tmp_dates %>% dplyr::select(-c(fake_year,year,month,day))
tmp_dates$Return_Data_new <- tmp_dates$Departure_Date+tmp_dates$length_new
#adding unusual dates dataframe to full frame
tmp <- tmp %>% dplyr::filter(holiday==F) %>%
  rbind(tmp_dates)

rio::export(tmp, "./data/version_03.xlsx", which = "Flights_C")




#version 04 - checking that employees only fly one flight per day
for (i in unique(tmp$EID)) {
  #check for each employee:
    #only one departure date
    #if travelling for more than 1 day, check that they are not also flying out to another trip, i.e. check that there is no Departure_Date == Return_Date
  
  
  
  
  
  k <- tmp %>% dplyr::filter(EID==7)
  ifelse(nrow(k)>1, 
         match(k$Departure_Date, k$Return_Date_new, nomatch = 0))
         
         
         
         
         ,
                print(k$EID)), print("No"))
  
  
  
  

    sum(ifelse(tmp$Departure_Date %in% tmp$Departure_Date, T, F))
  #tmp %>% count(Departure_Date, Return_Date)

}


