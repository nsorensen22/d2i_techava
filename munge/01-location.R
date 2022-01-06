#libraries
library(dplyr)
library(tidyverse)
library(lubridate)
#library(airportr)
library(xml2)
library(geosphere)
library(ggplot2)


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
  dplyr::mutate(Origin_Country = location_country,
                Origin = ifelse(Origin_Country=="Singapore", "SIN",
                                ifelse(Origin_Country=="England", c("LGW", "LHR"), c("JFK", "LGA"))),
                #Ori_Dest = ifelse(Origin==Destination, 1, 0),
                Destination = ifelse(Destination_Country==Origin_Country, "CTU", paste(Destination)),
                Destination_Country = ifelse(Destination=="CTU", "China", paste(Destination_Country)))
airport_db = read.table("./data/airports-extended.txt", sep = ",", header = F) %>% #https://openflights.org/data.html
  dplyr::select("V5", "V7", "V8")
tmp = tmp %>% dplyr::left_join(airport_db, by = c("Origin" = "V5")) %>%
  dplyr::rename("o_lat" = "V7", "o_long" = "V8") %>%
  dplyr::left_join(airport_db, by = c("Destination" = "V5")) %>%
  dplyr::rename("d_lat" = "V7", "d_long" = "V8")
tmp$Distance_Km <- distHaversine(tmp[, c(22,21)], tmp[, c(24,23)]) / 1000

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




#version 02 - time spent corresponding to distance
tmp <- rio::import("./data/version_02.xlsx", which = "Flights_C") 
ggplot(tmp, aes(Distance_Km, Hours)) + geom_point()
############ FIX MANUALLY




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
tmp_dates$Return_Date_New <- tmp_dates$Departure_Date+tmp_dates$length_new

#adding unusual dates dataframe to full frame
tmp <- tmp %>% dplyr::filter(holiday==F) %>%
  rbind(tmp_dates)

rio::export(tmp, "./data/version_03.xlsx", which = "Flights_C")




#version 04 - checking that employees only fly one flight per day
tmp <- rio::import("./data/version_03.xlsx", which = "Flights_C") 

df <- data.frame()
i = 60
for (i in unique(tmp$EID)) {
  tmp <- tmp %>% dplyr::filter(EID==i)
  tmp$test = ifelse(duplicated(tmp[,c(10,30)]), 1, 0)
  tmp <- tmp %>% dplyr::filter(test==1)
  df <- rbind(df, tmp)
}
         
        

#### MAKE FLIGHT DATES MORE RECENT?

