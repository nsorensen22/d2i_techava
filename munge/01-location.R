#Import dataset
flights_c <- rio::import("./data/Techava Data For Analysis.xlsx", which = "Flights_C")
employee <- rio::import("./data/Techava Data For Analysis.xlsx", which = "Employees")
creditcard <- rio::import("./data/Techava Data For Analysis.xlsx", which = "CreditCard")
region <- rio::import("./data/Techava Data For Analysis.xlsx", which = "Regions")




#version 1 - staff travelling from location other than office
names(c(flights_c, employee))
tmp <- flights_c %>% dplyr::rename("EID" = "EmployeeID") %>%  dplyr::left_join(employee) %>%
  dplyr::mutate(location_country = ifelse(Location=="New York", "USA", 
                                      ifelse(Location=="London", "England", "Singapore"))) %>%
  dplyr::mutate(Origin_Country = location_country)

rio::export(tmp, "./data/version_01.xlsx", which = "Flights_C")


#version 2 - making length of stay longer
tmp <- rio::import("./data/version_01.xlsx", which = "Flights_C")
tmp$length <- as.numeric(tmp$Return_Date-tmp$Departure_Date)
hist(tmp$length)
tmp$new_return_date <- ifelse(tmp$)


#version 3 - employees flying more than one flight per day


for (i in unique(tmp$EID)) {
  sum(ifelse(tmp$Departure_Date==tmp$Departure_Date, T, F))
  #tmp %>% count(Departure_Date, Return_Date)

}


#version 3 - no NY/Christmas flying
tmp$Departure_Date > 2019

test <- tmp %>% dplyr::filter(tmp$Departure_Date)

filter(FB, date >= as.Date("2013-01-01"), date <= as.Date("2013-12-31"))