data1 <- read.csv("E:/Projects/R_Programming/Data/Divvy_Stations_Trips_2014_Q1Q2/Divvy_Trips_2014_Q1Q2.csv")
data2 <- read.csv("E:/Projects/R_Programming/Data/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-07.csv")
data3 <- read.csv("E:/Projects/R_Programming/Data/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-0809.csv")
data4 <- read.csv("E:/Projects/R_Programming/Data/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q4.csv")

fileData <- rbind(data1,data2,data3,data4)

library(sqldf)
subscriberData <- sqldf("select * from fileData fd where fd.usertype = 'Subscriber' ")
customerData <- sqldf("select * from fileData fd where fd.usertype = 'Customer' ")
subscriberData$numericGender[subscriberData$gender=="Male"] <- "1"
subscriberData$numericGender[subscriberData$gender=="Female"] <- "0"

asDateStartTimeSubscriber <- strptime(subscriberData$starttime, format = c("%m/%d/%Y %H:%M"))
asDateStartTimeCustomer <- strptime(customerData$starttime, format = c("%m/%d/%Y %H:%M"))


dfSubscriber <- data.frame(months = months(asDateStartTimeSubscriber),
                 dayOfWeek = weekdays(asDateStartTimeSubscriber),
                 hours = as.numeric(format(asDateStartTimeSubscriber,"%H")),
                 lengthOfRentalsInHours = subscriberData$tripduration/3600,
                 age = as.numeric(format(Sys.Date(),'%Y')) - as.numeric(subscriberData$birthyear,format("%Y")),
                 gender = subscriberData$gender
                 )


dfSubscriberNumeric <- data.frame(months = as.numeric(format(asDateStartTimeSubscriber,"%m")),
                                  dayOfWeek = as.numeric(format(asDateStartTimeSubscriber,"%u")),
                                  hours = as.numeric(format(asDateStartTimeSubscriber,"%H")),
                                  lengthOfRentalsInHours = as.numeric(subscriberData$tripduration/3600),
                                  age = as.numeric(format(Sys.Date(),'%Y')) - as.numeric(subscriberData$birthyear,format("%Y")),
                                  gender = as.numeric(format(subscriberData$numericGender))
)



View(dfSubscriberNumeric)

dfCustomer <- data.frame(months = months(asDateStartTimeCustomer),
                           dayOfWeek = weekdays(asDateStartTimeCustomer),
                           hours = as.numeric(format(asDateStartTimeCustomer,"%H")),
                           lengthOfRentalsInHours = customerData$tripduration/3600
                         )

dfCustomerNumeric <- data.frame(months = as.numeric(format(asDateStartTimeCustomer,"%m")),
                                dayOfWeek = as.numeric(format(asDateStartTimeCustomer,"%u")),
                                hours = as.numeric(format(asDateStartTimeCustomer,"%H")),
                                lengthOfRentalsInHours = customerData$tripduration/3600
)


write.csv(dfCustomer,"E:/Projects/R_Programming/Data/customerData.csv")
write.csv(dfSubscriber,"E:/Projects/R_Programming/Data/subscriberData.csv")

write.csv(dfCustomerNumeric,"E:/Projects/R_Programming/Data/customerDataNumeric.csv")
write.csv(dfSubscriberNumeric,"E:/Projects/R_Programming/Data/subscriberDataNumeric.csv")

install.packages("xlsx")
library("xlsx")
write.xlsx(dfCustomer,"C:/Users/985176/Desktop/WorkingDirectory/customerData.xlsx")
write.xlsx(dfSubscriber,"C:/Users/985176/Desktop/WorkingDirectory/subscriberData.xlsx")





fit <- kmeans(dfCustomer, 5)
