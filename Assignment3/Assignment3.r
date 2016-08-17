# --------------------------------  combining all data --> started --------------------------------
data1 <- read.csv("E:/Projects/R_Programming/Assignment2/Divvy_Stations_Trips_2014_Q1Q2/Divvy_Trips_2014_Q1Q2.csv")
data2 <- read.csv("E:/Projects/R_Programming/Assignment2/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-07.csv")
data3 <- read.csv("E:/Projects/R_Programming/Assignment2/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-0809.csv")
data4 <- read.csv("E:/Projects/R_Programming/Assignment2/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q4.csv")

dataCombine <- rbind(data1,data2,data3,data4)
View(dataCombine)
asDateStartTime <- strptime(dataCombine$starttime, format = c("%m/%d/%Y %H:%M"))
dataCombine$scode[dataCombine$usertype=="Subscriber"] <- "1"
dataCombine$scode[dataCombine$usertype=="Customer"] <- "2"

df <- data.frame(months = months(asDateStartTime), dayOfWeek = weekdays(asDateStartTime,abbreviate = FALSE), hours = as.numeric(format(asDateStartTime,"%H")), lengthOfRentalsInHours = dataCombine$tripduration/3600,rentalType = dataCombine$usertype, rentalTypeNumeric = factor(dataCombine$scode))
View(df)
write.csv(df,"C:/Users/985176/Desktop/WorkingDirectory/TripData.csv")
dataFinal <- read.csv("E:/Projects/R_Programming/Assignment3/TripData.csv")
# --------------------------------  combining all data --> ended  --------------------------------


# --------------------------------  decision tree --> started  --------------------------------

library(rpart)
fit <- rpart(rentalType ~ months + dayOfWeek + hours + lengthOfRentalsInHours, method="class", data=dataFinal)
plotcp(fit)
printcp(fit)
summary(fit)

plot(fit, uniform=TRUE, 
     main="Classification Tree for rental type")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# --------------------------------  decision tree --> ended  --------------------------------


# --------------------------------  regresson --> started  --------------------------------


formula <- rentalTypeNumeric ~ months + dayOfWeek + hours + lengthOfRentalsInHours

fit <- lm(formula,data = dataFinal)
summary(fit)

# --------------------------------  regresson --> ended  --------------------------------


