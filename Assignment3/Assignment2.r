
data1 <- read.csv("E:/Projects/R_Programming/Assignment2/Divvy_Stations_Trips_2014_Q1Q2/Divvy_Trips_2014_Q1Q2.csv")
data2 <- read.csv("E:/Projects/R_Programming/Assignment2/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-07.csv")
data3 <- read.csv("E:/Projects/R_Programming/Assignment2/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-0809.csv")
data4 <- read.csv("E:/Projects/R_Programming/Assignment2/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q4.csv")

dataCombine <- rbind(data1,data2,data3,data4)
View(dataCombine)
asDateStartTime <- strptime(dataCombine$starttime, format = c("%m/%d/%Y %H:%M"))
dataCombine$scode[dataCombine$usertype=="Subscriber"] <- "1"
dataCombine$scode[dataCombine$usertype=="Customer"] <- "2"

df1  <- data.frame(months = months(asDateStartTime), dayOfWeek = weekdays(asDateStartTime,abbreviate = FALSE), hours = as.numeric(format(asDateStartTime,"%H")), lengthOfRentalsInHours = dataCombine$tripduration/3600, rentalTypeNumeric = factor(dataCombine$scode))
df <- data.frame(months = months(asDateStartTime), dayOfWeek = weekdays(asDateStartTime,abbreviate = FALSE), hours = as.numeric(format(asDateStartTime,"%H")), lengthOfRentalsInHours = dataCombine$tripduration/3600, rentalType = dataCombine$usertype)
View(head(df1))
write.csv(df,"E:/Projects/R_Programming/Assignment3/TripDataDecisionTree.csv")
write.csv(df1,"E:/Projects/R_Programming/Assignment3/TripDataRegression.csv")

dataFinal <- read.csv("E:/Projects/R_Programming/Assignment3/TripData.csv")

formula <- rentalTypeNumeric ~ months + dayOfWeek + hours + lengthOfRentalsInHours

fit <- lm(formula,data = dataFinal)
summary(fit)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Rental type")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
