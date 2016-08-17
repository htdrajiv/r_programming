# read csv file
regresson <- read.csv("E:/Projects/R_Programming/LibertyRegressionCsv.csv")

par(mfrow=c(2,2))

plot(regresson$Revenue, type = "l", ylab = "Revenue", xlab = "Year", main = "Line Graph of Revenue")

# install.packages("plotrix")
# library(plotrix)
pie3D(regresson$Revenue, main ="Pie chart of Revenue per year", radius = 3, explode = 0.5, labels =  regresson$Year)

barplot(regresson$Revenue, main = "Barplot of Revenue", xlab = "Year", ylab = "Revenue")

hist(regresson$Revenue, main = "Histogram of Revenue", xlab = "Revenue", breaks = 10)

# source("E:/Projects/R_Programming/Assignment1_BDA.r")

