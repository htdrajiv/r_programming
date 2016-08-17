
dfSubscriberNumeric <- read.csv("E:/Projects/R_Programming/Data/subscriberDataNumericQ1Q2.csv")

dfSubscriberNumericRemoveNan <- dfSubscriberNumeric[complete.cases(dfSubscriberNumeric),]

matrixData <- data.matrix(dfSubscriberNumericRemoveNan)
# View(matrixData)
fit <- kmeans(matrixData,5)
aggregate(matrixData,by=list(fit$cluster),FUN=mean)

#install.packages("mclust")

library("mclust")
fitCluster <- Mclust(matrixData)
plot(fitCluster)

library("cluster")
clusplot(matrixData, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

install.packages("fpc")
library(fpc)
plotcluster(matrixData, fit$cluster)




dfCustomerNumeric <- read.csv("E:/Projects/R_Programming/Data/customerDataNumericQ1Q2.csv")

dfCustomerNumericRemoveNan <- dfCustomerNumeric[complete.cases(dfCustomerNumeric),]

matrixData <- data.matrix(dfCustomerNumericRemoveNan)
# View(matrixData)
fit <- kmeans(matrixData,5)
aggregate(matrixData,by=list(fit$cluster),FUN=mean)

install.packages("mclust")

library(mclust)
fitCluster <- Mclust(matrixData)
plot(fitCluster)

library("cluster")
clusplot(matrixData, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

install.packages("fpc")
library(fpc)
plotcluster(matrixData, fit$cluster)








