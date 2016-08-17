install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(tm)
library(wordcloud)
library(RColorBrewer)

divvyBikeData <- read.csv("C:/Users/985176/Desktop/WorkingDirectory/Divvy_Stations_Trips_2014_Q1Q2/Divvy_Trips_2014_Q1Q2_csv.csv")

mycorpus = Corpus(VectorSource(divvyBikeData$from_station_name))

mycorpus = tm_map(mycorpus, removePunctuation)
mycorpus = tm_map(mycorpus, content_transformer(tolower))
mycorpus = tm_map(mycorpus, removeWords, c("lake","&","th","ct","st","avenue","ave","street",stopwords("english")))
mycorpus = tm_map(mycorpus, stripWhitespace)
mycorpus = tm_map(mycorpus, PlainTextDocument)

tdm <- TermDocumentMatrix(mycorpus)
m1 <- as.matrix(tdm)
v1<- sort(rowSums(m1), decreasing=TRUE)
d1 <- data.frame(word=names(v1), freq=v1)
d1 <- head(d1,100)
wordcloud(d1$word,d1$freq,col=brewer.pal(8,"Set2"), min.freq="5",random.order=FALSE)


















install.packages("igraph")

library(igraph)
myGraph <- graph(c( "A", "B", "A", "D", "B", "A", "B", "B", "B", "C", "C", "A", "D", "C", "D", "D","A","A",
                    "A", "B", "A", "D", "B", "A", "B", "B", "B", "C", "C", "A", "D", "C", "D", "D","A","A"), directed=TRUE)
plot(myGraph)

c( "A", "B", "A", "D", "B", "A", "B", "B", "B", "C", "C", "A", "D", "C", "D", "D","A","A", "A", "B",
   "A", "D", "B", "A", "B", "B", "B", "C", "C", "A", "D", "C", "D", "D","A","A")











library(sqldf)

sortedData <- sqldf("select dv.from_station_name,dv.to_station_name,count(1) from divvyBikeData dv
      group by dv.from_station_id,dv.to_station_id having count(1) > 1 order by count(1) desc limit 10")


View(graphData)
graphData <- graph.data.frame(sortedData,directed = TRUE)

View(page.rank(graphData)$vector)

plot(graphData)




