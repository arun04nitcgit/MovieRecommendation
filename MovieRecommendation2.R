install.packages(data.table, dependencies = TRUE)
library(readr)
library(data.table)
library(dplyr)
library(caTools)
library(recommenderlab)
library(ggplot2)


# data <- read.table('u.data')

#C:\Users\arun_manu\Documents\CognizantLearning\DSLA\R\Week5\MovieLens\Movielense-master

movies <- read_csv("CognizantLearning/DSLA/R/Week5/MovieLens/ml-20m/movies.csv")
ratings <-  fread("CognizantLearning/DSLA/R/Week5/MovieLens/Movielense-master/ratings100k.csv")

nrow(ratings)
#data <- ratings[1:500000,]
data <- ratings
data <- data[,- c("timestamp")]

head(movies)
tail(data)
tail(data)

# Number_Movies_29 <- data %>% filter(data$movieId == 29)
# nrow(movies)
#View(Number_Movies_29)
# Number_Movies_29
# nrow(Number_Movies_29)
Number_Ratings <- nrow(data)
Number_Movies <- length(unique(data$movieId))
Number_Users <- length(unique(data$userId))
Number_Movies
## [1] 1682
Number_Users
## [1] 943
density <- round(((Number_Ratings / (Number_Movies * Number_Users)) * 100), 1)
density


# data = data[ data$userId %in% names(table(data$userId))[table(data$userId) > 5] , ]
Number_Ratings <- nrow(data)
Number_Movies <- length(unique(data$movieId))
Number_Users <- length(unique(data$userId))
Number_Movies
## [1] 1681
Number_Users
## [1] 563
density2 <- round(((Number_Ratings / (Number_Movies * Number_Users)) * 100), 1)
density2

str(data)
summary(data)
hist(data$rating, main = "Histogram of Movie Ratings", xlab = "Ratings", col="hot pink")

data$rating = as.integer(data$rating)
dataMatrix <- as(data, "realRatingMatrix")
dataMatrix
ratingmat2 <- dataMatrix

e <- evaluationScheme(dataMatrix[1:round(706*0.75)], method="split", train=0.75, given=15, goodRating=3 , k = 1 )
e

recommender_model <- Recommender(getData(e, "train"), "UBCF")
recommender_model <- Recommender(ratingmat2, method = "UBCF",param=list(method="Cosine",nn=30))

recommender_model

recom <- predict(recommender_model, ratingmat2[1], n=30)
recom_list <- as(recom, "list")
# recom_result <- data.frame(matrix(NA,30))
# recom_result[1:30,1] <- movies[as.integer(recom_list[[1]][1:30]),3]
# recom_result<-as.data.frame(movies[recom_result,2])
# colnames(recom_result)<-list("Top-10 Movies")
# recom_result

#Obtain Top-10 recommendations
recom_result <- matrix(0,10)
for (i in c(1:10)){
  recom_result[i] <- as.integer(recom_list[[1]][i])
}
recom_result<-as.data.frame(movies[recom_result,2])
colnames(recom_result)<-list("Top-10 Movies")
recom_result


# 
# recom_result <- data.frame(na.omit(recom_result[order(order(recom_result)),]))
# recom_result <- data.frame(recom_result[1:10,])
# colnames(recom_result) <- "User-Based Collaborative Filtering Recommended Titles"
# print(recom_list)
# print(recom_result)
# r2 <- Recommender(getData(e, "train"), "IBCF")
# r2

p1 <- predict(r1, getData(e, "known"), type="ratings")
p1

## 125 x 1681 rating matrix of class 'realRatingMatrix' with 208250 ratings.
p2 <- predict(r2, getData(e, "known"), type="ratings")
p2
## 


error <- rbind(
  UBCF <- calcPredictionAccuracy(p1, getData(e, "unknown")),
  IBCF <- calcPredictionAccuracy(p2, getData(e, "unknown"))
)
error

