install.packages(data.table, dependencies = TRUE)
library(readr)
library(data.table)
library(dplyr)
# Read Movies and Ratings from CSV
movies <- read_csv("/R/Week5/MovieLens/ml-20m/movies.csv")
ratings <-  fread("/R/Week5/MovieLens/ml-20m/ratings.csv")
View(ratings)
View(movies)

# 
# library(data.table)
# genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
# View(genres)
# genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
# colnames(genres2) <- c(1:7)
# View(genres2)
# nrow(genres2)
# 
# genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")
# head(genre_list)
# genre_matrix <- matrix(0,27279,18) #empty matrix
# genre_matrix[1,] <- genre_list #set first row to genre list
# colnames(genre_matrix) <- genre_list #set column names to genre list
# View(genre_matrix)
# #iterate through matrix
# for (i in 1:nrow(genres2)) {
#   for (c in 1:ncol(genres2)) {
#     genmat_col = which(genre_matrix[1,] == genres2[i,c])
#     genre_matrix[i+1,genmat_col] <- 1
#   }
# }
# 
# #convert into dataframe
# genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
# nrow(genre_matrix2)
# for (c in 1:ncol(genre_matrix2)) {
#   genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
# } #convert from characters to integers
# 
# View(genre_matrix2)


data <- ratings
data <- data[,- c("timestamp")]

data$rating <- as.integer(round(data$rating))
data <- data[1:2000,]
str(data)
nrow(data)
View(data)
as(data$rating, "int")
str(data)
summary(data)
hist(data$rating, main = "Histogram of Movie Ratings", xlab = "Ratings", col="hot pink")

Number_Ratings <- nrow(data)
Number_Movies <- length(unique(data))
Number_Users <- length(unique(data$userId))
Number_Ratings
Number_Movies
## [1] 1682
Number_Users
## [1] 943

density <- round(((Number_Ratings / (Number_Movies * Number_Users)) * 100), 1)
density
str(data)
data = data[ data$movieId %in% names(table(data$userId))[table(data$userId) > 50] , ]
Number_Ratings <- nrow(data)
Number_Movies <- length(unique(data$item_id))
Number_Users <- length(unique(data$user_id))
Number_Movies
## [1] 1681
Number_Users
## [1] 563
density2 <- round(((Number_Ratings / (Number_Movies * Number_Users)) * 100), 1)
density2
## [1] 9.3

dataMatrix <- as(data, "realRatingMatrix")
dataMatrix




#This is movie lens dataset
# recommender package loading

library(recommenderlab)
help(package ="recommenderlab")

set.seed(1)

# datasets 
data_package <- data(package ="recommenderlab")
data_package
data_package$results[,"Item"]
# data("MovieLense")

MovieLense <- dataMatrix

class(MovieLense)
methods(class = class(MovieLense))
methods(class = "realRatingMatrix")

object.size(MovieLense) # realRatingMatrix 
object.size(as(MovieLense,"matrix")) # matrix is size much more than realratingmatrix
object.size(as(MovieLense,"matrix")) / object.size(MovieLense)

similarity_users <- similarity(MovieLense[1:4,], method="cosine", which="users")
class(similarity_users)
as.matrix(similarity_users)

similarity_users1 <- similarity(MovieLense[1:4,], method="pearson", which="users")
class(similarity_users1)
as.matrix(similarity_users1)

similarity_users2 <- similarity(MovieLense[1:4,], method="jaccard", which="users")
class(similarity_users2)
as.matrix(similarity_users2)

image(as.matrix(similarity_users),main="User similarity")
image(as.matrix(similarity_users1),main="User similarity")
image(as.matrix(similarity_users2),main="User similarity")

recommender_models <- recommenderRegistry$get_entries(dataType="realRatingMatrix")

names(recommender_models)

lapply(recommender_models,"[[", "description")
recommender_models


recommender_models$IBCF_realRatingMatrix$parameters

library("ggplot2")
dim(MovieLense)

slotNames(MovieLense)

class(MovieLense@data)

dim(MovieLense@data)

#exploring the values of the rating 
vector_ratings <- as.vector(MovieLense@data)
unique(vector_ratings)

table_ratings <- table(vector_ratings)
table_ratings

vector_ratings <- vector_ratings[vector_ratings != 0]

vector_ratings <- as.factor(vector_ratings)

qplot(vector_ratings) +
  ggtitle("Distribution of the ratings")

views_per_movie <- colCounts(MovieLense)

table_views <- data.frame(
  movie = names(views_per_movie),
  views = views_per_movie
)

table_views <- table_views[order(table_views$views, decreasing = TRUE),]

ggplot(table_views[1:6,],aes(x=movie, y = views)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45, hjust = 1) ) +
  ggtitle("Number of views of the top movies")


# explore the average ratings 

average_ratings <- colMeans(MovieLense)


qplot(average_ratings) +
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average movie rating")

average_ratings_relevant <- average_ratings[views_per_movie > 10 ]

qplot(average_ratings_relevant) +
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average movie rating")

image(MovieLense,main="Heatmap of the rating matrix")

image(MovieLense[1:10,1:15],main="Heatmap of the rating matrix")

min_n_movies <- quantile(rowCounts(MovieLense),0.99)
min_n_users <- quantile(colCounts(MovieLense),0.99)

min_n_movies

min_n_users

image(MovieLense[rowCounts(MovieLense) > min_n_movies,colCounts(MovieLense) > min_n_users],main="Heatmap of the rating matrix")


# rating_movies <- MovieLense[rowCounts(MovieLense) > 2,colCounts(MovieLense) > 1]
rating_movies <- MovieLense
min_movies <- quantile(rowCounts(rating_movies),0.98)
min_users <- quantile(colCounts(rating_movies),0.98)

image(MovieLense[rowCounts(MovieLense) > min_movies,colCounts(MovieLense) > min_users],main="Heatmap of the top uses and movies")

average_ratings_per_user <- rowMeans(rating_movies)

qplot(average_ratings_per_user) +
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average rating per user")

ratings_movies_norm <- normalize(rating_movies)

sum(rowMeans(ratings_movies_norm) > 0.00001)

image(ratings_movies_norm[rowCounts(ratings_movies_norm) > min_movies, 
                          colCounts(ratings_movies_norm) > min_users], 
      main = "Heatmap of the top users and movies")

ratings_movies_watched <- binarize(rating_movies, minRating = 1)

min_movies_binary <- quantile(rowCounts(rating_movies),0.95)
min_users_binary <- quantile(colCounts(rating_movies),0.95)

image(ratings_movies_watched[rowCounts(rating_movies) > min_movies_binary,
                             colCounts(rating_movies) > min_users_binary],
      main="Heatmap of the top users and movies ")


ratings_movies_good <- binarize(rating_movies, minRating = 3)

image(ratings_movies_good[rowCounts(rating_movies) > min_movies_binary,
                          colCounts(rating_movies) > min_users_binary],
      main="Heatmap of the top users and movies ")


# item based collaborative filtering

# training and test set 

which_train <- sample(x = c(TRUE, FALSE), size = nrow(rating_movies),replace = TRUE, prob=c(0.8,0.2))
recc_data_train <- rating_movies[which_train,]

recc_dataa_test <- rating_movies[!which_train,]
dim(recc_data_train)
head(recc_data_train)
which_set <- sample(x = 1:5, size =nrow(rating_movies), 
                    replace= TRUE )

for (i_model in 1:5 ){
  which_train <- which_set == i_model 
  recc_data_train <- rating_movies[which_train,]
  recc_data_test <- rating_movies[!which_train,]
}


# building the recommendation model 

recc_model <- Recommender(data = recc_data_train,method="IBCF", parameter = list(k = 30))
nrow(recc_data_test)
class(recc_model)


# exploring the recommended model 

model_details <- getModel(recc_model)
model_details$description

model_details$sim

class(model_details$sim)

dim(model_details$sim)

n_items_top <- 20

image(model_details$sim[1:n_items_top, 1:n_items_top], main = "Heatmap of the first rows and columns" )

model_details$k

row_sums <- rowSums(model_details$sim > 0)

table(row_sums)

col_sums <- colSums(model_details$sim > 0)

qplot(col_sums) + stat_bin(binwidth = 1 ) +
  ggtitle("Distribution of the column count")

which_max <- order(col_sums, decreasing = TRUE)[1:6]

rownames(model_details$sim)[which_max]


n_recommend <- 6

recc_predicted <- predict(object= recc_model, newdata = recc_data_test, n= n_recommend)

class(recc_predicted)


recc_user_1 <- recc_predicted@items[[1]]
# View(recc_user_1)
movies_user_1 <- recc_predicted@itemLabels[recc_user_1]

movies_user_1

recc_matrix <- sapply(recc_predicted@items,
                      function(x) {
                        colnames(rating_movies) [x]
                      })

dim(recc_matrix)

recc_matrix[,1:4]

number_of_items <- factor(table(recc_matrix))
chart_title <-  "Distribution of the number of items for IBCF"

qplot(number_of_items) +
  ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing =  TRUE)
number_of_items_top <- head(number_of_items_sorted, n= 4)
table_top <- data.frame(names(number_of_items_top),number_of_items_top)
table_top

