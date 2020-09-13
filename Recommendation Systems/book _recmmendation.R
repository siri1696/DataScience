
#Installing and loading the libraries
install.packages("recommenderlab", dependencies=TRUE)
install.packages("Matrix")
library("recommenderlab")
library(caTools)

#movie rating data
book_rate_data <- read.csv(file.choose())

#metadata about the variable
str(book_rate_data)
summary(book_rate_data)


#rating distribution
hist(book_rate_data$Book.Rating)
hist(book_rate_data$User.ID)

boxplot(book_rate_data$Book.Rating)


#the datatype should be realRatingMatrix inorder to build recommendation engine
book_rate_data_matrix <- as(book_rate_data[-1], 'realRatingMatrix')

#Popularity based 

book_recomm_model1 <- Recommender(book_rate_data_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(book_recomm_model1, book_rate_data_matrix[413:414], n=5)
as(recommended_items1, "list")
names(recommended_items2)


## Popularity model recommends the same books for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

book_recomm_model2 <- Recommender(book_rate_data_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(book_recomm_model2, book_rate_data_matrix[413:417], n=5)
as(recommended_items2, "list")

