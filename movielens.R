# This file was created from the Rmd file using the knitr::purl command as shown below
# knitr::purl("movielens.Rmd")

# It is essentially the same file as as the Rmd file
# Rmd code sections are still visible as comments and have the code chunk name and options shown
# The code for the kable functions in the Rmd file was specific to LaTeX, so it was altered
# to support general purpose display.

## ----setup, include=TRUE, message=FALSE, warning=FALSE, echo=TRUE----------------

#Load the libraries
library(tidyverse)
library(caret)
library(knitr)
library(kableExtra)
library("FactoMineR")
library("factoextra")
library(recosystem)

#turn off scientific notation and set number of digits to print
options(scipen = 999, digits = 8)

#Function to calculate Root Mean Square Error
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#create a "not in" function
`%notin%` <- Negate(`%in%`)

#create a cleanup function to remove unneeded variables from memory
cleanup <- function(keep = ""){
  # set up a default keep list
  keep_default <- c("RMSE","rcst","create_test_and_train",
                    "run_size", "%notin%", "cleanup", "rmse_results",
                    "start_time", "update_results_table", 
                    "display_results_table")
  # concatenate the user specified keep list to the default
  keep <- c(keep_default, keep)
  # get a list of all objects in the global environment
  all_objects <- ls(envir = .GlobalEnv)
  # set up the index for items to be deleted
  ind <- all_objects %notin%  keep
  # delete the unwanted items
  rm( list = all_objects[ind], envir = .GlobalEnv)
} 

# This function simplifies the update of the rmse_results after each model run.
update_results_table  <- function(results, method, type) {
  # the <<- operator updates the global rmse_results object by reference
  rmse_results <<- bind_rows(
    rmse_results,
    tibble(
      Method = method,
      Type = type,
      RMSE = results
    )
  )
}

# This function simplifies the display of the rmse_results
# unlike the Rmd version there is no conditional formatting
display_results_table <- function(caption) {
  rmse_results %>%
    kable(caption = caption) %>%
    kable_styling(latex_options = "scale_down")
}

# The rcst function is used in the PCA analysis
# The rcst (reconstruct) function converts the normalized y matrix back to 
# a set of predicted ratings. It essentially reverses the process used to
# originally construct the y matrix.
rcst <- function(m){
  m <- sweep(m, 1,  y_row_means2, "+")
  m <- sweep(m, 2, y_col_means, "+")
  m <- sweep(m, 1, y_row_means1, "+")
}

#run_size controls run size throughout the project. 
#For a full formal analysis, set the size to 1.0
#For a quick test run that completes in under 10 minutes, 
#(without the initial data download - turn that off)
#  set the run_size to 0.001 
#RMSE results will be significantly worse for small test runs
run_size <- 1



## ----data_load-------------------------------------------------------------------
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# # if using R 3.6 or earlier
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
# if using R 4.0 or later
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

###############################################################################
# Data Sets were saved to avoid the need to download the data
# and rerun this code chunk each time.
###############################################################################
# save datasets
write_rds(movielens, "movielens.rda") #save the full file
write_rds(edx, "edx.rda") #the training set
write_rds(validation, "validation.rda") #the validation set
###############################################################################
# remove temporary objects
cleanup(keep = c("edx", "validation"))



## ----edx_validation--------------------------------------------------------------
#look at the structure of the edx data set
edx <- read_rds("edx.rda")
str(edx, strict.width = "cut")

# print a table with the top 15 rows
edx %>% slice_head(n=15) %>%
  kable(caption = "edx Data Set - First 15 rows") %>%
  kable_styling(latex_options = "scale_down")


## ----validation_data_validation--------------------------------------------------
validation <- read_rds("validation.rda")
#look at the structure of the edx data set
str(validation, strict.width = "cut")
validation %>% slice_head(n=15) %>% 
  kable(caption = "Validation Data Set - First 15 rows") %>%
  kable_styling(latex_options = "scale_down")

# remove the validation data from memory
rm(validation)


## ----edx_size, echo=FALSE--------------------------------------------------------
#determine the number of movies
m <- edx %>% 
  group_by(movieId)%>% 
  summarize(count=n()) %>% 
  nrow()

#determine the number of users
u <- edx %>% 
  group_by(userId)%>% 
  summarize(count=n()) %>% 
  nrow()

#display the number of movies and users as well as the percent occupancy
cat("The matrix of", u, "users and", m, "movies contains", m*u, 
    "cells. \nSince edx contains", nrow(edx), "observations, the matrix is",
    nrow(edx) * 100/(m*u), "% occupied.")


## ----ratings, echo=FALSE, fig.cap="Rating Frequency"-----------------------------
# plot a histogram of ratings
edx %>% 
  group_by(rating) %>%
  ggplot(aes(x = rating)) +
  geom_histogram(bins = 10, col="red") +
  labs(title = "Rating Frequency") 



## ----popularity, echo=FALSE, fig.cap="Movie Popularity"--------------------------
#plot movie popularity
edx %>%
  group_by(movieId, title) %>%
  summarize(popularity = n()) %>%
  ggplot(aes(x = popularity)) +
  geom_histogram(col="red") +
  labs(title = "Movie Popularity",
       x="Number of Ratings (Log 10)") +
  scale_x_log10()



## ----most_popular, echo=FALSE----------------------------------------------------
#plot most popular movies
edx %>%
  group_by(movieId, title) %>%
  summarize(popularity = n()) %>%
  arrange(desc(popularity)) %>%
  head(n = 15)  %>%
  kable(caption = "Most Popular Movies")%>%
  kable_styling(latex_options = "scale_down")


## ----movie_ratings, fig.cap="Movie Ratings Example"------------------------------
# show the ratings for the Shawhank Redemption - movieId 318
ssratings <- edx %>% 
  filter(movieId == 318)

cat("The Shawshank Redemption received", nrow(ssratings),
    "\nThe mean rating was", mean(ssratings$rating),
    "\nThe standard deviation was", sd(ssratings$rating))

ssratings %>%   ggplot(aes(rating)) +
  geom_histogram(col="red", bins = 10) +
  labs(title = "Shawshank Redemption Ratings",
       x="Number of Ratings")



## ----users, fig.cap="User Rating Activity"---------------------------------------
#plot a histogram of user rating activity
edx %>%
  group_by(userId) %>%
  summarize(activity = n()) %>%
  ggplot(aes(activity)) +
  geom_histogram(col="red") +
  labs(title = "User Activity",
       x="Number of Ratings (Log 10)") +
  scale_x_log10()


## ----individual_user_ratings, fig.cap="User Ratings Example"---------------------
#show the rating history for user 318
user <- edx %>% 
  filter(userId == 318)

cat("This user produced", nrow(user), "ratings.",
    "\nThe mean rating was", mean(user$rating),
    "\nThe standard deviation was", sd(user$rating))

user %>%   ggplot(aes(rating)) +
  geom_histogram(col="red", bins = 10) +
  labs(title = "User 318 Ratings",
       x="Number of Ratings")



## ----individual_user_ratings2, fig.cap="User Ratings Example 2"------------------
#show the rating history for user 831
user <- edx %>% 
  filter(userId == 831)

cat("This user produced", nrow(user), "ratings.",
    "\nThe mean rating was", mean(user$rating),
    "\nThe standard deviation was", sd(user$rating))

user %>%   ggplot(aes(rating)) +
  geom_histogram(col="red", bins = 10) +
  labs(title = "User 831 Ratings",
       x="Number of Ratings")


## ----genres1, fig.cap="Genre Preference Example"---------------------------------
#create a genre preference df for user 318
genre_preference <- edx %>% 
  filter(userId == 318) %>%
  group_by(genres) %>%
  summarize(rating = mean(rating), userId = first(userId)) %>%
  arrange(rating) %>%
  mutate(genres = factor(genres, levels = genres))

cat("For user 318, there are", nrow(genre_preference),
    "combinations of genres that were rated.")

#plot the genre preference of user 318
genre_preference %>% 
  ggplot(aes(genres, rating)) +
  geom_point(col = "red") +
  labs(title = "User 318 Average Rating by Genre",
       x="Genres Sorted by User 318 Average Ratings",
       y = "Average Rating") +
  theme(axis.text.x = element_blank())



## ----genres2, fig.cap="Genre Preference Example 2"-------------------------------
#create a genre preference df for user 831
genre_preference2 <- edx %>% 
  filter(userId == 831)%>%
  group_by(genres) %>%
  summarize(rating = mean(rating), userId = first(userId)) %>%
  arrange(rating) %>%
  mutate(genres = factor(genres, levels = genres))

#combine the genre preference df for users 318 and 831
gp3 <- rbind(genre_preference, genre_preference2)

#plot the contrasting genre preference for the two users
gp3 %>% 
  mutate(userId = as.factor(userId))%>%
  ggplot(aes(genres, rating)) +
  geom_point(aes(col = userId)) +
  labs(title = "User 318 and 831 Average Rating by Genre",
       x="Genres Sorted by User 318 Average Ratings",
       y = "Average Rating")+
  theme(axis.text.x = element_blank())



## ----genres_impact, echo=FALSE, message=FALSE, warning=FALSE---------------------
# create a df that summarizes average rating by genres
e <- edx %>%
  group_by(genres) %>%
  summarize(avg_rating = mean(rating)) %>%
  arrange(desc(avg_rating))

#display the highest rated and lowest rated genres
e %>% head(20) %>% kable(caption = "Highest Rated Genres")%>%
  kable_styling(latex_options = "scale_down")

e %>% tail(20) %>% kable(caption = "Lowest Rated Genres")%>%
  kable_styling(latex_options = "scale_down")



## ----genre_plot, fig.cap="Genres Confidence Interval 1"--------------------------

#summarize the list of movies and ratings
edx <- read_rds("edx.rda")

#create a list of movies with average rating and compound genres columns
movie_list <- edx %>%
  group_by(movieId) %>%
  summarize(title = first(title),
            #year = first(year),
            genres = first(genres),
            movie_rating = mean(rating))

#split out genres into a character vector. There are 20 simple genres
simple_genres <- levels(as.factor(edx$genres)) %>%
  str_split("\\|", simplify = FALSE) %>%
  unlist()%>%
  unique()

# The max number of simple genres found in any compound rating was 7
genre_split <- c("genres_1", "genres_2", "genres_3", "genres_4", "genres_5", "genres_6", "genres_7")

#create a df that contains the ratings and 7 genres columns with the 
#simple genres listed or an NA
t <- edx %>% separate(genres, into = genre_split, sep = "\\|") 

#pivot the 7 genres columns into one genres column and drop the NA genres
simple_genres_user_rating <- t %>%
  pivot_longer(cols = starts_with("genres_")) %>%
  rename(genres = value) %>%
  filter(!is.na(genres))

# calculate the mean and confidence intervals by simple genres
# plot the error bars
simple_genres_user_rating %>% 
  mutate(genres = as.factor(genres)) %>%
  group_by(genres) %>%
  summarize(Average = mean(rating), se = sd(rating))%>%
  mutate(ymin = Average - qnorm(0.975)*se,
         ymax = Average + qnorm(0.975)*se) %>%
  mutate(genres = reorder(genres, Average))%>% 
  ggplot(aes(x = genres, ymin = ymin, ymax = ymax)) +
  geom_errorbar() +
  geom_point(aes(genres, Average)) +
  coord_flip() +
  labs(title = "Simple Genre",
       subtitle = "Confidence Intervals",
       x = "Simple Genre",
       y = "Ratings") 


## ----genre_plot2, fig.height= 5, fig.cap="Genres Confidence Interval 2"----------
# create a sample of 30 compound genres. 
# There are too many to reasonably plot otherwise.
sample_genres <- edx %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  slice_sample(n=30)

#calculate the average and confidence intervals for the compound genres
#plot the error bars
edx %>% 
  filter(genres %in% sample_genres$genres)%>%
  mutate(genres = as.factor(genres)) %>%
  group_by(genres) %>%
  summarize(Average = mean(rating), se = sd(rating))%>%
  mutate(ymin = Average - qnorm(0.975)*se,
         ymax = Average + qnorm(0.975)*se) %>%
  mutate(genres = reorder(genres, Average)) %>%
  ggplot(aes(x = genres, ymin = ymin, ymax = ymax)) +
  geom_errorbar() +
  geom_point(aes(genres, Average)) +
  coord_flip() +
  labs(title = "Genres Combinations",
       subtitle = "Sample of 30 - Confidence Intervals",
       x = "Genres Combinations",
       y = "Ratings") +
  theme(axis.text.x = element_text(hjust=.35))

cleanup()


## ----create_test_and_train, echo=TRUE--------------------------------------------
# Function to create test and training data frames from edx
# The function returns a list containing the two data frames.
create_test_and_train <- function(proportion = 1, 
                                  save = TRUE, 
                                  seed = 831, 
                                  data_frame = "default"){
  # The edx data set is the training dataset for this lab. 
  # By default(proportion = 1) we will use the full edx data set
  # If proportion is set to a number between zero and one, 
  # we will use the proportion specified.
  # if proportion is not between 0 and 1 we will generate an error.
  if (proportion < 0 | proportion > 1){
    stop("Error in create_test_and_train function, proportion value not between 0 and 1.")
  } 
  #set the seed value for repeatablity between runs
  set.seed(seed, sample.kind="Rounding")
  # if using R 3.5 or earlier, use `set.seed(seed)` instead
  
  if(data_frame == "default"){
     edx <- read_rds("edx.rda") 
  } else{
    edx <- data_frame
  }
  
  #keep specified proportion of randomly selected users
  temp <- edx %>% group_by(userId) %>% 
    summarize(count = n()) #summarize preserves the grouping
  #randomly select percentage of the userIds to keep
  ind_users <- sample(c(0,1), 
                      nrow(temp),
                      replace=TRUE,
                      prob = c(1-proportion, proportion)) %>% 
    as.logical()
  
  selected_users <- temp$userId[ind_users]
  edx_small <- edx %>% 
    filter(userId %in% selected_users)
  
  #strip out movies with 0 reviews
  selected_movies <- edx_small %>% 
    group_by(movieId) %>% 
    summarize(count = n()) %>%
    filter(count > 0)
  
  edx_small <- edx_small %>% filter(movieId %in% selected_movies$movieId) 
  
  #build a small test and training set from edx_small
  test_index <- createDataPartition(y = edx_small$rating, times = 1,
                                    p = 0.2, list = FALSE)
  train_set <- edx_small[-test_index,]
  temp <- edx_small[test_index,]
  test_set <- temp %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(temp, test_set)
  train_set <- rbind(train_set, removed)
  
  # Save the test and train sets in the working directory if save = TRUE
  if(save == TRUE){
    write_rds(train_set, "train_set.rda")
    write_rds(test_set, "test_set.rda")
  }
  
  # return test and train data sets as a list
  test_and_train_list <- list(train_set = train_set, test_set = test_set)
}



## ----base_model, echo=FALSE------------------------------------------------------
# Create the test and training data sets
df_list <- create_test_and_train(proportion = run_size)
train_set <- df_list$train_set
test_set <- df_list$test_set

#RMSE Function has been moved to the setup section

#First run using just the movie mean as the predictor
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
# create the df that stores the model results
rmse_results <- tibble(Method = "Just the average", 
                       Type = "Test",
                       RMSE = naive_rmse)

#add movie impact
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)

update_results_table(results = model_1_rmse,
             method = "Movie Effect Model",
             type = "Test")

#add user impact
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)

update_results_table(results = model_2_rmse,
             method = "Movie + User Effects Model",
             type = "Test")



#create a data frame that includes movie ID and titles
movie_titles <- train_set %>% 
  select(movieId, title) %>%
  distinct()

#regularize the movie rating 
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

#repredict using regularized movies
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)

update_results_table(results = model_3_rmse,
             method = "Regularized Movie Effect Model",
             type = "Test")


# use a tuning parameter for movie effect only
lambdas <- seq(2, 5, 0.5)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

l_m <- lambdas[which.min(rmses)] #test to save the best movie lambda

# use a tuning parameter for movie and user effects
lambdas <- seq(1, 5, 0.5)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

lambda <- lambdas[which.min(rmses)]

update_results_table(results = min(rmses),
             method = "Regularized Movie + User Effect Model",
             type = "Test")

display_results_table("Base Model Results")



## ----separate_lambdas, fig.cap="User Lambdas", echo=TRUE-------------------------
#Separate movie and user lambda's
#this is the same as the base model, except for l_m
#l_m provides a separate lambda for movies, user lambda remained simply l
l_m <- lambda
lambdas <- seq(4, 6, 0.1)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l_m))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

l <- lambdas[which.min(rmses)]
cat("The best value of user lambda tested was",  l, "\n")



## ----movie_lambdas, fig.cap="Movie Lambdas"--------------------------------------
lambdas <- seq(4, 6, 0.1)
rmses <- sapply(lambdas, function(l_m){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l_m))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses) 

l_m <- lambdas[which.min(rmses)]
cat("The best value of movie lambda tested was",  l_m)

update_results_table(results = min(rmses),
             method = "Regularized Movie + User Effect Model: Separate Lambdas",
             type = "Test")


display_results_table("RMSE Results")

#Save the predicted ratings from this model for use in the PCA analysis
# l holds the user lambda
# l_m holds the movie lambda
mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l_m))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  best_predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u, residual = rating-pred) 
  
#save these ratings to a file for use in an ensemble
write_rds(best_predicted_ratings, "base_model_predictions.rda")



## ----error_plot, fig.cap="Prediction Error"--------------------------------------
#plot the residuals by rating
best_predicted_ratings %>% 
  mutate(rating = as.factor(rating)) %>%
  ggplot(aes(rating, residual)) +
  geom_boxplot(color = "blue") +
  labs(title = "Residual by Rating",
       x="Actual Rating",
       y = "Residual Based on Prediction")
cleanup()


## ----initial_pca, echo=TRUE------------------------------------------------------

#load a small data set
df_list <- create_test_and_train(proportion = 0.005, save=FALSE)
train_set <- df_list$train_set

# set y_nmov = the number of movies in the training set (for later use)
y_nmov = train_set %>% group_by(movieId) %>% summarize(obs = n()) %>% nrow()
# set y_nmov = the number of movies in the training set (for later use)
y_nusers = train_set %>% group_by(userId) %>% summarize(obs = n()) %>% nrow()

#convert the dataset into a matrix with movies in columns and users in rows
#y_u,i is the entry in row u and column i => for user u and movie i
y <- train_set %>%
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = movieId, values_from = rating) %>%
  as.matrix()

movie_titles <- train_set %>%
  select(movieId, title) %>%
  distinct()
# save movieId by column for use during RMSE prediction
movieId_by_col <- colnames(y)

#add rownames and column names to facilitate exploration
rownames(y)<- y[,1]
y <- y[,-1]     #drop the userId column (the rowname now has the userId)
colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

#convert to residuals by removing the row and column averages
y_row_means1 <- rowMeans(y, na.rm=TRUE) #save the rowMeans for reconstructing the matrix
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y_col_means <- colMeans(y, na.rm=TRUE)
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))

#Remove NAs and zeroes by sweeping out the row means one more time
y[is.na(y)] <- 0  #make the residuals with NAs = 0
y_row_means2 <- rowMeans(y, na.rm=TRUE)
y <- sweep(y, 1, rowMeans(y))

#Set a value for max principal components
my_ncp <- 10
res.pca <- PCA(y, scale.unit = FALSE, graph = FALSE, ncp = my_ncp)

#We’ll use the factoextra R package to help in the interpretation of PCA. 

#eigenvalues measure the amount of variation retained by each principal component.
eig.val <- get_eigenvalue(res.pca)
kable(head(eig.val, n=10), caption = "Top 10 Dimensions and Eigenvalues") %>%
  kable_styling(latex_options = "scale_down")



## ----initial_pca2, fig.cap="Scree Plot"------------------------------------------
#An alternative method to determine the number of principal components is to 
# look at a Scree Plot, 
fviz_screeplot(res.pca, ncp=my_ncp)
# Plot the users (individuals) with the most impact




## ----initial_pca3, fig.cap="Dimension 1 & 2 User Impact"-------------------------
fviz_pca_ind(res.pca, col.var = "black", 
             select.ind = list(cos2 = 20),
             title = "Top 20 Users With The Most Impact" )



## ---- fig.cap="Dimension 1 & 2 Movie Impact"-------------------------------------
# Plot the movies (variables) with the most impact
fviz_pca_var(res.pca, col.var = "black", 
             select.var = list(cos2 = 20),
             title = "Top 10 Movies With The Most Impact",
             repel = TRUE)



## ----most_popular_data, echo=TRUE------------------------------------------------
#set the run size to no more than 15% of the most popular movies
proportion <- if_else(run_size<0.15, run_size, 0.15)

edx <- read_rds("edx.rda")
full_size <- nrow(edx)
most_rated <- edx %>% 
  group_by(movieId) %>%
  summarize(count = n(), title = first(title)) %>%
  arrange(desc(count)) %>%
  slice_head(prop = proportion)
edx <- edx %>% filter(movieId %in% most_rated$movieId)
sample_size <- nrow(edx)
cat("Top", proportion, "percent of the movies contain", sample_size*100/full_size, "percent of the ratings. \n")

df_list <- create_test_and_train(data_frame = edx)
train_set <- df_list$train_set
test_set <- df_list$test_set

cat("After splitting into test and train, the train set retains ", nrow(train_set)*100/full_size, "percent of the ratings.")




## ----edx_popular size, echo=TRUE-------------------------------------------------
m <- edx %>% 
  group_by(movieId)%>% 
  summarize(count=n()) %>% 
  nrow()

u <- edx %>% 
  group_by(userId)%>% 
  summarize(count=n()) %>% 
  nrow()

cat("The matrix of", u, "users and", m, "movies contains", m*u, 
    "cells. \nSince  our popular edx subset contains", nrow(edx), 
    "observations, \nthe matrix is",
    sample_size * 100/(m*u), 
    "% occupied. \nThis is up from a 1.2% occupancy in the original matrix")

# Remove unneeded objects
cleanup(keep = c("train_set", "test_set"))


## ----most_popular_pca, echo=TRUE-------------------------------------------------

# set y_nmov = the number of movies in the training set (for later use)
y_nmov = train_set %>% group_by(movieId) %>% summarize(obs = n()) %>% nrow()
# set y_nmov = the number of movies in the training set (for later use)
y_nusers = train_set %>% group_by(userId) %>% summarize(obs = n()) %>% nrow()

#convert the dataset into a matrix with movies in columns and users in rows
#y_u,i is the entry in row u and column i => for user u and movie i
y <- train_set %>%
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = movieId, values_from = rating) %>%
  as.matrix()

rm(train_set)

#add rownames and column names to facilitate exploration
rownames(y)<- y[,1]
y <- y[,-1]     #drop the userId column (the rowname now has the userId)

#save the rowMeans for reconstructing the matrix
y_row_means1 <- rowMeans(y, na.rm=TRUE) 
#convert to residuals by removing the row and column averages
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y_col_means <- colMeans(y, na.rm=TRUE)
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))

#back to the matrix developed from the actual data
y[is.na(y)] <- 0  #make the residuals with NAs = 0
y_row_means2 <- rowMeans(y, na.rm=TRUE)
y <- sweep(y, 1, rowMeans(y))

#Set a value for max principal components
my_ncp <- 10

# Run the PCA analysis
res.pca <- PCA(y, scale.unit = FALSE, graph = FALSE, ncp = my_ncp)

# Produce the forecast matrix
res.pca2 <- res.pca

# rec contains the predicted pca impact, but they are zeroed
rec <- reconst(res.pca2, ncp = my_ncp)

#This works to reconstruct the forecast from zeroed to comparable values
rec2 <- rcst(rec) 

cleanup(keep = c("rec2", "test_set")) #free up memory

#calculate the predictions
pred <- as_tibble(rec2, rownames=NA) %>% 
  rownames_to_column(var="userId") %>%
  pivot_longer(-userId, names_to = "movieId", values_to = "prediction") %>%
  mutate(userId = as.integer(userId), movieId = as.integer(movieId))

rm(rec2) #free up memory

# save the predictions for future use
write_rds(pred, "pca_popular.rda") 

#join with test set to get ratings 
pred <- test_set %>% left_join(pred, by=c("userId","movieId"))
 
update_results_table(results = RMSE(test_set$rating, pred$prediction),
             method = "Most Popular Movies PCA",
             type = "Test")

display_results_table("RMSE Results")



## ----ensemble, echo=TRUE---------------------------------------------------------
#Memory has been an issue here, so let's clean up and collect garbage
cleanup()
gc(verbose = FALSE)

# load the data needed for prediction
bmp <- read_rds("base_model_predictions.rda") #from the test data set
pca_pop <- read_rds("pca_popular.rda")

# load a clean version of the training and test set
df_list <- create_test_and_train(proportion = run_size)
train_set <- df_list$train_set
test_set <- df_list$test_set

#create the ensemble
ensemble <- bmp %>% 
  left_join(pca_pop, by=c("movieId", "userId")) %>%
  rename(pred_base = pred,
         pred_pca_pop = prediction)

#percent of observations with missing data from the pca popular analysis
mean(is.na(ensemble$pred_pca_pop))

#create the ensemble prediction
#the predictor is the pca result. If pca is NA, the base result is used.
ens <- ensemble %>% 
  mutate(ensemble_prediction = ifelse(is.na(pred_pca_pop),pred_base, pred_pca_pop)) %>%
  right_join(test_set,by=c("movieId", "userId")) %>%
  mutate(ensemble_prediction = ifelse(ensemble_prediction > 4.8, 4.8, ensemble_prediction),
         ensemble_prediction = ifelse(ensemble_prediction < 1.0, 1.0, ensemble_prediction))

#calculate the RSME
ensemble_rsme <- RMSE(ens$rating.x, ens$ensemble_prediction)

update_results_table(results = ensemble_rsme,
             method = "PCA Ensemble Prediction",
             type = "Test")

display_results_table("RMSE Results")



## ----recosystem, echo=TRUE-------------------------------------------------------
set.seed(831, sample.kind = "Rounding") 

#read in the train and test data
train_set <- read_rds("train_set.rda")
test_set <- read_rds("test_set.rda")

# Convert the train and test sets into recosystem input format
train_data <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
test_data  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

# Create the model object
r <-  recosystem::Reco()

# Tuning parameters were set per the recosystem vignette,
# With the exception of nthreads which will be set to the number of cores
cores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)
opts <- r$tune(train_data, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = cores, 
                                       niter = 10))
# Tuning is a time-consuming step, but producte a reuseable set of options
# Save the options for future use
write_rds(opts, "recosys_opts_movies.rda")

# Train the algorithm  
r$train(train_data, opts = c(opts$min, 
                             nthread = cores, 
                             niter = 50,
                             verbose = FALSE))

# Calculate the predicted values  
pred <-  r$predict(test_data, out_memory())

update_results_table(results = RMSE(test_set$rating, pred),
             method = "Matrix Factorization",
             type = "Test")


# Calculate results after clipping extreme predictions back
clip <- function(prediction){
  if (prediction > 4.75){
    prediction <- 4.75
  } else if (prediction < 0.75){
    prediction <- 0.75
  }
  return(prediction)
}

pred <- sapply(pred, clip)

update_results_table(results = RMSE(test_set$rating, pred),
             method = "Matrix Factorization with Clipping",
             type = "Test")

display_results_table("RMSE Results")


cleanup()



## ----recosystems_genres, echo=TRUE-----------------------------------------------
#Run recosystems and this time recommend genres instead of movies

#For the recosystem model to work, we neet to convert genres to factors
#so that they can be treated as an integer numeric. We will perform that
#conversion on the edx data frame before sending it to the test and train
#create function. This will ensure that the factor levels are consistent
#on the test and train data frames
edx_factors <- read_rds("edx.rda") %>%
  mutate(genres = as.factor(genres))

df_list <- create_test_and_train(proportion = run_size,
                                 data_frame = edx_factors)
train_set <- df_list$train_set
test_set <- df_list$test_set

set.seed(831, sample.kind = "Rounding") 

# Convert the train and test sets into recosystem input format
train_data <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = genres, 
                                           rating     = rating))
test_data  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = genres, 
                                           rating     = rating))

# Create the model object
r <-  recosystem::Reco()

# Tuning parameters were set per the recosystem vignette,
# With the exception of nthreads which will be set to the number of cores
cores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)
opts <- r$tune(train_data, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = cores, 
                                       niter = 10))

# Tuning is a time-consuming step, but producte a reuseable set of options
# Save the options for future use
write_rds(opts, "recosys_opts_genres.rda")

# Train the algorithm  
r$train(train_data, opts = c(opts$min, 
                             nthread = cores, 
                             niter = 50, 
                             verbose=FALSE))

# Calculate the predicted values  
pred <-  r$predict(test_data, out_memory())

update_results_table(results = RMSE(test_set$rating, pred),
             method = "Matrix Factorization of Genres",
             type = "Test")

display_results_table("RMSE Results - Full Test Suite")



## ----mf_ensemble, echo=TRUE------------------------------------------------------

cleanup()

edx_factors <- read_rds("edx.rda") %>%
  mutate(genres = as.factor(genres))

df_list <- create_test_and_train(proportion = run_size,
                                 data_frame = edx_factors)
train_set <- df_list$train_set
test_set <- df_list$test_set

set.seed(831, sample.kind = "Rounding") 

# Convert the train and test sets into recosystem input format
train_data <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
test_data  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

########################################################
# Recreate the MF Movies Model
cores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

#load the tuning parameters
opts <- read_rds("recosys_opts_movies.rda")

#create the recosystem modeling object
r <-  Reco()

#train the model
r$train(train_data, opts = c(opts$min, 
                             nthread = cores, 
                             niter = 50, 
                             verbose=FALSE))

# Calculate the predicted values for movies  
pred <-  r$predict(test_data, out_memory())

########################################################
# Recreate the MF Genres Model

# load the saved tuning options
opts <- read_rds("recosys_opts_genres.rda")

# Convert the train and test sets into recosystem input format
train_data <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = genres, 
                                           rating     = rating))
test_data  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = genres, 
                                           rating     = rating))
# train the model
r$train(train_data, opts = c(opts$min, 
                             nthread = cores, 
                             niter = 50, 
                             verbose=FALSE))

# Calculate the predicted values  
pred2 <-  r$predict(test_data, out_memory())

# Create the Matrix Factorization Ensemble
ensemble_mf <- tibble(movies = pred, genres = pred2)
# add clipping parameters to the ensemble
ens <- ensemble_mf %>% 
  mutate(prediction = (movies * 0.94) + (genres * 0.06)) %>%
  mutate(prediction = ifelse(prediction > 4.75, 4.75, prediction),
         prediction = ifelse(prediction < 0.75, 0.75, prediction))

#calculate the rmse results
ensemble_rsme <- RMSE(test_set$rating, ens$prediction)

update_results_table(ensemble_rsme,
                     method = "Matrix Factorization Ensemble",
                     type = "Test")
display_results_table("Final Results")



## ----pca_ensemble_validation, echo=TRUE------------------------------------------

# run against the validation set
validation <- read_rds("validation.rda")
# Use the entire edx data set to train the model
# Run results have demonstrated that the larger data sets yield better results
train_set <- read_rds("edx.rda")
# input the results from the pca most popular movie analysis
pca_pop <- read_rds("pca_popular.rda")

# l holds the user lambda 
l <- 5.1 
# l_m holds the movie lambda
l_m <- 4.7 
mu <- mean(train_set$rating)
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l_m))
b_u <- train_set %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))
best_predicted_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u, residual = rating-pred)
ensemble <- best_predicted_ratings %>%
  left_join(pca_pop, by=c("movieId", "userId")) %>%
  rename(pred_base = pred,
         pred_pca_pop = prediction)

# create the ensemble prediction
# the predictor is the pca result. If pca is NA, the base result is used.
# we will also add clipping logic to the ensemble prediction
ens <- ensemble %>%
  mutate(ensemble_prediction = ifelse(is.na(pred_pca_pop),pred_base, pred_pca_pop)) %>%
  right_join(validation,by=c("movieId", "userId")) %>%
  mutate(ensemble_prediction = ifelse(ensemble_prediction > 4.75, 4.75, ensemble_prediction),
         ensemble_prediction = ifelse(ensemble_prediction < 0.75, 0.75, ensemble_prediction))

ensemble_rsme <- RMSE(ens$rating.x, ens$ensemble_prediction)

update_results_table(results = ensemble_rsme,
                     method = "PCA Ensemble Prediction",
                     type = "Validation")

cleanup(keep = "validation")


## ----movies_mf_validation, echo=TRUE---------------------------------------------
#recosystem movies final validation 
set.seed(831, sample.kind = "Rounding")

# Use the entire edx data set to train the model
# Convert 'edx' and 'validation' sets to recosystem input format
edx <- read_rds("edx.rda")
edx_reco <-  with(edx, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating = rating))

#convert validation to recosystem input format
validation_reco  <-  with(validation, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))

# Create the model object
r <-  Reco()
cores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

# read in the tuning parameters from the test run
opts <- read_rds("recosys_opts_movies.rda")

#train the model
r$train(edx_reco, opts = c(opts$min, 
                           nthread = cores, 
                           niter = 50,
                           verbose = FALSE))

#create the predictors
pred <-  r$predict(validation_reco, out_memory())

update_results_table(results = RMSE(validation$rating, pred),
                     method = "Matrix Factorization",
                     type = "Validation")


## ----genres_mf_validation, echo=TRUE---------------------------------------------
#recosystem genres final validation 

set.seed(831, sample.kind = "Rounding")

# Use the entire edx data set to train the model
# Convert genres to a factor for use in this model
# Convert 'edx' and 'validation' sets to recosystem input format
edx <- read_rds("edx.rda") %>%
  mutate(genres = as.factor(genres))
edx_reco <-  with(edx, data_memory(user_index = userId, 
                                   item_index = genres, 
                                   rating = rating))
# Convert genres to a factor for use in this model
# Make sure factor levels in the validation data frame match with the edx df
# First we will create a data frame that can be used as a cross reference
# to the edx genres levels
genres_xref <- edx %>%
  mutate(genres_level = as.numeric(genres)) %>%
  group_by(genres_level) %>%
  summarize(genres = first(genres),
            genres_level = first(genres_level))
# Then we will join this to the validation data frame
validation <- validation %>%
  left_join(genres_xref, by="genres")
# Create the validation recosystem object using the genres_level column
validation_reco  <-  with(validation, data_memory(user_index = userId, 
                                                  item_index = genres_level, 
                                                  rating = rating))

# Create the model object
r <-  Reco()
cores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

# read in the tuning parameters from the test run
opts <- read_rds("recosys_opts_genres.rda")

#train the model
r$train(edx_reco, opts = c(opts$min, 
                           nthread = cores, 
                           niter = 50,
                           verbose = FALSE))

#create the predictors
pred2 <-  r$predict(validation_reco, out_memory())

update_results_table(results = RMSE(validation$rating, pred2),
                     method = "Matrix Factorization Genres",
                     type = "Validation")



## ----mf_ensemble_validation, echo=TRUE-------------------------------------------
#Create the Matrix Factorization Ensemble
ensemble_mf <- tibble(movies = pred, genres = pred2)

ens <- ensemble_mf %>% 
  mutate(prediction = (movies * 0.94) + (genres * 0.06)) %>%
  mutate(prediction = ifelse(prediction > 4.75, 4.75, prediction),
         prediction = ifelse(prediction < 0.75, 0.75, prediction))

ensemble_rsme <- RMSE(validation$rating, ens$prediction)

update_results_table(ensemble_rsme,
                     method = "Matrix Factorization Ensemble",
                     type = "Validation")
display_results_table("Final Results")
  


