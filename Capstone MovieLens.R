#edX Course PH125.9x - Capstone Project: MovieLens
#Marcos de Oliveira
#May 18th, 2019

#Load libraries and files
library(tidyverse)
library(caret)
library(dplyr)
library(lubridate)
library(ggplot2)

setwd("~/Marcos/Data Science/Cursos/Harvard Data Science Prof Cert/PH125.9x Capstone")
validation<-readRDS("validation.rds")
ratings<-readRDS("edx.rds")

# Explore data (answering Quiz questions)

#Q1 How many rows and columns are there in the edx dataset?
nrow(ratings)
ncol(ratings)

#Q2 How many zeros were given as ratings in the edx dataset?
length(which(ratings$rating==0))

#How many threes were given as ratings in the edx dataset?
length(which(ratings$rating==3))

# Q3 How many different movies are in the edx dataset?
ratings %>%
  summarise(n_distinct(movieId))

# Q4 How many different users are in the edx dataset?
ratings%>%
  summarise(n_distinct(userId))

#Q5 How many movie ratings are in each of the following genres in the edx dataset?
# Drama, Comedy, Thriller, Romance
Genres<-ratings%>%select(genres)
s<-split(Genres, sample(1:5, nrow(Genres), replace=T))
sep1 <- separate_rows(data = s[[1]], genres, sep = "\\|")
sep2 <- separate_rows(data = s[[2]], genres, sep = "\\|")
sep3 <- separate_rows(data = s[[3]], genres, sep = "\\|")
sep4 <- separate_rows(data = s[[4]], genres, sep = "\\|")
sep5 <- separate_rows(data = s[[5]], genres, sep = "\\|")
j<-do.call("rbind",list(sep1,sep2,sep3,sep4,sep5))
length(which(j$genres=="Drama"))
length(which(j$genres=="Comedy"))
length(which(j$genres=="Thriller"))
length(which(j$genres=="Romance"))

#Q6 Which movie has the greatest number of ratings?
ratings %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  top_n(1,count)

#Q7 What are the five most given ratings in order from most to least?
ratings %>%
  group_by(rating) %>%
  summarize(count=n()) %>%
  top_n(5,count)%>%
  arrange(desc(count))

#Q8 True or False: In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.)
ratings <- ratings %>%
  mutate(Integer=ifelse(round(rating) == rating,TRUE,FALSE))
summary(ratings$Integer)

#Further data exploitation
#Distribution of ratings

#Plot of count of each rating
ratings %>%
  group_by(rating) %>%
  summarize(count=n()) %>%
  ggplot(aes(x=rating,y=count))+
  geom_bar(fill="tomato3",stat="identity")+
  scale_x_continuous(breaks = pretty(ratings$rating, n = 10)) +
  scale_y_continuous(breaks = seq(0, 3000000, by = 500000),labels = scales::comma)+
  labs(title="Distribution of ratings", 
       y="Number of ratings",
       x="Rating")

#Rating along months
#Converting timestamp to posix and extracting day
ratings$date<-as_date(as_datetime(ratings$timestamp))

#Extracting month and year
ratings$monthyear<-format(ratings$date,"%Y-%m")

#Summarizing
monthly_summary <- ratings %>% group_by(monthyear) %>%
  summarize(count=n(),avgrating=mean(rating))
nrows<-nrow(monthly_summary)
maxcount<-max(monthly_summary$count)

maxmonthyear<-ratings$monthyear[which.max(monthly_summary$count)]
maxmonthavg<-monthly_summary$avgrating[which(monthly_summary$monthyear==maxmonthyear)]

#Finding out which were top 5 movies most rated in month with most ratings
maxmonth<-monthly_summary[which.max(monthly_summary$count),]$monthyear
ratings_maxmonth=ratings[which(ratings$monthyear==maxmonth),]

#Number of movies rated in month with max ratings
ratings_maxmonth %>%
  summarize(n_distinct(movieId))

#Top 5 movies in number of ratings in month with max count of ratings
ratings_maxmonth %>%
  group_by(title)%>%
  summarize(count=n())%>%
  top_n(5,count)%>%
  arrange(desc(count))


#Plotting Ratings count and average rating per month
#We will divide in two parts otherwise it will become cluttered
ScaleFactor<-50000
ggplot(monthly_summary[1:round(nrows/2),],aes(x=monthyear))+
  geom_point(aes(y=count, colour="red"))+
  geom_point(aes(y=avgrating*ScaleFactor, colour="black"))+
  labs(title="Monthly Evaluations (First half)", 
       y="Evaluations",
       x="Month") +  # title and caption
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank()) +  # turn off minor grid
  scale_y_continuous(name="Ratings Count", 
                     sec.axis=sec_axis(~./ScaleFactor, name="Avg. Rating")) +
  theme(
    legend.position = "none",
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red")
  )

ggplot(monthly_summary[round(nrows/2)+1:nrows,],aes(x=monthyear))+
  geom_point(aes(y=count, colour="red"))+
  geom_point(aes(y=avgrating*ScaleFactor, colour="black"))+
  labs(title="Monthly Evaluations (Second half)", 
       y="Evaluations",
       x="Month") +  # title and caption
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank()) +  # turn off minor grid
  scale_y_continuous(name="Ratings count", 
                     sec.axis=sec_axis(~./ScaleFactor, name="Avg.Rating")) +
  theme(
    legend.position = "none",
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red")
  )


#Users who rate integers
#Overall Integer and non-Integer ratings
ratings %>%
  group_by(Integer) %>%
  summarize(count=n())
#Use all() function to check if all values in previously defined Integer column are TRUE per user
user_integer <- ratings%>%group_by(userId) %>%
  summarize(Integerrate=all(Integer))
summary(user_integer[,2])

#First model - Simple overall rating means

#Create RMSE generic function
RMSE = function(true, predicted){
  sqrt(mean((true - predicted)^2))
}

#Assign train and test sets (more mnemonic to supervised learning)
train<-ratings
test<-validation

#Create Model 1
#Simple means as prediction
means<-mean(train$rating)
error<-RMSE(test$rating,means)
#Store results in a dataframe for comparison
rmse_results<-data_frame(method="Average",RMSE=error)
rmse_results


#Second Model - Adding _Movie bias_ 

#Group by movie_id and calculate movie bias
movie_avg<-train %>%
  group_by(movieId) %>%
  summarize(movie_bias=mean(rating-means))

#add movie_bias to test dataset
predicted<-means+test %>%
  left_join(movie_avg,by='movieId')%>%
  pull(movie_bias)

error<-RMSE(test$rating,predicted)  
rmse_results<-bind_rows(rmse_results,data_frame(method="MovieBias",RMSE=error))
rmse_results


#Third Model - Adding _User bias_
user_avg<-train %>%
  left_join(movie_avg,by='movieId') %>%
  group_by(userId) %>%
  summarize(user_bias=mean(rating-means-movie_bias))

#Add movie and user biases to test dataset
predicted<-test %>%
  left_join(movie_avg,by='movieId')%>%
  left_join(user_avg,by='userId')%>%
  mutate(pred=means+movie_bias+user_bias)%>%
  .$pred

error<-RMSE(test$rating,predicted)  
rmse_results<-bind_rows(rmse_results,data_frame(method="MovieandUserBias",RMSE=error))
rmse_results

# As can be seen, we achieved our target RMSE

#Calculating Model Accuracy

#Rounded to .5
predictedrounded<-round(predicted/0.5)*0.5
mean(predictedrounded==test$rating)

#Rounded to nearest integer as most of ratings are integers
predictedrounded<-round(predicted)
mean(predictedrounded==test$rating)
                                        