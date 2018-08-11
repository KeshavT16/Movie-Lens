####################################
#### libraries used
####################################

library(data.table) ## for importing data
library(tidyverse) ## for data wrangling (for dplyr and ggplot2)
library(stringr) ## for string manipulation
library(plotly) ## for interactive visualization

## setting seed for consistency
set.seed(123)
rm(list=ls())

####################################
#### variables used
####################################

package_list <-
  c(
    'data.table',
    'tidyverse',
    'stringr',
    'plotly'
  )

####################################
#### functions defined
####################################

## checks for whether a package is intsalled or not and loads it thereafter
for (package in package_list) {
  if (!require(package, character.only = T, quietly = T)) {
    install.packages(package, repos = "http://cran.us.r-project.org")
    library(package, character.only = T)
  }
}

####################################
#### data cleaning and preparation
####################################

## importing data 

links <- fread("links.csv")
movies <- fread("movies.csv")
ratings <- fread("ratings.csv")
tags <- fread("tags.csv")

## taking a glimpse at the data structures
str(links)
str(movies)
str(ratings)
str(tags)

############### data cleaning #################

## few values where there are multiple years
movies[unlist(regexpr("\\([0-9]{4}\\-",movies$title)) != -1,"title"]

## convert genres into rows
movies <- movies %>%
          mutate(genre = strsplit(genres,"\\|")
                 ,movie_title =  str_trim(substr(title, start = 1 , stop= unlist(regexpr("\\(([0-9\\-]*)\\)$",title)) -1))
                 ,movie_year =  as.numeric(substr(title, start = unlist(regexpr("\\(([0-9\\-]*)\\)$",title))+1 , stop= unlist(regexpr("\\(([0-9\\-]*)\\)$",title))+4))) %>%
          unnest(genre) %>%
          select(movieId,title,genre,movie_title,movie_year,genres)
str(movies)

## checking the values for NAS coerced
movies[is.na(as.numeric(substr(movies$title, start = unlist(regexpr("\\(([0-9\\-]*)\\)$",movies$title))+1 , stop= unlist(regexpr("\\(([0-9\\-]*)\\)$",movies$title))+4))),"title"]
## [1] "Hyena Road"                
##"The Lovers and the Despot" 
##"Stranger Things"           
##"Women of '69, Unboxed" 


## verifying the transform
filter(movies,movieId ==1)

## extracting date from timestamp for ratings and tags
ratings <- ratings %>%
          mutate(date = as.Date(as.POSIXct(timestamp,origin="1970-01-01 00:00:00",tz = "GMT"))
                 ,year = as.numeric(substr(date,1,4))) %>%
          select(-timestamp)

tags <- tags %>%
  mutate(date = as.Date(as.POSIXct(timestamp,origin="1970-01-01 00:00:00",tz = "GMT"))
         ,year = as.numeric(substr(date,1,4))) %>%
  select(-timestamp)



## summary of all the datasets

summary(movies) ## few NA's in movie_year
summary(links) ## few NA's in tmdbId
summary(tags)
summary(ratings)

####################################################
################### answers ########################
####################################################


##1.	Average rating for each movie released in or after 1996


movies %>%
  filter(movie_year > 1995) %>%
  distinct(movieId,movie_title) %>%
  merge(ratings, by="movieId", all.x = TRUE) %>%
  select(movieId,movie_title,rating) %>%
  group_by(movieId,movie_title) %>%
  summarize(avg_ratings =mean(rating))

##2.	Top 5 most reviewed movies every year after 1994


movies %>%
  distinct(movieId,movie_title) %>%
  merge(ratings, by="movieId") %>%
  select(movieId,movie_title,year) %>%
  filter(year>1994) %>%
  group_by(movieId,movie_title,year) %>%
  summarize(no_reviews =n()) %>%
  group_by(year) %>%
  mutate(rn =row_number(desc(no_reviews))) %>%
  filter(rn<6) %>%
  arrange(year,desc(no_reviews)) 


##3.	Average rating for "Drama", "Romance" and "Drama and Romance" movies

a <- ratings%>%
  group_by(movieId) %>%
  summarize(avg_rating = mean(rating)) %>%
  merge( distinct(movies,movieId,movie_title,genres),by="movieId", all.y = TRUE) %>%
  select(movieId,avg_rating,movie_title,genres) %>%
  mutate(horror = ifelse(grepl("^(.*)(Horror)(.*)$",genres),1,0),
         drama = ifelse(grepl("^(.*)(Drama)(.*)$",genres),1,0),
         horror_drama = ifelse(grepl("^(.*)(Horror)(.*)$",genres) & grepl("^(.*)(Drama)(.*)$",genres),1,0))

 
## horror
  mean(filter(a,horror == 1)$avg_rating, na.rm = TRUE)

## drama
  mean(filter(a,drama == 1)$avg_rating, na.rm = TRUE)

## horror and drama
  mean(filter(a,horror_drama == 1)$avg_rating, na.rm = TRUE)
 
##4.	Number of customers who rated a movie tagged as "horror" by year

  tags %>%
    filter(regexpr("[hH][oO][rR][rR][oO][rR]",tag) != -1) %>%
    distinct(movieId) %>% 
    merge(ratings, by="movieId", all.x = TRUE) %>%
    group_by(movieId,year) %>%
    summarize(no_users = n_distinct(userId)) %>%
    merge( distinct(movies,movieId,movie_title),by="movieId",all.x = TRUE) %>%
    select(movieId,no_users,movie_title,year) 
    
############################################
############### Data Plotting ##############
############################################
  
  
##1.	Trend of movie genres by the release years i.e. frequency of different genres of movies 
## released each year. If a movie is across multiple genres then count them in all
  
a <- movies %>%
    filter(genre != "(no genres listed)") %>%
    group_by(genre,movie_year) %>%
    summarize(count_movies= n()) %>%
    ggplot(aes(x=movie_year,y=count_movies,color=genre, text = genre))+
    geom_line()

## to make it interactive used plotly    
ggplotly(a)  

## An increasing popularity of genres like Drama, Comedy, Action, Romance and Thriller was observed with time  
## till 2000 but then again a decline was observed
  
##2.	Top 5 most reviewed movies every year after 1994 - would like to see all the years 
##  plotted at one go
  
a <- movies %>%
  distinct(movieId,movie_title) %>%
  merge(ratings, by="movieId") %>%
  select(movieId,movie_title,year) %>%
  filter(year>1994) %>%
  group_by(movieId,movie_title,year) %>%
  summarize(no_reviews =n()) %>%
  group_by(year) %>%
  mutate(rn =row_number(desc(no_reviews))) %>%
  filter(rn<6) %>%
  arrange(year,desc(no_reviews)) 

  
p <- ggplot(a, aes(x = year, y = no_reviews, text = paste("Movie:", movie_title))) +
  geom_point() 

ggplotly(p)