# Loading the libraries
library(dplyr)
library(readr)
library(readxl)
library(stringr)
#library(lubridate) does not work on the notebook

# Importing the data
price <- read_csv("datasets/airbnb_price.csv")
room_type <- read_excel("datasets/airbnb_room_type.xlsx")
last_review <- read_tsv("datasets/airbnb_last_review.tsv")


# Cleaning the data
price_tidy <- price %>%
    mutate(price_numeric = as.numeric(str_remove(price," dollars"))) # Getting the price

room_tipe_tidy <- room_type %>%
    mutate(room_type = str_to_title(room_type), # Unifying format
           room_type_fct = as.factor(room_type)) # Character to factor

last_review_tidy <- last_review %>%
    mutate(last_review_date = as.Date(last_review, format = "%B %d %Y")) %>% # Getting the dates
    arrange(last_review_date)


# QUESTIONS
# What is the average listing price?
avg_price <-price_tidy %>%
    summarize(mean(price_numeric))
avg_price <- as.numeric(avg_price) # Tible to numeric

print(paste("1. The average listing price is", round(avg_price,1),"dollars."))

# How many of the listings are private rooms?
nb_private_rooms <- room_tipe_tidy %>%
    select(room_type_fct) %>%
    filter(room_type_fct == "Private Room")  %>%
    count()
nb_private_rooms <- as.numeric(nb_private_rooms) # Tible to numeric

print(paste("2. The are", nb_private_rooms,"private rooms."))

# When were the earliest and most recent reviews in the dataset?
review_dates <- last_review_tidy %>%
    summarize(first_reviewed = min(last_review_date),
             last_reviewed = max(last_review_date))

print(paste("3. The earliest review was on", review_dates$first_reviewed,"and the most recent review was on", review_dates$last_reviewed,"."))
