
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

# These packages need to be loaded in the first @tests cell
library(testthat) 
library(IRkernel.testthat)

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

# There are two tests in this cell. The first one tests that the
# correct package was loaded. The second one tests that the
# correct data were read in.

# Load the necessary packages
library(readr)
library(readxl)
library(dplyr)
library(stringr)

# Import CSV for prices
airbnb_price <- read_csv('datasets/airbnb_price.csv')

# Import TSV for room types
airbnb_room_type <- read_excel('datasets/airbnb_room_type.xlsx')

# Import Excel file for review dates
airbnb_last_review <- read_tsv('datasets/airbnb_last_review.tsv')

# Join the three data frames together into one
listings <- airbnb_price %>%
  inner_join(airbnb_room_type, by = "listing_id") %>%
  inner_join(airbnb_last_review, by = "listing_id")


# Question 1: What is the average listing price? 
# To convert price to numeric, remove "dollars" from each value
soln_avg_price <- listings %>%
  mutate(price_clean = str_remove(price, " dollars") %>%
        as.numeric()) %>%
  # Take the mean of price_clean
  summarize(avg_price = mean(price_clean)) %>%
  # Convert from a tibble to a single number
  as.numeric()


# Question 2: How many of the listings are private rooms? 
# Since there are differences in capitalization, make capitalization consistent
private_room_count <- listings %>%
  mutate(room_type = str_to_lower(room_type)) %>%
  # Then count the number of each room_type
  count(room_type) %>%
  # Get row containing count for private rooms only
  filter(room_type == "private room") 

# Extract number of rooms
soln_nb_private_rooms <- private_room_count$n


# Question 3: Which listing was most recently reviewed? 
# In order to use a function like max() on the last_review column, it needs to be converted to Date
soln_review_dates <- listings %>%
  # Convert to date using the format 'Month DD YYYY'
  mutate(last_review_date = as.Date(last_review, format = "%B %d %Y")) %>%
  # Use max() and min() to take the latest and earliest dates
  summarize(first_reviewed = min(last_review_date),
            last_reviewed = max(last_review_date))


run_tests({
    test_that("avg_price is correct", {
        expect_true(avg_price >= floor(soln_avg_price) | avg_price <= ceiling(soln_avg_price), 
                     info = "avg_price contains the wrong value.")
    })
    
    
    test_that("nb_private_rooms is correct", {
        expect_equal(nb_private_rooms, nb_private_rooms, 
                     info = "nb_private_rooms contains the wrong value. Make sure you have included values regardless of capitalization.")
    })
    test_that("nb_private_rooms takes capitalization into account", {
        expect_false(avg_price == 2248 | avg_price == 7241 | avg_price == 1867, 
                     info = "There are more private rooms than what is currently contained in `nb_private_rooms`. Make sure you are considering what to do with differences in capitalization.")
    })
    
    
    test_that("review_dates has 1 row", {
        expect_true(nrow(review_dates) == 1,
                   info = "review_dates should have one row that contains the earliest/latest dates.")
    })
    test_that("review_dates has 2 cols", {
        expect_true(ncol(review_dates) == 2,
                   info = "review_dates should have two columns - `first_reviewed` and `last_reviewed`.")
    })
    test_that("first_reviewed column exists in review_dates", {
        expect_true("first_reviewed" %in% colnames(review_dates),
                    info = "review_dates should have a column called first_reviewed.")    
    })
    test_that("last_reviewed column exists in review_dates", {
        expect_true("last_reviewed" %in% colnames(review_dates),
                    info = "review_dates should have a column called last_reviewed.")    
    })
    test_that("review_dates$first_reviewed is correct", {
        expect_equal(pull(review_dates, first_reviewed),
                     pull(soln_review_dates, first_reviewed),
                     info = "The first_reviewed column of review_dates contains the wrong value. It should contain one `Date` which is the earliest last_review date in the dataset.")
    })
    test_that("review_dates$last_reviewed is correct", {
        expect_equal(pull(review_dates, last_reviewed),
                     pull(soln_review_dates, last_reviewed),
                     info = "The last_reviewed column of review_dates contains the wrong value. It should contain one `Date` which is the latest last_review date in the dataset.")
    })
})
