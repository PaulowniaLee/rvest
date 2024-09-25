#### Environment ####
library(tidyverse)
library(httr)
library(jsonlite)
library(pbapply)
library(usethis)

setwd("/Users/mac/Documents/R_Notes/rvest")
getwd()

#####

#### API case ####

# in this example, API returns json files (which is usual)
# some APIs may use different formats 
# same method can be applied to dynamic websites 
# where contents are communicated with json (as long as we get the link)



# 1. how many breweries are located in Durham, NC?
base_url <- "https://api.openbrewerydb.org/v1/breweries" # base url of the site
query <- "?by_state=north+carolina&by_city=durham" # depending on what you need
url <- c(base_url, query)
# "+" sign here means space, part of url encoding scheme 

# json直接读，不需要httr里的function
read_json(paste0(url, collapse = "")) %>%
  length()
# 9 breweries 


# Notes on url and queries 
{
# url breakup
# e.g. 
# http://www.domain.com:1234/path/to/resource?a=b&x=y
# protocal-     host  -port-   resource_path  -query
# remark
# 1. also for https
# 2. default port is 80 for http and 443 for https, typically not displayed
# 3. resource path: local path to the resource on the server

# query strings 
# named parameters and values that modify the behaviour of the resulting page
# format:
# field1=value1&field2=value2&field3=value3
# e.g. 
# https://app.ticketmaster.com/discovery/v2/events.json? (其他部分)
# attractionId=K8vZ917Gku7&countryCode=CA&apikey=RpD2faqwk2uio290 (query)

# url encoding 
# ususally handled automatically, omitted 
# (refer to the lecture slide)
  }  
# 每个API有自己的规则
# sequence of query parameters generally does not matter, 
# unless specifically asked by API documentation 

# json -jsonlite package 
# read_json() 
# read json as a list 
# fromJSON()
# read json and try to simplify it to a data frame 

#####

#### Ex Open Brewery API ####

# 2. Which city in North Carolina has the most micro breweries? 
# How many micro breweries do they have?

# 1) we don't know how many pages are there: check metadata first
query <- "/meta?by_state=north+carolina&by_type=micro" 
url <- c(base_url, query)
nc_meta <- read_json(paste0(url, collapse = ""))
# in total there are 180 micro breweries in NC, across all cities 

# 2) get these data 
query <- "?by_state=north+carolina&by_type=micro&per_page=200"
# 180个，一页200就可以覆盖完
url <- c(base_url, query)
nc <- read_json(paste0(url, collapse = ""))
# read_json always work
# when data is in good shape, can use fromJSON to get it in data frame directly 
nc_tidy <- fromJSON(paste0(url, collapse = ""))

nc_tidy %>%
  group_by(city) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(3)
# Raleigh in NC has most micro brewery, of 17



# 3. In what cities are Founders, Yuengling, and Boulevard brewed?
# p.s. these are three names (parts) for brewery 

# get meta first 
names <- c("founders", "yuengling", "boulevard") 
# 此API不区分大小写
city_brew_meta <- function(names){
  query <- "/meta?by_name="
  url <- c(base_url, query, names)
  read_json(paste0(url, collapse = "")) %>%
    .[1]
}
brewer_meta <- sapply(names, city_brew_meta)
# there are 3 founders, 3 yuengling, 1 boulevard 

# so we can get them directly 
city_brew <- function(names){
  query <- "?by_name="
  url <- c(base_url, query, names)
  fromJSON(paste0(url, collapse = "")) %>%
    pull(city) %>%
    unique()
}
sapply(names, city_brew) %>%
  set_names(names)



# 4. Which state has the most breweries?
# get meta 
query <- "/meta?by_country=united+states" 
url <- c(base_url, query)
state_meta <- read_json(paste0(url, collapse = ""))
# there are 7999 entries for united states 

# maximum each page is 200. 40 pages needed
# some kind of loop is needed to get them all 
n <- seq(1:50)
state_brew <- function(n){
  query <- "?by_country=united+states&per_page=200&page=" 
  url <- c(base_url, query, n)
  fromJSON(paste0(url, collapse = ""))
}
state <- sapply(n, state_brew) %>%
  do.call("rbind", .)

# get a sample of (large) state dataset, faster than directly viewing it
colnames(state)
head(state, 1)

state %>%
  group_by(state_province) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(3)
# California has most brewers, of 912

#####
  
#### Extra: add progress bar for *apply ####
# library(pbapply)
# add a progress time and estimated time needed for *apply functions
# to use, simply add "pbapply::pb" before base *apply functions
# e.g. 
# pbapply::pbsapply()

# a refinement of brewer example, with progress bar 
base_url <- "https://api.openbrewerydb.org/v1/breweries"
n <- seq(1:50)
state_brew <- function(n){
  query <- "?by_country=united+states&per_page=200&page=" 
  url <- c(base_url, query, n)
  fromJSON(paste0(url, collapse = ""))
}

state <- pbapply::pbsapply(n, state_brew) %>%
  do.call("rbind", .)

#####

#### Case: NYT best seller books ####

# Store API keys
# library(usethis)
# usethis::edit_r_environ()
# setting: nyt_book_api = (API Key value)
# 后面的格式: 直接“”就好
Sys.getenv("nyt_book_api")
# 这种方法可以公开分享code而且隐藏部分内容



# get api key from environment 
key <- Sys.getenv("nyt_book_api")
# first get list names 
namelist_response <- modify_url(
  url = "https://api.nytimes.com/",
  path = "/svc/books/v3/lists/names.json",
  query = list(`api-key` = key)
) %>%
  GET() %>%
  content(as = "text") %>%
  fromJSON() 
namelist <- namelist_response$results
remove(namelist_response)



# access current bestseller book list for graphic books and manga
manga_response <- modify_url(
  url = "https://api.nytimes.com/",
  path = "/svc/books/v3/lists.json",
  query = list(
    list = "graphic-books-and-manga",
    `api-key` = key)
) %>%
  GET() %>%
  content(as = "text") %>%
  fromJSON()
manga_list <- manga_response$results %>%
  select(rank, book_details, reviews) 
# 处理结果大概也很有趣，数据结构有点复杂




# access book review for Michelle Obama 
obama_response <- modify_url(
  url = "https://api.nytimes.com/",
  path = "svc/books/v3/reviews.json",
  query = list(author = "Michelle Obama",
               `api-key` = key)) %>%
  GET() %>%
  content(as = "text") %>%
  fromJSON()
obama_list <- obama_response$results
obama_list$summary

#####













