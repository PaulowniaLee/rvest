#### Environment ####
library(rvest)
library(tidyverse)
library(httr)
library(lubridate)
library(chron)

setwd("/Users/mac/Documents/R_Notes/rvest")

#####

#### Advanced Static Tricks ####
# 主要区别：
# 1. 加上user_agent和session，更像人
# 2. 加上delay proportional to response, 不拖垮网站
# 3. randomise the delay multiplier by sampling an interval, 更像人
# These definitely get scrapping slower, but make us less likely to be banned

url <- "http://books.toscrape.com"
my_ua <- user_agent(paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                           "AppleWebKit/537.36 (KHTML, like Gecko)",
                           "Chrome/127.0.0.0 Safari/537.36"))
# creaet a user agent, to make R behave like normal human
# up-to-date user_agent can be found in 
# https://techblog.willshouse.com/2012/01/03/most-common-user-agents/
# 实际上还可以用一个sample来轮换上面list中的user_agent
# 此外还可以使用proxy server，不过免费的proxy一般不稳定
my_session <- session(url, my_ua)
# create a session, so R can store cookies, navigate between pages, like a browser
my_session$response$status_code
# check the response: 200 means everything works well

books <- map_df(1:5, function(i) {
  
  # simple but effective progress indicator 
  cat(".")
  
  t0 <- Sys.time()
  wd_name <- c("http://books.toscrape.com/catalogue/page-", i, ".html")
  my_session <- session_jump_to(my_session, paste(wd_name, collapse = ""))
  # change to page with a different url 
  book <- read_html(my_session)
  # save a page from the session 
  t1 <- Sys.time()
  delay <- as.numeric(t1 - t0)
  t <- sample(1:7, size = 1) # get a randomized multiplier
  Sys.sleep(t*delay) # sleep t times longer than response delay
  # In this way, we slow down our requests
  
  tibble(
    title = book %>%
      html_elements(".product_pod a") %>%
      html_attr(name = "title") %>%
      na.omit(),
    
    price = book %>%
      html_elements(".price_color") %>%
      html_text( ),
    
    rating = book %>%
      html_elements(".star-rating") %>%
      html_attr(name = "class") %>%
      str_replace_all("[a-z]{4}\\-[a-z]{6}\\s", ""),
    
    available = book %>%
      html_elements(".availability") %>%
      html_text() %>%
      str_replace_all("\\s+", "")
  )
  
})

#####

#### Forms ####

# provide certain input, e.g., login credentials 
# we use form to do this 
# html_form: extract form 
# html_form_set: create our form 
# html_form_submit: submit our form 

# example: search sth on Google 
google <- read_html("http://www.google.com")
search <- html_form(google) %>%
  pluck(1) # pluck: 比[[]]更灵活的取序号方式,对nested object很有效
search %>% str() # str: compactly display the structure (alternative to summary)
# 注意
# 提取的form里，Google 搜索 name = “btnG"

search_sth <- search %>%
  html_form_set(q = "revue starlight")
resp <- html_form_submit(search_sth, submit = "btnG")
# submit: 
# NULL: use first button; string: select button by name 
# 这里是选择 btnG button,即Google 搜索
read_html(resp)



# Workflow 
# 1. extract the form 
# 2. set it (input what you need input)
# 3. start your session with the form 
# 4. submit the form 
my_ua <- user_agent(paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                           "AppleWebKit/537.36 (KHTML, like Gecko)",
                           "Chrome/127.0.0.0 Safari/537.36"))
# 不用这个agent好像就会被屏蔽

google_form <- read_html("http://www.google.com") %>%
  html_form() %>%
  pluck(1)
search_sth <- google_form %>%
  html_form_set(q = "revue starlight")

google_session <- session("http://www.google.com", my_ua) %>%
  session_submit(search_sth, submit = "btnG")

resp <- google_session %>%
  read_html()

#####

#### Automating Scraping ####

# two ways
# 1. manually create a list of urls 
# 2. automatically acquire next destination from the page 

# Approach 1
# 参见爬书网站的例子，略

# Approach 2 
# 关键：replace the link argument with the one extracted 
# while 
output_list <- vector(mode = "list", length = 10L)
# 更高明一点的空list生成方法
i <- 1
start_url <- "https://www.r-bloggers.com/"
content <- vector(mode = "list", length = 10L)
session_page <- session(start_url)
session_page$response$status_code
# 200, we are connected

while (session_page$response$status_code == 200 && i <= 10) {
  content[[i]] <- session_page |> 
    read_html()
  
  session_page <- session_page |> session_follow_link(css = ".next")
  i <- i + 1
}

#####

#### Case: scrap Indeed HK ####

# we begin by testing our connection 
url <- paste0("https://hk.indeed.com/jobs?q=Data+Scientist&l=Hong+Kong",
              "&from=searchOnHP&vjk=a4755233228dc139")
my_ua <- user_agent(paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                           "AppleWebKit/537.36 (KHTML, like Gecko)",
                           "Chrome/127.0.0.0 Safari/537.36"))
my_session <- session(url, my_ua)
my_session$response$status_code
# connected 



# before we build a loop to automate, we test how to extract each elements
{
page <- read_html(my_session)

# job titles 
page %>%
  html_elements(".css-jspxzf") %>%
  pluck(1)
#查看一个完整的串,用来看class name
page %>%
  html_elements(".css-jspxzf span") %>%
  html_attr("title")

# company names 
page %>%
  html_elements(".css-1qv0295") %>%
  html_text()

# company locations 
page %>%
  html_elements(".css-1p0sjhy") %>%
  html_text()

# job description (in subpages)
p <- page %>%
  html_elements(".css-jspxzf") %>%
  html_attr("href") %>%
  pluck(1)

sub_url <- paste0("https://hk.indeed.com", p)
sub_session <- session(sub_url, my_ua)
sub_session$response$status_code
sub_page <- read_html(sub_session)

sub_page %>% 
  html_elements(".css-16y4thd") %>%
  html_text()
}


# now we can build our loop 
url <- paste0("https://hk.indeed.com/jobs?q=Data+Scientist&l=Hong+Kong",
              "&from=searchOnHP&vjk=a4755233228dc139")
my_ua <- user_agent(paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                           "AppleWebKit/537.36 (KHTML, like Gecko)",
                           "Chrome/127.0.0.0 Safari/537.36"))
my_session <- session(url, my_ua)
my_session$response$status_code

#链接有规律性，不需要while loop
#观察到一共有10页
page_results <- c("", seq(from = 10, to = 90, by = 10))

jobs <- map_df(1:10, function(i) {
  
  # simple but effective progress indicator 
  cat(".")
  
  if (i == 1){
  wd_name <- c("https://hk.indeed.com/jobs?q=Data+Scientist&l=Hong+Kong")
  } else{
  wd_name <- c("https://hk.indeed.com/jobs?q=Data+Scientist&l=Hong+Kong",
               "&start=", page_results[i])
  }
  #第一页是特殊的，没有"&start=",用if来实现
  t0 <- Sys.time()
  my_session <- session_jump_to(my_session, paste(wd_name, collapse = ""))
  page <- read_html(my_session)
  t1 <- Sys.time()
  delay <- as.numeric(t1 - t0)
  Sys.sleep(7*delay) 
  
  count <- page %>%
    html_elements(".css-jspxzf span") %>%
    html_attr("title") %>%
    length()
  count <- seq(1, count, by = 1)
  # determine how many subpages we should look for in job description section
  
  tibble(
    job_title = page %>%
      html_elements(".css-jspxzf span") %>%
      html_attr("title"),
    
    company_name = page %>%
      html_elements(".css-1qv0295") %>%
      html_text(),
    
    company_location = page %>%
      html_elements(".css-1p0sjhy") %>%
      html_text(),
    
    job_description = sapply(count, function(count){
      link = page %>%
        html_elements(".css-jspxzf") %>%
        html_attr("href") %>%
        pluck(count)
      
      t0 <- Sys.time()
      des <- session_jump_to(my_session,
                      paste0("https://hk.indeed.com", link)) %>%
        read_html() %>%
        html_elements(".css-16y4thd") %>%
        html_text()
      t1 <- Sys.time()
      delay <- as.numeric(t1 - t0)
      Sys.sleep(2*delay)
      
      return(des)
      })
   )
  
})

write_csv(jobs, file = "Post_data_scientists_hkindeed.csv")
#####
