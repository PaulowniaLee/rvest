#### Environment ####
library(rvest)
library(tidyverse)
library(httr)

setwd("/Users/mac/Documents/R_Notes/rvest")

#####

#### No Loop ####
url <- paste0("https://sothebysrealty.ca/include/",
              "ajax_generate_url.php?token=3dc1205520a7b4c2bce3f936ad86e30ad4923859&agent_slug",
              "=&aid=0&team_id=0&view=grid&sort=1&p_bed=0&p_bath=0&p_mls=1&p_types%5B%5D=1&",
              "p_regions%5B%5D=3&p_minprice=0&p_maxprice=1000000&p_status%5B%5D=1&",
              "bound_east=&bound_west=&bound_north=&bound_south=&per_page=32&",
              "page=1", # key 
              "&start=&per_page_init=32&office=0&search_uri=%2Fen%",
              "2Fsearch-results%",
              "2Fregion-greater-vancouver-province-british-columbia-real-estate",
              "%2F&location_update=0&developement=0&_=1724827704806")

cookie <- "_ga_4KVFKZ44MY=GS1.1.1724827704.3.0.1724827704.60.0.0; PHPSESSID=ek90s8notitg3unnj9cbiuck5v; _gcl_au=1.1.511000992.1724407155; __hssrc=1; __hstc=20458343.097cc17094c753d217b249cccc1ab048.1724407155341.1724816195859.1724819711168.3; hubspotutk=097cc17094c753d217b249cccc1ab048; _fbp=fb.1.1724407156487.49864433889020503; _ga=GA1.1.1636125321.1724407156; _gid=GA1.2.2107320218.1724816192; _yosid=e04dc396-ddbf-431a-8cb2-82478a914712; _yoid=c31e5b6c-c63a-42e0-8a46-c17cf14145af; didomi_token=eyJ1c2VyX2lkIjoiMTkxN2VhYWItMmFjMS02ZDczLWFlMjgtZGYxNDBmZGM3NTkwIiwiY3JlYXRlZCI6IjIwMjQtMDgtMjNUMDk6NTY6NDEuNzcyWiIsInVwZGF0ZWQiOiIyMDI0LTA4LTIzVDA5OjU5OjE0LjYyOFoiLCJ2ZXJzaW9uIjoyLCJwdXJwb3NlcyI6eyJlbmFibGVkIjpbImdlb2xvY2F0aW9uX2RhdGEiLCJkZXZpY2VfY2hhcmFjdGVyaXN0aWNzIl19LCJ2ZW5kb3JzIjp7ImVuYWJsZWQiOlsiZ29vZ2xlIiwiYzpodWJzcG90IiwiYzpob3RqYXIiLCJjOmJyaWdodGNvdmUtcGxheWVyIiwiYzpsaW5rZWRpbiIsImM6Z29vZ2xlYW5hLTRUWG5KaWdSIiwiYzpuZWNlc3NhcnktWkRKVHdjZVkiLCJjOmxvY2FsbG9naS1IWUtqcUI0UiIsImM6bGlzdHRyYWMtZ0RwSG5NVjciXX0sInZlbmRvcnNfbGkiOnsiZW5hYmxlZCI6WyJnb29nbGUiXX0sImFjIjoiQUZtQUNBRmsuQUZtQUNBRmsifQ==; euconsent-v2=CQDzioAQDzioAAHABBENBCFsAP_gAEPgAAQAJ2wCwAGAAZACOAKMBGoC8wICgQIAmiBNgCdoJ2wBwAjgCjAGWAvMCbAE6wJ2gAAA.f_wACHwAAAAA; ll-visitor-id=6aef91b5-cf64-4626-90a2-c681aae587c0"

response <- url %>%
  GET(add_headers(.headers = c(cookie))) %>%
  content(type = "text", encoding = "UTF-8")

urls <- response %>%
  read_html() %>%
  html_elements(".plink") %>%
  html_attr("href")



my_ua <- user_agent(paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                           "AppleWebKit/537.36 (KHTML, like Gecko)",
                           "Chrome/127.0.0.0 Safari/537.36"))
my_session <- session("https://www.google.com/?client=safari", my_ua)
page <- read_html(my_session)


# address 
page %>%
  html_elements("div [class='cnt normal flex-sb'] > span > p") %>%
  html_text() %>%
  .[1]

# price
page %>%
  html_elements(".price_social") %>%
  html_text() %>%
  .[1]

# square feet 
page %>%
  html_elements("[class='quick_facts'] > li") %>%
  html_text() %>%
  .[3] 

# number of bedrooms 
page %>%
  html_elements("[class='quick_facts'] > li") %>%
  html_text() %>%
  .[1] 

# number of bathrooms 
page %>%
  html_elements("[class='quick_facts'] > li") %>%
  html_text() %>%
  .[2]

# following are elements that may not exist for every page 
# we get li directly 

key_facts <- page %>%
  html_elements("[class='key_facts'] > li") %>%
  html_text()
key_facts %>%
  str_replace_all(".*: ", "") %>% #删掉name,留value
  set_names(nm = 
              str_replace_all(key_facts, ":.*", "") %>% #删掉value,留name
              str_replace_all("[0-9]+", "") %>% #删掉name里的数字
              str_trim()
            ) 
# name value 合体


#####

#### Loop Version ####

urls <- data.frame()

for (i in (1:40)){
  
  # generate ajax url 
  url_name <- c("https://sothebysrealty.ca/include/",
"ajax_generate_url.php?token=3dc1205520a7b4c2bce3f936ad86e30ad4923859&agent_slug",
"=&aid=0&team_id=0&view=grid&sort=1&p_bed=0&p_bath=0&p_mls=1&p_types%5B%5D=1&",
"p_regions%5B%5D=3&p_minprice=0&p_maxprice=1000000&p_status%5B%5D=1&",
"bound_east=&bound_west=&bound_north=&bound_south=&per_page=32&",
"page=",
i,
"&start=&per_page_init=32&office=0&search_uri=%2Fen%",
"2Fsearch-results%",
"2Fregion-greater-vancouver-province-british-columbia-real-estate",
"%2F&location_update=0&developement=0&_=1724827704806")
  url <- paste0(url_name, collapse = "")

  # get response 
  t0 <- Sys.time()
  response <- url %>%
    GET(add_headers(.headers = c(cookie))) %>%
    content(type = "text", encoding = "UTF-8")
  t1 <- Sys.time()
  delay <- as.numeric(t1 - t0)
  t <- sample(1:7, size = 1) 
  Sys.sleep(t*delay)
  
  # get links 
  links <- response %>%
    read_html() %>%
    html_elements(".plink") %>%
    html_attr("href")
  
  # rbind 
  df <- data.frame(link = links)
  urls <- rbind(urls, df)
  
  # progress 
  if (i%%10==0) {cat("*")} else {cat(".")}
}
# we cannot get ajax urls by loop: they change after every reload
# (so ajax of each page are of different patterns)
# a case where Selenium is a must 

df_all <- data.frame()

# this part is purely rvest, works anyway
houses <- function(urls){
  
  # save link (we are not using i)
  link <- urls
  
  # open 
  session_jump_to(my_session, link)
  page <- read_html(my_session)
  
  {
  # price 
  price <- page %>%
    html_elements(".price_social") %>%
    html_text() %>%
    .[1]
  
  # address 
  address <- page %>%
    html_elements("div [class='cnt normal flex-sb'] > span > p") %>%
    html_text() %>%
    .[1]
  
  # square feet 
  square_feet <- page %>%
    html_elements("[class='quick_facts'] > li") %>%
    html_text() %>%
    .[3] 
  
  # keyfacts 
  key_facts <- page %>%
    html_elements("[class='key_facts'] > li") %>%
    html_text()
  key_facts <- key_facts %>%
    str_replace_all(".*: ", "") %>% #删掉name,留value
    set_names(nm = 
                str_replace_all(key_facts, ":.*", "") %>% #删掉value,留name
                str_replace_all("[0-9]+", "") %>% #删掉name里的数字
                str_trim()
    ) 
  
  # keyfacts for each house can be different
  # we fill in NA when there is no such observation 
  {
  # property type 
  building_type <- ifelse("Property Type" %in% names(key_facts),
                          key_facts["Property Type"], NA)
  
  # year built 
  year_built <- ifelse("Year Built" %in% names(key_facts),
                       key_facts["Year Built"], NA)
  
  # number of bedrooms
  bedrooms <- ifelse("Bedrooms" %in% names(key_facts),
                     key_facts["Bedrooms"], NA)
  
  # number of bathrooms 
  bathrooms <- ifelse("Bathrooms" %in% names(key_facts),
                      key_facts["Bathrooms"], NA)
  
  # property tax 
  taxes <- ifelse("Municipal Taxes" %in% names(key_facts),
                  key_facts["Municipal Taxes"], NA)
    }
  
  }
  
  # compile 
  df_single <- data.frame(
    price = price,
    address = address,
    squares = square_feet,
    type = building_type,
    year = year_built,
    bed = bedrooms,
    bath = bathrooms,
    tax = taxes
  )
  
  # rbind 
  df_all <- rbind(df_all, df_single)
}

#####