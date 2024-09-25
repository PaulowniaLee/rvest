#### Environment ####
library(tidyverse)
library(rvest)
library(ggplot2)

#####


#### Collect ####

# create data frame 
url <- paste0(
  "http://www.sxfsw.com/index-htm-caid-3/ccid34-3043/ccid1-207/",
  "ccid2-5001/cook_id-3/ccid6-187.html")
h <- read_html(url)
temp_list <- list()

for (i in (1:3)){
  house_price <- {
    h %>%
      html_elements(".esfylist-wb") %>%
      map_dfr(~ tibble(
        # title section 
        title = .x %>%
          html_element(".info em") %>%
          html_text() %>%
          str_replace("[\u4e00-\u9fa5]{2}$", "") %>%
          str_replace("\\d+[\u4e00-\u9fa5]+\\s+$", "") %>%
          str_replace("\\r\\n\\s+$", "") %>%
          str_replace("\\s+", ""),
        # 户型 secction 
        type = .x %>%
          html_element(".info2 b") %>%
          html_text(), 
        # area section 
        area = .x %>%
          html_element(".info2") %>%
          html_text() %>%
          str_trim() %>%
          str_replace(".{6}\\r\\n\\s+", "") %>%
          str_replace("\\D{2}$", "") %>%
          as.numeric(),
        # total price 
        total = .x %>%
          html_element(".zj") %>%
          html_text() %>%
          as.numeric(),
        # per price 
        per = .x %>%
          html_element("em+ em i") %>%
          html_text() %>%
          as.numeric()
      ))
  }
  temp_list[[i]] <- house_price
  url <- {
    html_element(h, ".p_curpage+ .p_num") %>%
    html_attr("href")
  }
  h <- read_html(url)
}
remove(h, i, url, house_price)

house_data <- rbind(temp_list[[1]], temp_list[[2]], temp_list[[3]])
remove(temp_list)

write_csv(
  house_data,
  file = "House_Data_新城路.csv"
)
#####

#### Visulisation ####

