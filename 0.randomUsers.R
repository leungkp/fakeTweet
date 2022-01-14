#install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)
library(tidyverse)
library(stringr)
set.seed(20210705)

### Random user data
get_random_user <- function(n = 1) {
  result <- vector("list", length = n)
  
  for (i in 1:n) {
    randomUser <-  GET("https://randomuser.me/api/?nat=us")
    randomUser_data <- fromJSON(rawToChar(randomUser$content))
    
    name <- randomUser_data$results$name
    
    id <- paste0(str_sub(name$first, 1, 4),
                 str_sub(name$last, 1, 3), 
                 paste0(sample(0:9, 3, replace = TRUE), collapse = ""))
    
    thumbnail <- randomUser_data$results$picture$thumbnail
    
    result[[i]]$first <- name$first
    result[[i]]$last <- name$last
    result[[i]]$id <- id
    result[[i]]$thumbnail <- thumbnail
    
  }
  return(result)
}

randomUsers <- get_random_user(n = 1000)

## Restructure and save
randomUsers %>%
  bind_rows() %>%
  write_csv("randomUsers.csv")
