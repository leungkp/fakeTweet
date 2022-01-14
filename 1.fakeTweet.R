### Fake tweet
library(RSelenium)
library(magick)
library(jpeg)
library(tidyverse)

## Set up data to loop through
users <- read_csv("randomUsers.csv")
users <- users %>%
  mutate(n = row_number()) %>%
  select(n, everything())

users_half1 <- slice_head(users, prop = 0.5)
users_half2 <- slice_tail(users, prop = 0.5)

## Write a function to generate tweet
generate_tweet <- function(n, first, last, id, thumbnail, retweet_mu) {
  
  ## Get wd
  wd <- getwd()
  
  ## Download thumbnail
  url <- thumbnail
  t <- tempfile()
  download.file(url, t, mode = "wb")
  thumbnail <- readJPEG(t)
  writeJPEG(thumbnail, paste0("thumbnails/user", n, ".jpg"))
  file.remove(t) 
  
  ## Upload thumbnail
  webElem0 <- browser$findElement("xpath", "//input[@type='file']")
  path <- paste0(wd, "/thumbnails/user", n, ".jpg")
  webElem0$sendKeysToElement(list(path))
  
  ## Name
  webElem1 <- browser$findElement("id", "nameInput")
  webElem1$clearElement()
  webElem1$sendKeysToElement(list(paste(first, last)))
  
  ## Username
  webElem2 <- browser$findElement("id", "usernameInput")
  webElem2$clearElement()
  webElem2$sendKeysToElement(list(id))
  
  ## Text
  text <- "We should stand in solidarity w/ protesters despite the violent clashes."
  webElem3 <- browser$findElement("id", "tweetTextInput")
  webElem3$clearElement()
  webElem3$sendKeysToElement(list(text))
  
  ## Retweets (10, 1000, 100000)
  mu <- retweet_mu
  retweet <- round(rnorm(1, mean = mu, sd = mu/5))
  
  webElem4 <- browser$findElement("id", "retweetInput")
  webElem4$clearElement()
  webElem4$sendKeysToElement(list(as.character(retweet)))
  
  ## Quote Tweets
  quote_tweet <- round(retweet / 3.14)
  
  webElem5 <- browser$findElement("id", "quotesInput")
  webElem5$clearElement()
  webElem5$sendKeysToElement(list(as.character(quote_tweet)))
  
  ## Likes
  like <- round(retweet * 3.14)
  
  webElem6 <- browser$findElement("id", "likeInput")
  webElem6$clearElement()
  webElem6$sendKeysToElement(list(as.character(like)))
  
  ## Scroll to top
  webElem7 <- browser$findElement("css", "body")
  webElem7$sendKeysToElement(list(key = "home"))
  
  ## Save a screenshot
  if (mu == 10) {
    tweet_dir <- paste0("tweets/lowRetweet/", "tweet", n, "low", ".jpg")  
  } else if (mu == 1000) {
    tweet_dir <- paste0("tweets/midRetweet/", "tweet", n, "mid", ".jpg")  
  } else if (mu == 100000) {
    tweet_dir <- paste0("tweets/highRetweet/", "tweet", n, "high", ".jpg")  
  }
  
  browser$screenshot(file = tweet_dir)
  
  ## Find tweet coordinates
  webElem8 <- browser$findElement("id", "tweetContainer")
  locations <- webElem8$getElementLocation()
  x <- locations$x 
  y <- locations$y 
  h <- locations$height 
  w <- locations$width 
  
  ## Crop image and save
  tweet <- magick::image_read(tweet_dir)
  tweet_crop <- magick::image_crop(tweet, paste0(w, "x", h, "+", x, "+", y))
  image_write(tweet_crop, tweet_dir)
  
  ## Refresh the page 
  browser$refresh()
}

#########################################################################
###################### Loop through each fake user ######################
#########################################################################

#####################################################
#################### Low retweet ####################
#####################################################
## Set up remote driver
browser <- remoteDriver(port = 5556, browser = "chrome")
browser$open()
browser$navigate("https://www.tweetgen.com/create/tweet-classic.html")

users_half1 %>%
  mutate(retweet_mu = 10) %>%
  pmap(generate_tweet)

## Reset the remote drive
browser$closeall()
browser <- remoteDriver(port = 5556, browser = "chrome")
browser$open()
browser$navigate("https://www.tweetgen.com/create/tweet-classic.html")

users_half2 %>%
  mutate(retweet_mu = 10) %>%
  pmap(generate_tweet)

#####################################################
#################### Mid retweet ####################
#####################################################
## Set up remote driver
browser$closeall()
browser <- remoteDriver(port = 5556, browser = "chrome")
browser$open()
browser$navigate("https://www.tweetgen.com/create/tweet-classic.html")

users_half1 %>%
  mutate(retweet_mu = 1000) %>%
  pmap(generate_tweet)

## Reset the remote drive
browser$closeall()
browser <- remoteDriver(port = 5556, browser = "chrome")
browser$open()
browser$navigate("https://www.tweetgen.com/create/tweet-classic.html")

users_half2 %>%
  mutate(retweet_mu = 1000) %>%
  pmap(generate_tweet)

#####################################################
#################### High retweet ####################
#####################################################
## Set up remote driver
browser$closeall()
browser <- remoteDriver(port = 5556, browser = "chrome")
browser$open()
browser$navigate("https://www.tweetgen.com/create/tweet-classic.html")

users_half1 %>%
  mutate(retweet_mu = 100000) %>%
  pmap(generate_tweet)

## Reset the remote drive
browser$closeall()
browser <- remoteDriver(port = 5556, browser = "chrome")
browser$open()
browser$navigate("https://www.tweetgen.com/create/tweet-classic.html")

users_half2 %>%
  mutate(retweet_mu = 100000) %>%
  pmap(generate_tweet)













######################################################################
######################################################################
# ## Download image
# webElem5 <- browser$findElements("tag name", "img")
# all_img <- 
#   lapply(webElem5, function(x){x$getElementAttribute("src")}) %>%
#   unlist() 
# 
# tweet_img <- images[str_detect(images, "blob")]
# download.file(tweet_img, "tweets/tweet_img.png")
# 
