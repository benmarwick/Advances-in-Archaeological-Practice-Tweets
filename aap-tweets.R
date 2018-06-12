library(dplyr)
library(rtweet)
library(rvest)
library(stringr)
library(magrittr)
library(webshot)
library(purrr)

# Here is a function to scrape the AAP website of the TOC for the latest issue
# It grabs the article titles and DOIs, then composes a tweet with them

write_aap_tweets <- function(){
  
  url <- "https://www.cambridge.org/core/journals/advances-in-archaeological-practice/latest-issue"
  
  # Retrieve the individual listings
  titles <- read_html(url) %>%
    html_nodes(css = ".part-link") %>%
    html_text %>%
    purrr::map_chr(~str_remove_all(.x, "\n"))
  
  # get the DOI for the article
  articleurls <- read_html(url) %>%
    html_nodes(css = ".doi") %>%
    html_attr("href")  
  
  # get screenshots of the title and abstract to attach to the tweets
  media_file_names <- 
    map_chr(articleurls, ~.x %>% 
      urltools::path(.) %>% 
      str_replace(., "/", ".") %>% 
      str_glue('.png'))
    
    webshot(articleurls, 
                   file = media_file_names,
                   # top, left, width, and height
                   cliprect = c(550, 0, 1000, 1200),
                   zoom = 1.5)
  
  # compose text of tweet
  tweets <- paste0("New in @saaorg's AAP: ",titles, " ", articleurls, " #archaeology" )
  
  return(list(tweets = tweets,
              media_file_names = media_file_names))
}  

# run the function 
tweets <- write_aap_tweets()

# One-time token set-up ----------------
#
# go to apps.twitter.com to set up
#
# appname <- "aap_saaorg_tweetbot"
# 
# key <- "kEsQRCaY0yT8qebGcT9GcSfvs"
# secret <- "BWTYlxfatWxXpTfDF9uaYY57JmtKXeFfHPbtzjwQ6PaGPvc6EJ"
# 
# aap_saaorg_twitter_token <- create_token(
#   app = appname,
#   consumer_key = key,
#   consumer_secret = secret)
## save token to home directory
# saveRDS(aap_saaorg_twitter_token, file = "aap_saaorg_twitter_token.rds")
# don't git commit the token, add it to .gitignore
#---------------------------------------

twitter_token <- readRDS("aap_saaorg_twitter_token.rds")

for(i in 1:length(tweets$tweets)){  
  post_tweet(tweets$tweets[i], 
             media = tweets$media_file_names[i],
             token=twitter_token, 
             )
  if(i<length(tweets$tweets)){Sys.sleep(time=180)}
}

# clean up by deleting the image files
unlink(tweets$media_file_names)
