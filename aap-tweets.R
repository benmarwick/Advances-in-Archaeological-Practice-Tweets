library(dplyr)
library(rtweet)
library(rvest)
library(stringr)
library(magrittr)
library(webshot)
library(purrr)
library(googledrive)

#---------------------------------------------------------------------
# We have some folders on Google drive that the editor has selected
# some images from each article. The folders are named with the DOI 
# of the article

# get list of folders on google drive
get_google_drive_folders <-  drive_ls("@aap_saaorg")

# create a directory for each DOI to hold what we get from google drive
map(get_google_drive_folders$name, dir.create)

# for each DOI-directory, get contents of matching folder on google drive
for (i in get_google_drive_folders$name) {
  print(i)
  # get contents of google drive folder matching DOI
  x <- drive_ls(str_glue('@aap_saaorg/{i}'))
  # get only images
  x <- x %>% filter(str_detect(tolower(name), ".jpg|.png"))
  # download this to the local folder for this DOI
  setwd(i)
  drive_download(as_id(x$id), 
                 overwrite = TRUE)
  setwd("../")
}

# Now we have local folders named by the DOI of the article, with an image
# in each that we want to attach to the tweet

#-----------------------------------------------------------------------------

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

#------------------------------------------------------------------
# Post tweets

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
             # TODO: add image from google drive also, only where image exists 
             token=twitter_token, 
             )
  if(i<length(tweets$tweets)){Sys.sleep(time=180)}
}

#------------------------------------------------------------------
# clean up

# clean up by deleting the image files
unlink(tweets$media_file_names)

# clean up by deleting folders with ditor-selected images
unlink(get_google_drive_folders$name, recursive = TRUE, force = TRUE)


