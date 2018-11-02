library(dplyr)
library(rtweet)
library(rvest)
library(stringr)
library(magrittr)
library(webshot)
library(purrr)
library(googledrive)
library("jpeg")
library("tiff")
library(png)
library(pdftools)
library(imager)


#---------------------------------------------------------------------
# We have some folders on Google drive that the editor has selected
# some images from each article. The folders are named with the DOI 
# of the article

# get list of folders on google drive
get_google_drive_folders <-  drive_ls("@aap_saaorg/current items")

# rename the folders to get rid of /
map2(get_google_drive_folders$id,
     str_replace_all(get_google_drive_folders$name, "/", "\\."),
     ~drive_rename(as_id(.x), 
             name = .y, 
             verbose = TRUE))

# get list of folders on google drive again
get_google_drive_folders <-  drive_ls("@aap_saaorg/current items")

# just ones with well-formed names
get_google_drive_folders <- 
  get_google_drive_folders %>% 
  filter(str_detect(name, "10.1017"))

# create a directory for each DOI to hold what we get from google drive
map(get_google_drive_folders$name, dir.create)

# for each DOI-directory, get contents of matching folder on google drive
for (i in get_google_drive_folders$name) {
  print(i)
  # get contents of google drive folder matching DOI
  x <- drive_ls(str_glue('@aap_saaorg/current items/{i}'))
  # get only images
  x <- x %>% filter(str_detect(tolower(name), ".jpg|.png|.tif|.pdf"))
  # download this to the local folder for this DOI
  setwd(i)
  drive_download(as_id(x$id), 
                 overwrite = TRUE)
  setwd("../")
}


# if jpg, tif or pdf, convert to png
for(i in get_google_drive_folders$name){
  
  setwd(i)
  print(i)
  
  w <- list.files(".", pattern = ".png|.PNG$")
  x <- list.files(".", pattern = ".tif|.TIF|.tiff|.TIFF$")
  y <- list.files(".", pattern = ".pdf|.PDF$")
  z <- list.files(".", pattern = ".jpg|.JPG|.jpeg|.JPEG$")
  
 
  # if we have a tif, convert to png
   if(length(x) == 1){
  img <- readTIFF(x)
  png::writePNG(img, target = paste0(i, '.png'))
  
  } else 
    
    # if we have a convert PDF, convert to png
    if(length(y) == 1){
      bitmap <- pdf_render_page(x, page = 1, dpi = 300)
      png::writePNG(bitmap, target = paste0(i, '.png'))

  } else
    
    # , convert to jpg, convert png
    if(length(z) == 1){
      jpg <- readJPEG(z)
      writePNG(jpg, paste0(i, '.png'))
 
    } else 
      
      # if we already have a png, rename the png to DOI.png
      if(length(w) == 1){
        w <- readPNG(w)
        png::writePNG(w, target = paste0(i, '.png'))
        # delete the original PNG
        rm(w)
        
      } else {
    
    # do nothing
  }
 
  # check file size is under 3 MB (limit is 5MB)
  # and shrink it if it's over, shrink to X00 px wide
  img <- paste0(i, '.png')
  file_size <- file.size(img) / 1e6
  if(file_size > 3) { 
    
    webshot::resize(img,  "600x")
    
  } else {
    
    # do nothing
    
  }
  
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
# key <- "xxx"
# secret <- "xxx"
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
  print(tweets$tweets[i])
  # post the text
  post_tweet(tweets$tweets[i], 
             # attach the screenshot of the title and abstract
             media = c(tweets$media_file_names[i],
                       # attach the PNGs from the folder
                       # that has a filename that matches the DOI
                       list.files(str_remove(tweets$media_file_names[i], ".png"),
                                  pattern = "png$", 
                                  full.names = TRUE)),
             token=twitter_token
             )
  if(i<length(tweets$tweets)){Sys.sleep(time=180)}
}

#------------------------------------------------------------------
# clean up

# clean up by deleting the image files
unlink(tweets$media_file_names)

# clean up by deleting folders with editor-selected images
unlink(str_subset(list.dirs(), "10.1017"), recursive = TRUE, force = TRUE)


#---------------------------------------------------------------------
# if I delete a bunch of directories of recent articles from gdrive, here's how I can make them locally
url <- "https://www.cambridge.org/core/journals/advances-in-archaeological-practice/latest-issue"
library(rvest)

html <- read_html(url)
doi <- html_nodes(html, ".doi")

dois <- 
  doi %>% 
  html_text() %>% 
  str_remove_all("https://doi.org/") %>% 
  str_replace_all("/", 
                  "\\.") 

map(dois, ~dir.create(.x))




