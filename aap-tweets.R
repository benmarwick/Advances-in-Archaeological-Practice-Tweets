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
# of the article (and sometimes the surnames of the authors)

# get list of folders on google drive
get_google_drive_folders <-  drive_ls("@aap_saaorg/current items")

doi_regex <- "10[.][0-9]{4,}([^\\s]+)"

# get DOIs only
google_drive_folders_dois <- str_extract(get_google_drive_folders$name, doi_regex)
# no slash
google_drive_folders_dois_no_slash <- str_replace_all(google_drive_folders_dois, "/", "\\.")

# rename the folders to get rid of / and strip down to DOI only
map2(get_google_drive_folders$id,
     google_drive_folders_dois_no_slash,
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
  print(str_glue("Trying to get an image from {i}..."))
  pwd <- here::here()
  setwd(pwd)
  # get contents of google drive folder matching DOI
  x <- drive_ls(str_glue('@aap_saaorg/current items/{i}'))
  if(nrow(x) > 0) {
    # get only images
    x <- x %>% filter(str_detect(tolower(name), ".jpg|.jpeg|.png|.tif|.pdf"))
    # download each image to the local folder for this DOI
    setwd(i)
    map(x$id, ~drive_download(as_id(.x), 
                   overwrite = TRUE))
    setwd(pwd)
    
  } else {
    # skip and do nothing
    print(str_glue("No image file in {i}"))
  }
  print(str_glue("Finished with {i}."))
}


# if jpg, tif or pdf, convert to png
dois <- list.dirs() %>% 
  str_subset("10.1017") %>% 
  str_remove_all(regex("^./| .*$"))

for(i in dois){
  
  pwd <- here::here()
  setwd(i)
  print(str_glue("Starting to work on {i}..."))
  
  w <- list.files(".", pattern = ".png|.PNG$")
  x <- list.files(".", pattern = ".tif|.TIF|.tiff|.TIFF$")
  y <- list.files(".", pattern = ".pdf|.PDF$")
  z <- list.files(".", pattern = ".jpg|.JPG|.jpeg|.JPEG$")
  
 
  if(sum(length(x), length(y), length(z), length(w)) != 0){
    
  # if we have a tif, convert to png
     walk(x, ~png::writePNG(readTIFF(.x, convert =  TRUE), 
                            target = paste0(i, "-", 
                                            format(Sys.time(), "%H%M%S%OS3"), 
                                            '.png')))
 
    
    # if we have a convert PDF, convert to png
      walk(y, ~png::writePNG(pdf_render_page(.x, page = 1, dpi = 300), 
                             target = paste0(i, "-", 
                                             format(Sys.time(), "%H%M%S%OS3"), 
                                             '.png')))
    
    # , convert to jpg, convert png
      walk(z, ~writePNG(readJPEG(.x), paste0(i, "-", 
                                             format(Sys.time(), "%H%M%S%OS3"), 
                                             '.png')))
     
      
      # if we already have a png, rename the png to DOI.png
        walk(w, ~png::writePNG(readPNG(.x), target = paste0(i, "-", 
                                                            format(Sys.time(), "%H%M%S%OS3"), 
                                                            '.png')))
        
        # delete the original PNG
        walk(w, ~rm(.x))
        
      } else {
    
    # do nothing
        print(str_glue("No converting done for {i}..."))
      }
  
 
  # check file size is under 3 MB (limit is 5MB)
  # and shrink it if it's over, shrink to X00 px wide
  imgs <- list.files( pattern = paste0(i, ".*png"))
  for(img in imgs){

  file_size <- file.size(img) / 1e6
  if(file_size > 3 & !is.na(file_size)) { 
    
    webshot::resize(img,  "600x")
    
    print(str_glue("Finished resizing on {i}..."))
    
  } else {
    
    # do nothing
    print(str_glue("No resizing done for {i}..."))
    
  }
  }
  
  setwd(pwd)
  print(str_glue("Finished work on {i}..."))
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
  print("Getting the article titles...")
  
  # get the link to the article
  articlelinks <- read_html(url) %>%
    html_nodes(css = ".part-link") %>%
    html_attr("href")  
  print("Getting the article DOIs...")
  
  # go to the article page to get its DOI
  articleurls <- 
    str_glue('https://www.cambridge.org{articlelinks}') %>% 
      map_chr( ~read_html(.x) %>% 
                html_nodes(css = ".doi") %>%
                html_attr("href") %>% 
             .[[1]]) # because the reference list sometimes has DOIs
  print("Getting the article DOIs...")
  
  
  # get screenshots of the title and abstract to attach to the tweets
  media_file_names <- 
    map_chr(articleurls, ~.x %>% 
      urltools::path(.) %>% 
      str_replace(., "/", ".") %>% 
      str_glue('.png'))
  print("Getting the article screenshots...")
    
    webshot(articleurls, 
                   file = media_file_names,
                   # top, left, width, and height
                   cliprect = c(550, 0, 1000, 1200),
                   zoom = 1.5)
  
  # compose text of tweet
  tweets <- paste0("New in @saaorg's AAP: ",titles, " ", articleurls, " #archaeology" )
  
  return(list(tweets = tweets,
              media_file_names = media_file_names,
              articleurls = articleurls))
}  

#------------------------------------------------------------------
# Compose tweets

# run the function 
tweets <- write_aap_tweets()

# One-time token set-up ----------------
#
# go to apps.twitter.com to set up
#
# appname <- "aap_saaorg_tweetbot"
# 
# key <- ""
# secret <- ""
# 
# aap_saaorg_twitter_token <- create_token(
#   app = appname,
#   consumer_key = key,
#   consumer_secret = secret)
# # save token to home directory
# saveRDS(aap_saaorg_twitter_token, file = "aap_saaorg_twitter_token.rds")
# don't git commit the token, add it to .gitignore
#---------------------------------------


# this is securely stored online on the @aap_saaorg Google Drive 
# at https://drive.google.com/drive/u/1/folders/1ChWXaeK5_dMN6YoH6ocWf6dNA6-xd_2K
twitter_token <- readRDS("aap_saaorg_twitter_token.rds")

# Post tweets to the world!
# Warning: take a look at `tweets` first to make sure they look good

article_ids <- str_remove(tweets$articleurls, "https://doi.org/")
article_ids <- str_replace(article_ids, "/", ".")
 
for(i in 1:length(tweets$tweets)){  
  print(tweets$tweets[i])
  # post the text
  post_tweet(tweets$tweets[i], 
             # attach the screenshot of the title and abstract
             media = c(tweets$media_file_names[i],
                       # attach the PNGs from the folder
                       # that has a filename that matches the DOI
                       list.files(str_remove(tweets$media_file_names[i], ".png"),
                                  pattern = paste0(article_ids[i], ".*png"), 
                                  full.names = TRUE,
                                  recursive = TRUE)),
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

# END


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




