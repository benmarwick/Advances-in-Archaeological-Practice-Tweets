library(dplyr)
library(rtweet)
library(rvest)
library(stringr)
library(magrittr)
library(purrr)
library(googledrive)
library("jpeg")
library("tiff")
library(png)
library(pdftools)
library(imager)
library(webshot2) # remotes::install_github("rstudio/webshot2")

#---------------------------------------------------------------------
# We have some folders on Google drive that the editor has selected
# some images from each article. The folders are named with the DOI 
# of the article (and nothing else in the name, only the DOI)

# get list of folders on google drive, assuming that I only have one 
# folder called 'Current Items', and that's the one with the images!
g_drive <- drive_find(q = "name contains 'Current Items'")
get_google_drive_folders <-  drive_ls(as_id(g_drive$id))

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
g_drive <- drive_find(q = "name contains 'Current Items'")
get_google_drive_folders <-  drive_ls(as_id(g_drive$id))

# just ones with well-formed names
get_google_drive_folders <- 
  get_google_drive_folders %>% 
  filter(str_detect(name, "10.1017"))

# create a directory for each DOI to hold what we get from google drive
map(get_google_drive_folders$name, dir.create)

# for each DOI-directory, get contents of matching folder on google drive
for (i in seq(get_google_drive_folders$id)) {
  print(str_glue("Trying to get an image from {get_google_drive_folders$name[i]}..."))
  pwd <- here::here()
  setwd(pwd)
  # get contents of google drive folder matching DOI
  x <- drive_ls(str_glue('https://drive.google.com/drive/u/1/folders/{get_google_drive_folders$id[i]}'))
  if(nrow(x) > 0) {
    # get only images
    x <- x %>% filter(str_detect(tolower(name), ".jpg|.jpeg|.png|.tif|.pdf"))
    # download each image to the local folder for this DOI
    setwd(get_google_drive_folders$name[i])
    map(x$id, ~drive_download(as_id(.x), 
                   overwrite = TRUE))
    setwd(pwd)
    
  } else {
    # skip and do nothing
    print(str_glue("No image file in {get_google_drive_folders$name[i]}"))
  }
  print(str_glue("Finished with {get_google_drive_folders$name[i]}."))
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
                                            '.png')),
          dpi = 300)
 
    
    # if we have a convert PDF, convert to png
      walk(y, ~png::writePNG(pdf_render_page(.x, page = 1, dpi = 300), 
                             target = paste0(i, "-", 
                                             format(Sys.time(), "%H%M%S%OS3"), 
                                             '.png')),
           dpi = 300)
    
    # , convert to jpg, convert png
      walk(z, ~png::writePNG(readJPEG(.x), paste0(i, "-", 
                                             format(Sys.time(), "%H%M%S%OS3"), 
                                             '.png')),
           dpi = 300)
     
      
      # if we already have a png, rename the png to DOI.png
        walk(w, ~png::writePNG(readPNG(.x), target = paste0(i, "-", 
                                                            format(Sys.time(), "%H%M%S%OS3"), 
                                                            '.png')),
             dpi = 300)
        
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
  if(file_size > 4 & !is.na(file_size)) { 
    
    webshot::resize(img,  "900x")
    
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

# get twitter handles from our google sheet
library(googlesheets4)
sheet_with_twitter_accounts <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1wo1pLRvC8Vhzoelyd-v0g66FYDQSvcgqG6Qhw9bmx04/edit#gid=0") %>% 
  mutate(basenames = str_remove(doi, "10.1017."))
  
#-----------------------------------------------------------------------------

# Here is a function to scrape the AAP website of the TOC for the latest issue
# It grabs the article titles and DOIs, then composes a tweet with them

write_aap_tweets <- function(){
  
  url <- "https://www.cambridge.org/core/journals/advances-in-archaeological-practice/latest-issue"
  
  #create a web session with the desired login address
  url_uw <- "myuw.washington.edu"
  pgsession <- session(url_uw)
  pgform <- html_form(pgsession)[[1]]  
  filled_form <- html_form_set(pgform, 
                            j_username="bmarwick", 
                            j_password="****`")
  session_submit(pgsession, filled_form)
  
  # Retrieve the individual listings
  titles <- session_jump_to(pgsession, url) %>%
    html_nodes(css = ".part-link") %>%
    html_text %>%
    purrr::map_chr(~str_remove_all(.x, "\n"))
  print("Getting the article titles...")
  
  # get the link to the article
  articlelinks <- session_jump_to(pgsession, url) %>%
    html_nodes(css = ".part-link") %>%
    html_attr("href")  
  print("Getting the article links...")
  
  # go to the article page to get its DOI
  articleurls <- 
    str_glue('https://www.cambridge.org{articlelinks}') %>% 
      map_chr( ~session_jump_to(pgsession, .x) %>% 
                html_nodes(css = "#article-tab .app-link__text.app-link--accent .text") %>%
                 html_text() ) # because the reference list sometimes has DOIs
  print("Getting the article DOIs...")
  
  # get screenshots of the title and abstract to attach to the tweets
  media_file_names <- 
    map_chr(articleurls, ~.x %>% 
      urltools::path(.) %>% 
      str_replace(., "/", ".") %>% 
      str_glue('.png'))
  print("Getting the article screenshots...")
    
  for(i in 1:length(articleurls)){
    
  print(paste0("Getting the screenshot for ", articleurls[[i]], "..."))
    
  webshot(articleurls[[i]], 
          file = media_file_names[[i]],
          vwidth = 1200,
          vheight = 1300,
          delay = 1,
          # top, left, width, and height
          cliprect =  #"viewport",
            c(0, 
              0, 
              1200,
              1300),
          useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.212 Safari/537.36",
          zoom = 1)
  
  text_img <- magick::image_read(media_file_names[[i]])
  
  text_img_cropped <- 
    magick::image_crop(text_img,
                       geometry="+0+110",
                       gravity="NorthWest",
                       repage=TRUE)
  
  magick::image_write(text_img_cropped,
                      path = media_file_names[i])
}
  
    # get twitter handle of authors from our spreadsheet
    twitter_handles <- 
    sheet_with_twitter_accounts$`twitter-account`[ match(basename(articleurls), 
          sheet_with_twitter_accounts$basenames)]
    twitter_handles <- ifelse(!is.na(twitter_handles), 
                              paste0("by ", twitter_handles, " "), 
                              twitter_handles)
    twitter_handles[is.na(twitter_handles)] <- ""
    
    # get custom hastags for each paper from our spreadsheet
    custom_hashtags <- 
      sheet_with_twitter_accounts$hashtags[ match(basename(articleurls), 
                                                           sheet_with_twitter_accounts$basenames)]
    custom_hashtags[is.na(custom_hashtags)] <- ""
  
  # compose text of tweet
  tweets <- paste0("New in @saaorg's AAP: ",titles, " ", 
                   twitter_handles, articleurls, 
                   " #archaeology ", custom_hashtags )
  
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

# updated with the current way to auth twitter
# aap_saaorg_twitter_token <-
#   rtweet_bot(api_key = "", 
#              api_secret = "", 
#              access_token = "", 
#              access_secret = "")
# saveRDS(aap_saaorg_twitter_token, file = "aap_saaorg_twitter_token.rds")

#---------------------------------------


# this is securely stored online on the @aap_saaorg Google Drive 
# at https://drive.google.com/drive/u/1/folders/1ChWXaeK5_dMN6YoH6ocWf6dNA6-xd_2K
aap_saaorg_twitter_token <- auth_as(auth = "aap_saaorg_twitter_token.rds")
auth_as(auth = aap_saaorg_twitter_token)

# check it
auth_get() 

# it didn't work until I logged into the aap twitter, 
# ran auth_setup_default() 
# and then it just worked fine, very odd

# Post tweets to the world!
# Warning: take a look at `tweets` first to make sure they look good

tweets

# check chr length, max allowed in 280
nchar(tweets$tweets)

article_ids <- str_remove(tweets$articleurls, "https://doi.org/")
article_ids <- str_replace(article_ids, "/", ".")
 
for(i in 9:length(tweets$tweets)){  
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
         media_alt_text = c("screenshot of the abstract of the journal article",
                            rep(
                            "figure from the journal article",
                            length( list.files(str_remove(tweets$media_file_names[i], ".png"),
                                               pattern = paste0(article_ids[i], ".*png"), 
                                               full.names = TRUE,
                                               recursive = TRUE))))
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




