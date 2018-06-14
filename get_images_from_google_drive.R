
library(googledrive)

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

# post tweet with images...

# clean up by deleting folders
unlink(get_google_drive_folders$name, recursive = TRUE, force = TRUE)



        
               