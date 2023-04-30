# Install and load the Google drive package
install.packages("googledrive")
library(googledrive)

# Authenticate with Google
drive_auth()

# Liberal media from facebook
lib_media_fb_id <- "17dHtrwAGnNzcJTAFrb_WpCMyfPup3-cB"
lib_media_fb <- drive_get(as_id(lib_media_fb_id))
temp_file <- tempfile()
drive_download(lib_media_fb, path = temp_file)
lib_media_fb_data <- readRDS(temp_file)

# Conservative media from facebook
con_media_fb_id <- "1-KBo97SeLYIA28QL7SbMsGkCMUajg7Jh"
con_media_fb <- drive_get(as_id(con_media_fb_id))
temp_file <- tempfile()
drive_download(con_media_fb, path = temp_file)
con_media_fb_data <- readRDS(temp_file)

# Liberal congressman/woman from facebook
lib_congress_fb_id <- "1sIcpN2efXOMFDq6S7GomCS8WYhPN7X2T"
lib_congress_fb <- drive_get(as_id(lib_congress_fb_id))
temp_file <- tempfile()
drive_download(lib_congress_fb, path = temp_file)
lib_congress_fb_data <- readRDS(temp_file)

# Conservative congressman/woman from facebook
con_congress_fb_id <- "1qWMHc9qfKd-1__qOkjZMiFd705amn1mU"
con_congress_fb <- drive_get(as_id(con_congress_fb_id))
temp_file <- tempfile()
drive_download(con_congress_fb, path = temp_file)
con_congress_fb_data <- readRDS(temp_file)
