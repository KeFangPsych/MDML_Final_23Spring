# Install and load the Google drive package
install.packages("googledrive")
library(googledrive)

# Authenticate with Google (permission required)
# See console for verification
drive_auth()

# Get the file ID from the file URL
temp_file <- tempfile()
drive_download(drive_get(as_id("1hdZyGfpNsKPGUBXu8tTTocsVhceR2V7E")), 
               path = temp_file)
source(temp_file)

# Liberal media from facebook
# str(lib_media_fb_data)

# Conservative media from facebook
# str(con_media_fb_data)

# Liberal congressman/woman from facebook
# str(lib_congress_fb_data)

# Conservative congressman/woman from facebook
# str(con_congress_fb_data)
