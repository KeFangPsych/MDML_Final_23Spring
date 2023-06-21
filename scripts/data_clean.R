# Install and load the Google drive package
#install.packages("googledrive")
library(googledrive)
#install.packages("cld2")
library(cld2)
library(tidyverse)
library(stringr)
library(tidytext)
data(stop_words)

# put the data_retrieve.R file in the same folder
source('scripts/data_retrieve.R')


con_congress_fb_clean <- con_congress_fb_data %>%
  select(-liberalidentity, -conservativeidentity, -TopDemocrat, -TopRepublican, 
         -DemocratCongress, -RepublicanCongress, -Democrat, -Republican, 
         -Democrat2, -Republican2, -PositiveAffect, -NegativeAffect, -`Facebook Id`,
         -MoralEmotionalPositive, -MoralEmotionalNegative, -MoralEmotional, 
         -Moral, -Affect, -Anger, -Anxiety, -Sadness, -Polarization, -`Overperforming Score`) %>%
  rename(page_name = `Page Name`,
         user_name = `User Name`,
         followers = `Likes at Posting`,
         post_date = Created,
         post_type = Type,
         response_like = Likes,
         response_comment = Comments,
         response_share = Shares,
         response_love = Love,
         response_wow = Wow,
         response_haha = Haha,
         response_sad = Sad,
         response_angry = Angry,
         response_care = Care,
         video_type = `Video Share Status`,
         video_view_from_post = `Post Views`,
         video_view_total = `Total Views`,
         video_view_crossposts = `Total Views For All Crossposts`,
         video_length = `Video Length`,
         post_url = URL,
         post_text = Message,
         attached_link = `Final Link`,
         attached_link2 = Link,
         link_text = `Link Text`,
         link_description = Description,
         image_text = `Image Text`,
         sponsor_id = `Sponsor Id`,
         sponsor_name = `Sponsor Name`,
         all_text = text) %>%
  mutate(poster_party = "con",
         poster_role = "congress") %>%
  unite(col = "all_text", c(post_text, image_text, link_text, link_description), sep = ". ", na.rm = TRUE, remove = FALSE) %>%
  mutate(all_text = str_replace_all(all_text, "[\U{1F300}-\U{1F5FF}\U{1F600}-\U{1F64F}\U{1F680}-\U{1F6FF}\U{1F700}-\U{1F77F}\U{1F780}-\U{1F7FF}\U{1F800}-\U{1F8FF}\U{1F900}-\U{1F9FF}\U{1FA00}-\U{1FA6F}\U{1FA70}-\U{1FAFF}\U{02702}-\U{027B0}\U{02639}\U{1F300}-\U{1F5FF}\U{2B05}-\U{2B07}]", "")) %>% 
  mutate(language = detect_language(all_text))%>% 
  filter(language == 'en') %>%
  mutate(all_text = str_replace_all(all_text, "http[^[:space:]]*", "")) %>%
  mutate(all_text = str_replace_all(all_text, "\\d", "")) %>%
  unnest_tokens(word, all_text) %>%
  anti_join(stop_words)

#saveRDS(con_congress_fb_clean, file = "data/con_congress_fb_clean.rds")

lib_congress_fb_clean <- lib_congress_fb_data %>%
  select(-liberalidentity, -conservativeidentity, -TopDemocrat, -TopRepublican, 
         -DemocratCongress, -RepublicanCongress, -Democrat, -Republican, 
         -Democrat2, -Republican2, -PositiveAffect, -NegativeAffect, -`Facebook Id`,
         -MoralEmotionalPositive, -MoralEmotionalNegative, -MoralEmotional, 
         -Moral, -Affect, -Anger, -Anxiety, -Sadness, -Polarization, -`Overperforming Score`) %>%
  rename(page_name = `Page Name`,
         user_name = `User Name`,
         followers = `Likes at Posting`,
         post_date = Created,
         post_type = Type,
         response_like = Likes,
         response_comment = Comments,
         response_share = Shares,
         response_love = Love,
         response_wow = Wow,
         response_haha = Haha,
         response_sad = Sad,
         response_angry = Angry,
         response_care = Care,
         video_type = `Video Share Status`,
         video_view_from_post = `Post Views`,
         video_view_total = `Total Views`,
         video_view_crossposts = `Total Views For All Crossposts`,
         video_length = `Video Length`,
         post_url = URL,
         post_text = Message,
         attached_link = `Final Link`,
         attached_link2 = Link,
         link_text = `Link Text`,
         link_description = Description,
         image_text = `Image Text`,
         sponsor_id = `Sponsor Id`,
         sponsor_name = `Sponsor Name`,
         all_text = text) %>%
  mutate(poster_party = "lib",
         poster_role = "congress") %>%
  unite(col = "all_text", c(post_text, image_text, link_text, link_description), sep = ". ", na.rm = TRUE, remove = FALSE) %>%
  mutate(all_text = str_replace_all(all_text, "[\U{1F300}-\U{1F5FF}\U{1F600}-\U{1F64F}\U{1F680}-\U{1F6FF}\U{1F700}-\U{1F77F}\U{1F780}-\U{1F7FF}\U{1F800}-\U{1F8FF}\U{1F900}-\U{1F9FF}\U{1FA00}-\U{1FA6F}\U{1FA70}-\U{1FAFF}\U{02702}-\U{027B0}\U{02639}\U{1F300}-\U{1F5FF}\U{2B05}-\U{2B07}]", "")) %>% 
  mutate(language = detect_language(all_text))%>% 
  filter(language == 'en') %>%
  mutate(all_text = str_replace_all(all_text, "http[^[:space:]]*", "")) %>%
  mutate(all_text = str_replace_all(all_text, "\\d", "")) %>%
  unnest_tokens(word, all_text) %>%
  anti_join(stop_words)

#saveRDS(lib_congress_fb_clean, file = "data/lib_congress_fb_clean.rds")

con_media_fb_clean <- con_media_fb_data %>%
  select(-liberalidentity, -conservativeidentity, -TopDemocrat, -TopRepublican, 
         -DemocratCongress, -RepublicanCongress, -Democrat, -Republican, 
         -Democrat2, -Republican2, -PositiveAffect, -NegativeAffect, -`Facebook Id`,
         -MoralEmotionalPositive, -MoralEmotionalNegative, -MoralEmotional, 
         -Moral, -Affect, -Anger, -Anxiety, -Sadness, -Polarization, -`Overperforming Score`) %>%
  rename(page_name = `Page Name`,
         user_name = `User Name`,
         followers = `Likes at Posting`,
         post_date = Created,
         post_type = Type,
         response_like = Likes,
         response_comment = Comments,
         response_share = Shares,
         response_love = Love,
         response_wow = Wow,
         response_haha = Haha,
         response_sad = Sad,
         response_angry = Angry,
         response_care = Care,
         video_type = `Video Share Status`,
         video_view_from_post = `Post Views`,
         video_view_total = `Total Views`,
         video_view_crossposts = `Total Views For All Crossposts`,
         video_length = `Video Length`,
         post_url = URL,
         post_text = Message,
         attached_link = `Final Link`,
         attached_link2 = Link,
         link_text = `Link Text`,
         link_description = Description,
         image_text = `Image Text`,
         sponsor_id = `Sponsor Id`,
         sponsor_name = `Sponsor Name`,
         all_text = text) %>%
  mutate(poster_party = "con",
         poster_role = "media") %>%
  unite(col = "all_text", c(post_text, image_text, link_text, link_description), sep = ". ", na.rm = TRUE, remove = FALSE) %>%
  mutate(all_text = str_replace_all(all_text, "[\U{1F300}-\U{1F5FF}\U{1F600}-\U{1F64F}\U{1F680}-\U{1F6FF}\U{1F700}-\U{1F77F}\U{1F780}-\U{1F7FF}\U{1F800}-\U{1F8FF}\U{1F900}-\U{1F9FF}\U{1FA00}-\U{1FA6F}\U{1FA70}-\U{1FAFF}\U{02702}-\U{027B0}\U{02639}\U{1F300}-\U{1F5FF}\U{2B05}-\U{2B07}]", "")) %>% 
  mutate(language = detect_language(all_text))%>% 
  filter(language == 'en') %>%
  mutate(all_text = str_replace_all(all_text, "http[^[:space:]]*", "")) %>%
  mutate(all_text = str_replace_all(all_text, "\\d", "")) %>%
  unnest_tokens(word, all_text) %>%
  anti_join(stop_words)

#saveRDS(con_media_fb_clean, file = "data/con_media_fb_clean.rds")

lib_media_fb_clean <- lib_media_fb_data %>%
  select(-liberalidentity, -conservativeidentity, -TopDemocrat, -TopRepublican, 
         -DemocratCongress, -RepublicanCongress, -Democrat, -Republican, 
         -Democrat2, -Republican2, -PositiveAffect, -NegativeAffect, -`Facebook Id`,
         -MoralEmotionalPositive, -MoralEmotionalNegative, -MoralEmotional, 
         -Moral, -Affect, -Anger, -Anxiety, -Sadness, -Polarization, -`Overperforming Score`) %>%
  rename(page_name = `Page Name`,
         user_name = `User Name`,
         followers = `Likes at Posting`,
         post_date = Created,
         post_type = Type,
         response_like = Likes,
         response_comment = Comments,
         response_share = Shares,
         response_love = Love,
         response_wow = Wow,
         response_haha = Haha,
         response_sad = Sad,
         response_angry = Angry,
         response_care = Care,
         video_type = `Video Share Status`,
         video_view_from_post = `Post Views`,
         video_view_total = `Total Views`,
         video_view_crossposts = `Total Views For All Crossposts`,
         video_length = `Video Length`,
         post_url = URL,
         post_text = Message,
         attached_link = `Final Link`,
         attached_link2 = Link,
         link_text = `Link Text`,
         link_description = Description,
         image_text = `Image Text`,
         sponsor_id = `Sponsor Id`,
         sponsor_name = `Sponsor Name`,
         all_text = text) %>%
  mutate(poster_party = "lib",
         poster_role = "media") %>%
  unite(col = "all_text", c(post_text, image_text, link_text, link_description), sep = ". ", na.rm = TRUE, remove = FALSE) %>%
  mutate(all_text = str_replace_all(all_text, "[\U{1F300}-\U{1F5FF}\U{1F600}-\U{1F64F}\U{1F680}-\U{1F6FF}\U{1F700}-\U{1F77F}\U{1F780}-\U{1F7FF}\U{1F800}-\U{1F8FF}\U{1F900}-\U{1F9FF}\U{1FA00}-\U{1FA6F}\U{1FA70}-\U{1FAFF}\U{02702}-\U{027B0}\U{02639}\U{1F300}-\U{1F5FF}\U{2B05}-\U{2B07}]", "")) %>% 
  mutate(language = detect_language(all_text))%>% 
  filter(language == 'en') %>%
  mutate(all_text = str_replace_all(all_text, "http[^[:space:]]*", "")) %>%
  mutate(all_text = str_replace_all(all_text, "\\d", "")) %>%
  unnest_tokens(word, all_text) %>%
  anti_join(stop_words)

#saveRDS(lib_media_fb_clean, file = "data/lib_media_fb_clean.rds")

#con_congress_fb_clean <- readRDS("data/con_congress_fb_clean.rds")
#lib_congress_fb_clean <- readRDS("data/lib_congress_fb_clean.rds")
#con_media_fb_clean <- readRDS("data/con_media_fb_clean.rds")
#lib_media_fb_clean <- readRDS("data/lib_media_fb_clean.rds")

all_con <- rbind(con_congress_fb_clean, con_media_fb_clean)
#saveRDS(all_con, file = "data/all_con.rds")

all_lib <- rbind(lib_congress_fb_clean, lib_media_fb_clean)
#saveRDS(all_lib, file = "data/all_lib.rds")

all_congress <- rbind(con_congress_fb_clean, lib_congress_fb_clean)
#saveRDS(all_congress, file = "data/all_congress.rds")

all_media <- rbind(con_media_fb_clean, lib_media_fb_clean)
#saveRDS(all_media, file = "data/all_media.rds")

all_clean <- rbind(con_congress_fb_clean, lib_congress_fb_clean, con_media_fb_clean, lib_media_fb_clean)
#saveRDS(all_clean, file = "data/all_data.rds")


con_congress_posts <- con_congress_fb_data %>%
  select(-liberalidentity, -conservativeidentity, -TopDemocrat, -TopRepublican, 
         -DemocratCongress, -RepublicanCongress, -Democrat, -Republican, 
         -Democrat2, -Republican2, -PositiveAffect, -NegativeAffect, -`Facebook Id`,
         -MoralEmotionalPositive, -MoralEmotionalNegative, -MoralEmotional, 
         -Moral, -Affect, -Anger, -Anxiety, -Sadness, -Polarization, -`Overperforming Score`) %>%
  rename(page_name = `Page Name`,
         user_name = `User Name`,
         followers = `Likes at Posting`,
         post_date = Created,
         post_type = Type,
         response_like = Likes,
         response_comment = Comments,
         response_share = Shares,
         response_love = Love,
         response_wow = Wow,
         response_haha = Haha,
         response_sad = Sad,
         response_angry = Angry,
         response_care = Care,
         video_type = `Video Share Status`,
         video_view_from_post = `Post Views`,
         video_view_total = `Total Views`,
         video_view_crossposts = `Total Views For All Crossposts`,
         video_length = `Video Length`,
         post_url = URL,
         post_text = Message,
         attached_link = `Final Link`,
         attached_link2 = Link,
         link_text = `Link Text`,
         link_description = Description,
         image_text = `Image Text`,
         sponsor_id = `Sponsor Id`,
         sponsor_name = `Sponsor Name`,
         all_text = text) %>%
  mutate(poster_party = "con",
         poster_role = "congress") %>%
  unite(col = "all_text", c(post_text, image_text, link_text, link_description), sep = ". ", na.rm = TRUE, remove = FALSE) %>%
  mutate(all_text = str_replace_all(all_text, "[\U{1F300}-\U{1F5FF}\U{1F600}-\U{1F64F}\U{1F680}-\U{1F6FF}\U{1F700}-\U{1F77F}\U{1F780}-\U{1F7FF}\U{1F800}-\U{1F8FF}\U{1F900}-\U{1F9FF}\U{1FA00}-\U{1FA6F}\U{1FA70}-\U{1FAFF}\U{02702}-\U{027B0}\U{02639}\U{1F300}-\U{1F5FF}\U{2B05}-\U{2B07}]", "")) %>% 
  mutate(language = detect_language(all_text))%>% 
  filter(language == 'en') %>%
  mutate(all_text = str_replace_all(all_text, "http[^[:space:]]*", "")) %>%
  mutate(all_text = str_replace_all(all_text, "\\d", ""))

#saveRDS(con_congress_posts, file = "data/con_congress_posts.rds")


lib_congress_posts <- lib_congress_fb_data %>%
  select(-liberalidentity, -conservativeidentity, -TopDemocrat, -TopRepublican, 
         -DemocratCongress, -RepublicanCongress, -Democrat, -Republican, 
         -Democrat2, -Republican2, -PositiveAffect, -NegativeAffect, -`Facebook Id`,
         -MoralEmotionalPositive, -MoralEmotionalNegative, -MoralEmotional, 
         -Moral, -Affect, -Anger, -Anxiety, -Sadness, -Polarization, -`Overperforming Score`) %>%
  rename(page_name = `Page Name`,
         user_name = `User Name`,
         followers = `Likes at Posting`,
         post_date = Created,
         post_type = Type,
         response_like = Likes,
         response_comment = Comments,
         response_share = Shares,
         response_love = Love,
         response_wow = Wow,
         response_haha = Haha,
         response_sad = Sad,
         response_angry = Angry,
         response_care = Care,
         video_type = `Video Share Status`,
         video_view_from_post = `Post Views`,
         video_view_total = `Total Views`,
         video_view_crossposts = `Total Views For All Crossposts`,
         video_length = `Video Length`,
         post_url = URL,
         post_text = Message,
         attached_link = `Final Link`,
         attached_link2 = Link,
         link_text = `Link Text`,
         link_description = Description,
         image_text = `Image Text`,
         sponsor_id = `Sponsor Id`,
         sponsor_name = `Sponsor Name`,
         all_text = text) %>%
  mutate(poster_party = "lib",
         poster_role = "congress") %>%
  unite(col = "all_text", c(post_text, image_text, link_text, link_description), sep = ". ", na.rm = TRUE, remove = FALSE) %>%
  mutate(all_text = str_replace_all(all_text, "[\U{1F300}-\U{1F5FF}\U{1F600}-\U{1F64F}\U{1F680}-\U{1F6FF}\U{1F700}-\U{1F77F}\U{1F780}-\U{1F7FF}\U{1F800}-\U{1F8FF}\U{1F900}-\U{1F9FF}\U{1FA00}-\U{1FA6F}\U{1FA70}-\U{1FAFF}\U{02702}-\U{027B0}\U{02639}\U{1F300}-\U{1F5FF}\U{2B05}-\U{2B07}]", "")) %>% 
  mutate(language = detect_language(all_text))%>% 
  filter(language == 'en') %>%
  mutate(all_text = str_replace_all(all_text, "http[^[:space:]]*", "")) %>%
  mutate(all_text = str_replace_all(all_text, "\\d", ""))

#saveRDS(lib_congress_posts, file = "data/lib_congress_posts.rds")



con_media_posts <- con_media_fb_data %>%
  select(-liberalidentity, -conservativeidentity, -TopDemocrat, -TopRepublican, 
         -DemocratCongress, -RepublicanCongress, -Democrat, -Republican, 
         -Democrat2, -Republican2, -PositiveAffect, -NegativeAffect, -`Facebook Id`,
         -MoralEmotionalPositive, -MoralEmotionalNegative, -MoralEmotional, 
         -Moral, -Affect, -Anger, -Anxiety, -Sadness, -Polarization, -`Overperforming Score`) %>%
  rename(page_name = `Page Name`,
         user_name = `User Name`,
         followers = `Likes at Posting`,
         post_date = Created,
         post_type = Type,
         response_like = Likes,
         response_comment = Comments,
         response_share = Shares,
         response_love = Love,
         response_wow = Wow,
         response_haha = Haha,
         response_sad = Sad,
         response_angry = Angry,
         response_care = Care,
         video_type = `Video Share Status`,
         video_view_from_post = `Post Views`,
         video_view_total = `Total Views`,
         video_view_crossposts = `Total Views For All Crossposts`,
         video_length = `Video Length`,
         post_url = URL,
         post_text = Message,
         attached_link = `Final Link`,
         attached_link2 = Link,
         link_text = `Link Text`,
         link_description = Description,
         image_text = `Image Text`,
         sponsor_id = `Sponsor Id`,
         sponsor_name = `Sponsor Name`,
         all_text = text) %>%
  mutate(poster_party = "con",
         poster_role = "media") %>%
  unite(col = "all_text", c(post_text, image_text, link_text, link_description), sep = ". ", na.rm = TRUE, remove = FALSE) %>%
  mutate(all_text = str_replace_all(all_text, "[\U{1F300}-\U{1F5FF}\U{1F600}-\U{1F64F}\U{1F680}-\U{1F6FF}\U{1F700}-\U{1F77F}\U{1F780}-\U{1F7FF}\U{1F800}-\U{1F8FF}\U{1F900}-\U{1F9FF}\U{1FA00}-\U{1FA6F}\U{1FA70}-\U{1FAFF}\U{02702}-\U{027B0}\U{02639}\U{1F300}-\U{1F5FF}\U{2B05}-\U{2B07}]", "")) %>% 
  mutate(language = detect_language(all_text))%>% 
  filter(language == 'en') %>%
  mutate(all_text = str_replace_all(all_text, "http[^[:space:]]*", "")) %>%
  mutate(all_text = str_replace_all(all_text, "\\d", ""))

#saveRDS(con_media_posts, file = "data/con_media_posts.rds")



lib_media_posts <- lib_media_fb_data %>%
  select(-liberalidentity, -conservativeidentity, -TopDemocrat, -TopRepublican, 
         -DemocratCongress, -RepublicanCongress, -Democrat, -Republican, 
         -Democrat2, -Republican2, -PositiveAffect, -NegativeAffect, -`Facebook Id`,
         -MoralEmotionalPositive, -MoralEmotionalNegative, -MoralEmotional, 
         -Moral, -Affect, -Anger, -Anxiety, -Sadness, -Polarization, -`Overperforming Score`) %>%
  rename(page_name = `Page Name`,
         user_name = `User Name`,
         followers = `Likes at Posting`,
         post_date = Created,
         post_type = Type,
         response_like = Likes,
         response_comment = Comments,
         response_share = Shares,
         response_love = Love,
         response_wow = Wow,
         response_haha = Haha,
         response_sad = Sad,
         response_angry = Angry,
         response_care = Care,
         video_type = `Video Share Status`,
         video_view_from_post = `Post Views`,
         video_view_total = `Total Views`,
         video_view_crossposts = `Total Views For All Crossposts`,
         video_length = `Video Length`,
         post_url = URL,
         post_text = Message,
         attached_link = `Final Link`,
         attached_link2 = Link,
         link_text = `Link Text`,
         link_description = Description,
         image_text = `Image Text`,
         sponsor_id = `Sponsor Id`,
         sponsor_name = `Sponsor Name`,
         all_text = text) %>%
  mutate(poster_party = "lib",
         poster_role = "media") %>%
  unite(col = "all_text", c(post_text, image_text, link_text, link_description), sep = ". ", na.rm = TRUE, remove = FALSE) %>%
  mutate(all_text = str_replace_all(all_text, "[\U{1F300}-\U{1F5FF}\U{1F600}-\U{1F64F}\U{1F680}-\U{1F6FF}\U{1F700}-\U{1F77F}\U{1F780}-\U{1F7FF}\U{1F800}-\U{1F8FF}\U{1F900}-\U{1F9FF}\U{1FA00}-\U{1FA6F}\U{1FA70}-\U{1FAFF}\U{02702}-\U{027B0}\U{02639}\U{1F300}-\U{1F5FF}\U{2B05}-\U{2B07}]", "")) %>% 
  mutate(language = detect_language(all_text))%>% 
  filter(language == 'en') %>%
  mutate(all_text = str_replace_all(all_text, "http[^[:space:]]*", "")) %>%
  mutate(all_text = str_replace_all(all_text, "\\d", ""))

#saveRDS(lib_media_posts, file = "data/lib_media_posts.rds")
