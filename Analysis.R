# Import packages and data

library(tidyverse)
library(stringr)
library(tidytext)
library(data.table)
library(googledrive)
library(scales)
library(cld2)
library(topicmodels)
library(reshape2)
library(stm)
library(foreach)
library(doParallel)
library(ranger)
library(caret)

con_media_fb_data <- readRDS("C:/ALL/MyMyMy/NYU_MA/NYU_study/23 Spring ML/HWs/MDML_Final_23Spring/data/ConservativeMediaFacebookUpdated.rds")
lib_media_fb_data <- readRDS("C:/ALL/MyMyMy/NYU_MA/NYU_study/23 Spring ML/HWs/MDML_Final_23Spring/data/LiberalMediaFacebookUpdated.rds")
data(stop_words)

# Data cleaning


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


all_media_posts <- rbind(con_media_posts, lib_media_posts)

all_media_posts <- all_media_posts %>% mutate(document_index = row_number())

all_media <- all_media_posts %>%
  unnest_tokens(word, all_text) %>%
  anti_join(stop_words)


all_media <- all_media %>% 
  filter(!(document == "text248160" & poster_party == "con"))

all_media_posts <- all_media_posts %>%
  filter(!(document == "text248160" & poster_party == "con"))



# Discriptive analysis


all_media_drsp <- all_media %>%
  count(poster_party, word) %>%
  group_by(poster_party) %>%
  mutate(proportion_party = n/sum(n)) %>%
  select(-n) %>%
  filter(proportion_party >= 0.0001) %>%
  arrange(desc(proportion_party)) %>%
  pivot_wider(names_from = poster_party, values_from = proportion_party)


fig_1 <- ggplot(all_media_drsp, aes(x = con, y = lib, 
                                    color = abs(con - lib))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "blue") +
  theme(legend.position="none") +
  labs(x = "Conservative", y = "Liberal") + 
  ggtitle("Difference in word frequency between liberal and conservative media")

ggsave(plot=fig_1, file='figures/fig_1.png', height=7, width=7)



# Sentiment analysis

## Bing package


bing_sentiments <- get_sentiments('bing') %>% 
  filter(!word %in% c('trump', 'donald', 'jeo', 'biden', 'vice', 'liberal', 'conservative', 'democrat', 'republican', 'top', 'supreme', 'grand'))

all_sentiment_bing <- all_media %>%
  inner_join(bing_sentiments) %>%
  group_by(document, poster_party) %>%
  count(sentiment, sort = T) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(net_sentiment = ifelse(is.na(positive), 0, positive) - ifelse(is.na(negative), 0, negative))

all_media_posts <- right_join(all_sentiment_bing, all_media_posts, by = c("document", "poster_party"))


all_media_posts <- all_media_posts %>%  
  mutate(net_sentiment = if_else(is.na(net_sentiment), 0, net_sentiment),
         positive = if_else(is.na(positive), 0, positive),
         negative = if_else(is.na(negative), 0, negative))


fig_2 <- ggplot(all_media_posts, 
                aes(x = net_sentiment, y = response_like, color = poster_party)) +
  geom_point(alpha = 0.1) +
  geom_smooth(color = "black") +
  ylab("Likes") +
  xlab("Sentiment")+
  ggtitle("Sentiment and Likes received") +
  ylim(0, 1e+05)+
  xlim(-20, 20) +
  facet_wrap(~poster_party)

ggsave(plot=fig_2, file='figures/fig_2.png', height=5, width=7)


fig_3 <- all_media_posts %>%
  ggplot(aes(x= net_sentiment)) +
  geom_histogram(aes(y=..density..), binwidth=1, fill="lightblue") + 
  geom_density(adjust = 7, alpha = .2, color="grey", fill = "antiquewhite3") + 
  geom_vline(aes(xintercept=mean(net_sentiment)), col="orange", linetype="dashed", size=1) + 
  ylab("Frequency") +
  xlab("Mean Sentiment")+
  ggtitle("Distribution of mean sentiment") +
  facet_wrap(~poster_party)

ggsave(plot=fig_3, file='figures/fig_3.png', height=5, width=8)


top_sentiment_bing <- all_media %>%
  inner_join(bing_sentiments) %>%
  group_by(poster_party, word, sentiment) %>%
  count(sort = TRUE) %>%
  ungroup()


fig_4 <- top_sentiment_bing %>%
  filter(poster_party == "lib") %>%
  group_by(sentiment) %>%
  slice_max(n, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment for liberal media",
       y = NULL)

ggsave(plot=fig_4, file='figures/fig_4.png', height=5, width=8)


fig_5 <- top_sentiment_bing %>%
  filter(poster_party == "con") %>%
  group_by(sentiment) %>%
  slice_max(n, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment for conservative media",
       y = NULL)
ggsave(plot=fig_5, file='figures/fig_5.png', height=5, width=8)


## Afinn package


afinn_sentiments <- get_sentiments('afinn') %>% 
  filter(!word %in% c('trump', 'donald', 'jeo', 'biden', 'vice', 'liberal', 'conservative', 'democrat', 'republican', 'top', 'supreme', 'grand'))


all_sentiment_afinn <- 
  all_media %>%
  inner_join(afinn_sentiments) %>%
  group_by(document, poster_party) %>%
  summarise(sentiment = sum(value)) %>%
  arrange(sentiment)

all_media_posts <- right_join(all_sentiment_afinn, all_media_posts, by = c("document", "poster_party"))

all_media_posts <- all_media_posts %>%  
  mutate(sentiment = if_else(is.na(sentiment), 0, sentiment))

fig_6 <- ggplot(all_media_posts, 
                aes(x = sentiment, y = response_like, color = poster_party)) +
  geom_point(alpha = 0.1) +
  geom_smooth(color = "black") +
  ylim(0, 1e+05)+
  xlim(-50, 50) +
  facet_wrap(~poster_party)
ggsave(plot=fig_6, file='figures/fig_6.png', height=5, width=8)

fig_7 <- 
  all_media_posts %>%
  ggplot(aes(x= sentiment)) +
  geom_histogram(aes(y=..density..), binwidth=1, fill="lightblue") + 
  geom_density(adjust = 7, alpha = .2, color="grey", fill = "antiquewhite3") + 
  geom_vline(aes(xintercept=mean(sentiment)), col="orange", linetype="dashed", size=1) + 
  ylab("Frequency") +
  xlab("Mean Sentiment")+
  ggtitle("Distribution of mean sentiment") +
  facet_wrap(~poster_party)

ggsave(plot=fig_7, file='figures/fig_7.png', height=5, width=8)


## Consistency between packages


sentiment_corr_test <- cor.test(all_media_posts$net_sentiment, all_media_posts$sentiment)
sentiment_corr_test



# LDA


lda_media_data <- all_media %>%
  ungroup() %>% 
  sample_n(200000) %>%
  count(document_index, word)%>%
  cast_dtm(document_index, word, n)


lda_media <- LDA(lda_media_data, k = 20)


lda_media_beta <- tidy(lda_media, matrix = "beta") 

lda_media_beta %>%
  group_by(topic) %>%
  slice_max(beta, n =10) %>%
  ungroup() %>%
  arrange(topic, -beta)


lda_media_beta <- tidy(lda_media, matrix = "beta") 
top_words <- lda_media_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

plot_data <- top_words %>%
  mutate(topic = factor(topic, levels = unique(topic))) %>%
  arrange(topic, -beta) %>%
  group_by(topic) %>%
  mutate(word_rank = row_number()) %>%
  ungroup()

ggplot(plot_data %>% filter(topic %in% c(9,18,20)), aes(x = word_rank, y = beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, nrow = 1, strip.position = "bottom") +
  geom_text(aes(label = term), vjust = -0.25, size = 3.5) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Word Rank", y = "Beta") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank())


perplexity(lda_media)


lda_media_gamma <- tidy(lda_media, matrix = "gamma")

assigned_topics <- lda_media_gamma %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  ungroup() %>%
  select(document, topic)

colnames(assigned_topics) <- c("document_index", "assigned_topic")

assigned_topics$document_index <- as.numeric(assigned_topics$document_index)
assigned_topics <- inner_join(assigned_topics, all_media_posts)


# Random forest


library(caret)


trainIndex <- createDataPartition(assigned_topics$response_like, p = 0.8, list = FALSE, times = 1)
train_set <- assigned_topics[trainIndex,]
test_set <- assigned_topics[-trainIndex,]

rf_model <- ranger(
  response_like ~ assigned_topic + sentiment + followers + poster_party + assigned_topic + post_type + user_name + post_date + video_view_from_post,
  data = train_set,
  num.trees = 1000,
  importance = "impurity"
)

predictions <- predict(rf_model, data = test_set)
test_set$predicted_response <- predictions$predictions

residuals <- test_set$response_like - test_set$predicted_response
squared_residuals <- residuals^2
mse <- sqrt(mean(squared_residuals))
rmse <- sqrt(mse)
rmse