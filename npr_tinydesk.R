library(tuber); library(dplyr); library(readr)
library(stringr)

# Go to your Google API Console, set up an app, and create a text file with
# the App ID in the first line, and the Secret in the 2nd line

path_to_keyfile <- "../google_api_key.txt"
app_id <- read_lines(path_to_keyfile)[1]
app_secret <- read_lines(path_to_keyfile)[2]
# The tuber::yt_oauth function will authenticate w/ Google API in your browser, and
# created a cached .http.oauth file 
yt_oauth(app_id = app_id, app_secret = app_secret)
rm(app_id, app_secret)

# Grab the Tiny Desk Concert videos from the NPR Music channel with tuber::yt_search
tinydesk_orig <- yt_search(channel_id = "UC4eYXhJI4-7wSWc8UNRwD4A", term = "Tiny")
# Data transformation
tinydesk <- tinydesk_orig %>%
  filter(str_detect(title, "Tiny Desk Concert")) %>% 
  mutate_if(is.factor, as.character) %>%
  select(-contains("thumbnails"), -description, -channelTitle, 
         -liveBroadcastContent, -channelId) %>%
  mutate(publish_date = as.Date(str_sub(publishedAt, 1, 10))) %>%
  distinct()

# For each video, lookup stats with tuber::get_stats, and save into a list of 1-row data frames
video_stats <- vector(mode = "list", length = nrow(tinydesk))
for(i in seq_along(video_stats)){
  video_stats[[i]] <- get_stats(tinydesk$video_id[i]) %>% bind_cols() %>%
    mutate_at(vars(viewCount:commentCount), as.integer)
}

# Join the video stats to the tinydesk data frame (the dplyr::bind_rows call collapses the
# list of data frames into one data frame)
tinydesk <- tinydesk %>%
  left_join(video_stats %>% bind_rows(), by = c("video_id" = "id")) %>% 
  mutate(subject = 
           str_extract(title, 
                       "^.+(?=(\\: NPR Music)|( NPR Music)|(\\: Tiny Desk)|(\\: (An )?NPR Tiny))"),
         link = paste0("https://www.youtube.com/watch?v=", video_id)) %>%
  arrange(desc(viewCount)) %>%
  select(-favoriteCount, -publishedAt)

# Write to csv file (it's sorted with the videos w/ most views at the top)
write_csv(tinydesk, "NPR Music - Tiny Desk Videos.csv")

# DATA VISUALIZATION ----
library(ggplot2); library(scatterD3)
scatterD3(
  data = tinydesk %>% mutate(publish_year = as.integer(year(publish_date))), 
  x = likeCount, y = dislikeCount, url_var = link, 
  lines = data.frame(slope = lm1$coefficients[2], intercept = lm1$coefficients[1]), 
  tooltip_text = paste("<strong>Title: </strong>", tinydesk$title, "<br>", 
                       "<strong>Publish Date: </strong>", tinydesk$publish_date, "<br>", 
                       "<strong>Views: </strong>", 
                       sapply(tinydesk$viewCount, scales::comma), "<br>", 
                       "<strong>Likes: </strong>", tinydesk$likeCount, "<br>", 
                       "<strong>Dislikes: </strong>", tinydesk$dislikeCount, "<br>", 
                       "(click point to view video in new tab)"), 
  col_var = publish_year)