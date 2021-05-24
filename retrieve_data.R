# Prep work: load package and get token -----------------------------------
library(geniusr)
library(dplyr)
library(tidytext)
library(data.table)
library(beepr)
library(parallel)
library(stringr)
library(ggplot2)
# install.packages("RColorBrewer")
library(RColorBrewer)
# install.packages('wordcloud2')
library(wordcloud)
# install.packages("tm")
library(tm)
library(tidyr)
library(ggraph)
library(igraph)
library(syuzhet)
library(widyr)

setwd("~/Desktop/DS3")
Sys.setenv(GENIUS_API_TOKEN = readLines("genius_api_token"))
genius_token()


# Retrieve data from Genius API -------------------------------------------
song_df <- search_artist("taylor swift")$artist_id %>% 
  get_artist_songs_df() %>% distinct(song_id, .keep_all = TRUE)

song_df$song_id <- as.character(song_df$song_id)
str(song_df)
# write.csv(song_df, file = "song_df.csv")

process_album  <- function(song_id) {
  song <- get_song(song_id = song_id)
  album_list <- list()
  album_list[['song_id']] <- song_id
  album_list[['album']] <- song$content$album$name
  # album_list[['album_id']] <- song$content$album$id
  album_list[['release']] <- song$content$release_date
  return(album_list)
}

numCores <- detectCores() ## you can also do detectCores() - 1 to preserve one core other tasks while this runs!


album_df <- rbindlist(mclapply(c(song_df$song_id), process_album, mc.cores = numCores), fill = T)
# write.csv(album_df, file = "album_df.csv")

song_all <- merge(song_df, album_df, by= "song_id", all.x = T)

process_lyrics_url <- function(lyrics_url){
  lyrics_list <- list()
  lyrics_list <- get_lyrics_url(song_lyrics_url= lyrics_url)
}

lyrics_df_url <- rbindlist(mclapply(song_df$song_lyrics_url, process_lyrics_url, mc.cores = numCores))
# write.csv(lyrics_df_url, file = "lyrics_df_url.csv")

process_lyrics_id <- function(id){
  lyrics_list <- list()
  lyrics_list <- get_lyrics_id(song_id = id)
}

lyrics_df_id <- rbindlist(mclapply(c(song_df$song_id), process_lyrics_id, mc.cores = numCores))
# write.csv(lyrics_df_id, file = "lyrics_df_id.csv")

lyrics_df_1 <- merge(lyrics_df_id, song_all,  by= 'song_id', all.x = T)

lyrics_df_2 <- merge(lyrics_df_url,song_all,  by= 'song_lyrics_url', all.y = T) %>% data.table()

# write.csv(lyrics_df_1, 'lyrics_df_1.csv')
# write.csv(lyrics_df_2, 'lyrics_df_2.csv')

lyrics_df_1 <- lyrics_df_1[complete.cases(line),,]

lyrics_df_2 <- lyrics_df_2[complete.cases(line),,]

lyrics_df_1 <- lyrics_df_1 %>% select(order(colnames(lyrics_df_1)))
lyrics_df_2 <- lyrics_df_2 %>% select(order(colnames(lyrics_df_2)))

lyrics_df_1 <- lyrics_df_1[,-c(15:17)]
lyrics_df_2 <- lyrics_df_2[,-c(15:17)]

all_equal(lyrics_df_1, lyrics_df_2)

table(colnames(lyrics_df_1) == colnames(lyrics_df_2) )

do.call(paste0, lyrics_df_1) %in% do.call(paste0, lyrics_df_2) %>% table()
# diff <- setdiff(lyrics_df_1,lyrics_df_2)

table(lyrics_df_1$line %in% lyrics_df_2$line)
table(lyrics_df_1$song_id %in% lyrics_df_2$song_id)

differ <- subset(lyrics_df_1, !(lyrics_df_1$song_id %in% lyrics_df_2$song_id))

lyrics_df <- rbind(lyrics_df_2,differ)

write_csv2(lyrics_df, "lyrics_df.csv")








