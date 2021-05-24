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

genius_token()


# Retrieve data from Genius API -------------------------------------------

song_df <- search_artist("taylor swift")$artist_id %>% 
  get_artist_songs_df() %>% distinct(song_id, .keep_all = TRUE)

song_df$song_id <- as.character(song_df$song_id)
str(song_df)

process_album  <- function(song_id) {
  song <- get_song(song_id = song_id)
  album_list <- list()
  album_list[['song_id']] <- song_id
  album_list[['album']] <- song$content$album$name
  album_list[['release']] <- song$content$release_date
  return(album_list)
}

numCores <- detectCores() ## you can also do detectCores() - 1 to preserve one core other tasks while this runs!

x <- data.frame(id= c(50968,3774,187089))
album_df_0 <- rbindlist(mclapply(x$id, process_album, mc.cores = numCores), fill = T)
album_df_1 <- rbindlist(mclapply(x$id, process_album, mc.cores = numCores), fill = T)

album_df<- rbindlist(mclapply(song_df$song_id, process_album, mc.cores = numCores), fill = T)

# test <-  rbindlist(mclapply(song_df$song_id, process_album, mc.cores = numCores), fill = T)
# all.equal(album_df, test)

song_all <- merge(song_df, album_df, by= "song_id")

process_lyrics <- function(id){
  lyrics_list <- list()
  lyrics_list <- get_lyrics_id(song_id = id)
}

lyrics_df <- rbindlist(mclapply(song_df$song_id, process_lyrics, mc.cores = numCores))
# lyrics_df_1 <- rbindlist(mclapply(song_df$song_id, process_lyrics, mc.cores = numCores), fill = T)
# lyrics_df_2 <- rbindlist(mclapply(song_df$song_id, process_lyrics, mc.cores = numCores), use.names = T)


# test_final <- merge(lyrics_df, song_df, by= "song_id",all.x=TRUE)
lyrics_All <- merge(lyrics_df, song_all, by= "song_id",all.x=TRUE)
str(lyrics_All)

table(lyrics_All$song_name.x == lyrics_All$song_name.y) # F:T 2320:9112 

# data cleaning -----------------------------------------------------------


# lyrics_All[!(lyrics_All$song_name.x %in% lyrics_All$song_name.y)][nchar(song_name.x) < nchar(song_name.y), .(x= song_name.x, y=song_name.y),by= 'song_id']
# song names from column x are shorter than y. so we choose column y
# long names usually come with other singer, the special versions (delux, etc.) of the song. column could help us better merger the same songs.
# notice that we have some clashes between x and y song names, so we check and correct them.

#delete useless columns
lyrics_All[, c("artist_name.x", 'song_lyrics_url', 'artist_id', 'artist_name.y', 'artist_url'):=NULL]
# keep_col <- c("song_id", "line","section_name", 'song_name.x','song_name.y', 'annotation_count','release', 'album')

# there are speeches, interviews, World Tour Dates and the forewords of songs, delete them.

drop_names <- c("speech\\d*", "foreword", "Liner Notes","The Power of Pop",'Intro','tour',"Voice Memo","Setlist", "Album", "call", "Future of Music")
lyrics_All <- lyrics_All[!grep(paste(drop_names,collapse="|"), song_name.y, ignore.case = TRUE),,]

# remove white space of all column
lyrics_All$album <- str_trim(lyrics_All$album)

names(table(lyrics_All$section_artist))
table(nchar(lyrics_All$section_name))

# drop_names <- "speeches|interview|forward|foreword"

test_final_t <- unique(lyrics_All[complete.cases(album), .(album, song_name.x),by=song_id])[,.N,by=album] 

test_final_s <- unique(lyrics_All[complete.cases(album), .(album, song_name.x,song_name.y),by=song_id])

test_final_s[album=='Taylor Swift',]

# data visualization ------------------------------------------------------

# extract collaborator names from section singer
names(table(lyrics_All$section_artist))

lyrics_All$collaborator <- str_replace_all(lyrics_All$section_artist, 
                                           c("[^a-zA-Z0-9\\s]|Taylor Swift|Bad Blood|Should've Said No|TIWWCHNT|New Year's Day|WANEGBT|Long Live|Clean|and|with|Both" = "")) %>% 
  str_trim()

names(table(lyrics_All$collaborator))
# %>% str_squish()
# make empty cells NA 
is.na(lyrics_All) <- lyrics_All == ''
sort(table(lyrics_All$collaborator))

lyrics_All[, .(count =.N), by = collaborator][complete.cases(collaborator)] %>% 
  ggplot(aes(x= collaborator, y= count, fill= count)) +
  geom_col() +
  coord_flip() +
  aes(x=reorder(collaborator,count)) +
  scale_fill_gradient(low='pink', high="#0089A7")+
  xlab("") + ylab("")+ 
  theme_light() +
  theme(legend.position = "none") 

# which album/song has most words? 
lyrics_All$len_line <- str_count(lyrics_All$line, '\\w+')

lyrics_All[, len_song := sum(len_line), by = song_id]

# test <- unique(lyrics_All[, .(len_song = sum(len_line), song_name.y), by = song_id])[order(-len_song)][1:30]

unique(lyrics_All[, .(len_song, song_name.x), by = song_id])[order(-len_song)][1:12] %>% 
  ggplot(aes(x= song_name.y, y=len_song, fill = len_song)) +
  scale_fill_gradient(low='#999999', high="#E69F00")+ 
  geom_bar(stat='identity') +
  coord_flip() +
  aes(x=reorder(song_name.x,len_song)) +
             theme_minimal() +
  xlab("") + ylab("")+ 
  theme(legend.position = "none") 


# test <- unique(lyrics_All[, .(len_song = sum(len_line), song_name.y), by = song_id])[order(-len_song)][1:30]

unique(lyrics_All[complete.cases(album), .(len_album = sum(len_line)), by = album])[order(-len_album)][1:15] %>% 
  ggplot(aes(x= album, y=len_album, fill = len_album)) +
  scale_fill_gradient(low='#D7C4BB', high="#86C166")+ 
  geom_bar(stat='identity') +
  coord_flip() +
  aes(x=reorder(album,len_album)) +
  theme_minimal() + 
  xlab("") + ylab("")+ 
  theme(legend.position = "none") 

# https://www.youtube.com/watch?v=SVY8I46dkb0            
# How does the word count change following the release time?


unique(lyrics_All[complete.cases(release), .(len_song, year = substring(release, 1, 4)), by = song_id]) %>% 
  ggplot(aes(x= factor(year),y=len_song, group = 1)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x, colour = "#DB4D6D")+
  theme_minimal() 

# How does the distribution of word count in all her songs?
unique(lyrics_All[, .(len_song, song_name.y), by = song_id]) %>% 
  ggplot(aes(len_song)) +
  geom_histogram(bins=30,aes(fill = ..count..))+
  geom_vline(aes(xintercept=mean(len_song)),
             color='#FFFFFF', linetype='dashed', size=1) +
             geom_density(aes(y= 46* ..count..),alpha=.3, fill='#7DB9DE') +
  ggtitle('Distribution of word count') +
  theme_minimal()+xlab("") + ylab("")+ 
  theme(legend.position = "none") 

# which section accounts for the largest part of her songs?
lyrics_All$section_name <- gsub('\\d', '',lyrics_All$section_name) %>% str_squish() 
names(table(lyrics_All$section_name))

# test <- lyrics_All[, .(n_section = .N),by= section_name][order(-n_section)]
lyrics_All[, .(n_section = .N),by= section_name][order(-n_section)][1:10] %>% 
  ggplot(aes(x= section_name, y= n_section, fill = n_section)) +
  scale_fill_gradient(low='#999999', high="#E69F00")+ 
  geom_bar(stat='identity') +
  coord_flip() +
  aes(x=reorder(section_name,n_section)) +
  theme_minimal() 
  
# word cloud
#Create a vector containing only the text
text <- unique(lyrics_All$line)
# Create a corpus  
docs <- Corpus(VectorSource(text)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
# Converting the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Removing english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# creating term document matrix
dtm <- TermDocumentMatrix(docs) 
# defining tdm as matrix
matrix <- as.matrix(dtm) 
# getting word counts in decreasing order
words <- sort(rowSums(matrix),decreasing=TRUE) 
# creating a data frame with words and their frequencies
dt <- data.table(word = names(words),freq=words)

set.seed(1233) # for reproducibility 
wordcloud(words = dt$word, freq = dt$freq,
          min.freq = 1,scale=c(1.5,.8),
          max.words=300, random.order=FALSE,rot.per=0.15,
          colors=brewer.pal(8,"Paired"))

# bigrams

lyrics_bigrams <- lyrics_All%>%
  unnest_tokens(bigram, line, token = 'ngrams', n = 2) %>%
  separate(bigram, c('word1', 'word2'), sep = ' ') %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>% drop_na()

head(lyrics_bigrams, 10)

# draw a plot 
  set.seed(2021)
  arrow <- grid::arrow(type = 'closed', length = unit(.15, 'inches'))
  
  lyrics_bigrams[n > 3&!str_detect(word1, '\\d')&!str_detect(word2, '\\d'),,] %>%
    graph_from_data_frame() %>%
    ggraph(layout = 'fr') +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = arrow) +
    geom_node_point(color = 'lightblue', size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    ggtitle('Network graph of bigrams') +
    theme_void()

# Sentiment Analysis
 # Getting the sentiment value for the lyrics, and add cumulative value of the sentiments to a datatable
  # sentimentscores <- data.table(colSums(get_nrc_sentiment((text))[,]))
  
  sentim_scores <- get_nrc_sentiment((text))
  sentim_scores<- data.frame(colSums(sentim_scores[,]))
  sentim_scores <- sentim_scores %>% mutate(Sentiment = rownames(sentim_scores)) %>% rename(Score = colSums.sentim_scores.....)
  
  # Plot for the cumulative sentiments
  sentim_scores %>% 
    ggplot(aes(x=sentiment,y=Score, fill= Score))+
    geom_bar(stat = 'identity')+
    ggtitle('Total sentiment based on scores')+
    aes(x=reorder(sentiment,Score)) +
    scale_fill_gradient(low='#67B7C2', high="#C26793")+ 
    theme_light() +
    theme(legend.position = 'none')
# topic modeling
 
