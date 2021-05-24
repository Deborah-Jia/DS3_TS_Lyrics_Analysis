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

lyrics_df_2 <- merge(lyrics_df_url,song_all,  by= 'song_lyrics_url', all.y = T)

# data cleaning -----------------------------------------------------------

# there are speeches, interviews, World Tour Dates and the forewords of songs, delete them.

drop_names <- c("speech\\d*", "foreword", "Forward",'interview',"Liner Notes","The Power of Pop",'Intro','tour',"Voice Memo","Setlist", "Album", "call", "Future of Music", "Before Turning 30","Booklet")

lyrics_df_2[, c("artist_name.x", 'song_lyrics_url', 'artist_id', 'artist_name.y', 'artist_url'):=NULL]

lyrics_df_2 <- lyrics_df_2[!grep(paste(drop_names,collapse="|"), song_name.y, ignore.case = TRUE),,]

# remove white space of all column
lyrics_df_2$album <- str_trim(lyrics_df_2$album)


names(table(lyrics_All$section_artist))
table(nchar(lyrics_All$section_name))


write.csv(lyrics_df_2, 'lyrics_df_2.csv')

# lyrics 1 and 2 cleaning  and merging ------------------------------------
lyrics_All <- fread('lyrics_df_2.csv')


# data visualization ------------------------------------------------------

# extract collaborator names from section singer
names(table(lyrics_All$section_artist))

lyrics_All$collaborator <- str_replace_all(lyrics_All$section_artist, 
                                           c("[^a-zA-Z0-9\\s]|Taylor Swift|Bad Blood|Should've Said No|Comes Around|TIWWCHNT|New Year's Day|WANEGBT|What Goes Around|Long Live|Clean|Youre Not Sorry|and|with|Both" = "",
                                             "Brendon Urie Brendon Urie & Brendon Urie   Brendon Urie" = "Brendon Urie")) %>% 
  str_trim()

names(table(lyrics_All$collaborator))
# %>% str_squish()
# make empty cells NA 
is.na(lyrics_All) <- lyrics_All == ''
sort(table(lyrics_All$collaborator))

lyrics_All$len_line <- str_count(lyrics_All$line, '\\w+')

lyrics_All[, len_song := sum(len_line), by = song_id]

lyrics_All <- lyrics_All[complete.cases(len_song),,]

lyrics_All$section_name <- gsub('\\d', '',lyrics_All$section_name) %>% str_squish() 
names(table(lyrics_All$section_name))

lyrics_All[complete.cases(collaborator), .(count =.N), by = collaborator][1:10] %>% 
  ggplot(aes(x= collaborator, y= count, fill= count)) +
  geom_col() +
  coord_flip() +
  aes(x=reorder(collaborator,count)) +
  scale_fill_gradient(low='pink', high="#0089A7")+
  xlab("") + ylab("")+ 
  theme_light() +
  theme(legend.position = "none") 

# which album/song has most words? 

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
  sentim_scores <- sentim_scores %>% mutate(sentiment = rownames(sentim_scores)) %>% rename(Score = colSums.sentim_scores.....)
  
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
  # 1.1 Pre-processing text
  library(stringr)
  
  # must occur after the first occurrence of an empty line,
  
  library(tidytext)
  cleaned_text <- lyrics_All[complete.cases(line, album, song_id), .(song_id, album, line),] %>% tibble()
  
  TS_words <- cleaned_text %>%
    unnest_tokens(word, line) %>%
    filter(str_detect(word, "[a-z']$"),
           !word %in% stop_words$word)

  #Words in albums
  TS_words %>%
    count(word, sort = TRUE)

  (words_by_album <- TS_words %>%
    count(album, word, sort = TRUE) %>%
    ungroup())
  
#1.2.1 Finding tf-idf within albums
  tf_idf <- words_by_album %>%
    bind_tf_idf(word, album, n) %>%
    arrange(desc(tf_idf))
  
  tf_idf

  st_album <- "Taylor Swift|Fearless|speak now|red|1989|reputation|lover|Folklore|evermore"
  
  tf_idf %>%
    filter(str_detect(album, regex(st_album, ignore_case = T))) %>%
    group_by(album) %>%
    slice_max(tf_idf, n = 12) %>%
    ungroup() %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(tf_idf, word, fill = album)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ album, scales = "free") +
    labs(x = "tf-idf", y = NULL)  

  # study the correlation: remove more albums as current result doesn't make much sense
  library(widyr)
  
  album_cors <- words_by_album %>%
    pairwise_cor(album, word, n, sort = TRUE)
  
  album_cors

  # filter for stronger correlations among albums
  library(ggraph)
  library(igraph)
  set.seed(2022)
  
  album_cors %>%
    filter(correlation > .4) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(alpha = correlation, width = correlation)) +
    geom_node_point(size = 6, color = "lightblue") +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()

  #1.2.2 Topic modeling
  # include only words that occur at least 50 times
  word_albums <- TS_words %>%
    filter(str_detect(album, regex(st_album, ignore_case = T))) %>%
    group_by(word) %>%
    mutate(word_total = n()) %>%
    ungroup() %>%
    filter(word_total > 50)
  
  # convert into a document-term matrix
  # with document names such as 
  TS_dtm <- word_albums %>%
    unite(document, album, song_id) %>%
    count(document, word) %>%
    cast_dtm(document, word, n)

  library(topicmodels)
  TS_lda <- LDA(TS_dtm, k = 9, control = list(seed = 2022))  

  TS_lda %>%
    tidy() %>%
    group_by(topic) %>%
    slice_max(beta, n = 8) %>%
    ungroup() %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()  
  
  TS_lda %>%
    tidy(matrix = "gamma") %>%
    separate(document, c("album", "song_id"), sep = "_") %>%
    mutate(album = reorder(album, gamma * topic)) %>%
    ggplot(aes(factor(topic), gamma)) +
    geom_boxplot() +
    facet_wrap(~ album) +
    labs(x = "Topic",
         y = "# of messages where this was the highest % topic")  

  # Sentiment analysis: Sentiment analysis by word:
  library("textdata")
  contributions <- TS_words %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(value))
  
  contributions
# Which words had the most effect on sentiment values overall (Figure 9.7)?
  contributions %>%
    slice_max(abs(contribution), n = 25) %>%
    mutate(word = reorder(word, contribution)) %>%
    ggplot(aes(contribution, word, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    labs(y = NULL)

  # word by album
  top_sentiment_words <- words_by_album %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    mutate(contribution = value * n / sum(n))
  
  top_sentiment_words

  top_sentiment_words %>%
    filter(str_detect(album, regex(st_album, ignore_case = T))) %>%
    group_by(album) %>%
    slice_max(abs(contribution), n = 12) %>%
    ungroup() %>%
    mutate(album = reorder(album, contribution),
           word = reorder_within(word, contribution, album)) %>%
    ggplot(aes(contribution, word, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    scale_y_reordered() +
    facet_wrap(~ album, scales = "free") +
    labs(x = "Sentiment value * # of occurrences", y = NULL)  

  ### Sentiment analysis by lyrics lines
  cleaned_text[str_detect(line, "\\w+"), sentiment := get_sentiment(lyric_column), ]

  