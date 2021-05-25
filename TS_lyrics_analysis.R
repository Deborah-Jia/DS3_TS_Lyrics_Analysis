library(geniusr)
library(dplyr)
library(tidytext)
library(data.table)
library(beepr)
library(parallel)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(tidyr)
library(ggraph)
library(igraph)
library(syuzhet)
library(widyr)
library(widyr)
library(tidyr)

# data cleaning -----------------------------------------------------------
setwd("~/Desktop/DS3")
lyrics_All <- fread('lyrics_df.csv')

# remove useless columns
lyrics_All[, c("artist_name.x", 'annotation_count','artist_id', 'artist_name.y', 'section_name','artist_url'):=NULL]
str(lyrics_All)

# there are speeches, interviews, World Tour Dates and the forewords of songs, delete them.
drop_names <- c("speech\\d*", "foreword", "Forward",'interview',"Liner Notes",'Intro','tour',"Voice Memo","Setlist", "Album", "phone call", "Future of Music", "Before Turning 30", "power of pop",
                "Booklet", "Tribute", "Prologue")

lyrics_All <- lyrics_All[!grep(paste(drop_names,collapse="|"), song_name.y, ignore.case = TRUE),,]

# change album name "Red (Deluxe Edition)", "1989 (Deluxe)",  to "Red" and "1989", as it is the only album available
lyrics_All$album <-replace(lyrics_All$album, lyrics_All$album == "Red (Deluxe Edition)", "Red") 
lyrics_All$album <-replace(lyrics_All$album, lyrics_All$album == "1989 (Deluxe)", "1989") 

# remove white space of column album
lyrics_All$album <- str_trim(lyrics_All$album)

# test_final_t <- unique(lyrics_All[complete.cases(album), .(album, song_name.x),by=song_id])[,.N,by=album] 
# test_final_s <- unique(lyrics_All[complete.cases(album), .(album, song_name.x,song_name.y),by=song_id])

# extract collaborator names from section singer
names(table(lyrics_All$section_artist))

lyrics_All$collaborator <- str_replace_all(lyrics_All$section_artist, 
                                           c("[^a-zA-Z0-9\\s]|Taylor Swift|Bad Blood|Should've Said No|Comes Around|TIWWCHNT|New Year's Day|WANEGBT|What Goes Around|Long Live|Clean|and|with|Both" = "",
                                             "Brendon Urie Brendon Urie" = "Brendon Urie", "Youre Not Sorry" = "",
                                             'Brendon Urie   Brendon Urie' = "Brendon Urie")) %>% str_trim()

names(table(lyrics_All$collaborator))

# make empty cells NA 
is.na(lyrics_All) <- lyrics_All == ''
sort(table(lyrics_All$collaborator))

# calculate the length of each lyrics line and each song
lyrics_All$len_line <- str_count(lyrics_All$line, '\\w+')
lyrics_All[, len_song := sum(len_line), by = song_id]

# remove songs that are empty and shorter than 60 (64 is her shotrtest song)
lyrics_All <- lyrics_All[complete.cases(len_song),,]
lyrics_All <- lyrics_All[len_song> 60,,]


# data visualization ------------------------------------------------------
# who is Taylor's most dedicated collaborator
lyrics_All[complete.cases(collaborator), .(count =.N), by = collaborator][1:10] %>% 
  ggplot(aes(x= collaborator, y= count, fill= count)) +
  geom_col() +
  coord_flip() +
  aes(x=reorder(collaborator,count)) +
  scale_fill_gradient(low='pink', high="#0089A7")+
  xlab("") + ylab("")+ 
  theme_light() +
  theme(legend.position = "none") 


# How does the distribution of word count in all her songs?
unique(lyrics_All[, .(len_song, song_name.y), by = song_id]) %>% 
  ggplot(aes(len_song)) +
  geom_histogram(bins=30,aes(fill = ..count..))+
  geom_vline(aes(xintercept=mean(len_song)),
             color='#FFFFFF', linetype='dashed', size=1) +
  geom_density(aes(y= 28* ..count..),alpha=.3, fill='#7DB9DE') +
  ggtitle('Distribution of word count') +
  theme_minimal()+xlab("") + ylab("")+ 
  theme(legend.position = "none") 

# which album/song has most words? 
st_album <- c("Taylor Swift", "Fearless", "Speak Now", "Red", "1989", "reputation","Lover","Folklore","evermore")

#SONG
unique(lyrics_All[, .(len_song, song_name.x), by = song_id])[order(-len_song)][1:12] %>% 
  ggplot(aes(x= song_name.y, y=len_song, fill = len_song)) +
  scale_fill_gradient(low='#999999', high="#E69F00")+ 
  geom_bar(stat='identity') +
  coord_flip() +
  aes(x=reorder(song_name.x,len_song)) +
             theme_minimal() +
  xlab("") + ylab("")+ 
  theme(legend.position = "none") 

# album
unique(lyrics_All[(album %in% st_album) & complete.cases(album), .(len_album = sum(len_line)), by = album])[order(-len_album)] %>% 
  ggplot(aes(x= album, y=len_album, fill = len_album)) +
  scale_fill_gradient(low='#D7C4BB', high="#86C166")+ 
  geom_bar(stat='identity') +
  coord_flip() +
  aes(x=reorder(album,len_album)) +
  theme_minimal() + 
  xlab("") + ylab("")+ 
  theme(legend.position = "none")
          
# How does the word count change following the release time?
unique(lyrics_All[complete.cases(release), .(len_song, year = substring(release, 1, 4)), by = song_id]) %>% 
  ggplot(aes(x= factor(year),y=len_song, group = 1)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x, colour = "#DB4D6D")+
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
# Removing English common stop words
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

# Most common positive and negative words
tidy_lyrics <- lyrics_All[complete.cases(line, album, song_id), .(song_id, album, line),] %>% unnest_tokens(word, line)


bing_word_counts <- tidy_lyrics %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


# bigrams and n-grams analysis

# Counting and correlating among albums
bigrams_words <- lyrics_All[complete.cases(line, album, song_id), .(song_id, album, line),] %>%  unnest_tokens(bigram, line, token = "ngrams", n = 2)

bigrams_separated <- bigrams_words %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE) 

library(igraph)

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame()

bigram_graph
set.seed(2021)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# n-grams


# we need to filter for at least relatively common words first
word_cors <- bigrams_words %>%
  group_by(bigram) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, album, sort = TRUE)

set.seed(2021)

word_cors %>%
  filter(correlation > .8) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# N-gram analysis
cleaned_text <- lyrics_All[album %in% st_album, .(song_id, album, line),] %>% filter(str_detect(line, "^[^>]+[A-Za-z\\d]") | line == "") 

 n_bigrams <- cleaned_text %>%
  unnest_tokens(bigram, line, token = "ngrams", n = 2)

usenet_bigram_counts <- n_bigrams %>%
  count(album, bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

negate_words <- c("not", "no", "can't", "don't", "won't", "never")

usenet_bigram_counts %>%
  filter(word1 %in% negate_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 10) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word2, contribution, word1)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_y_reordered() +
  labs(x = "Sentiment value * # of occurrences",
       y = "Words preceded by a negation")

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
  cleaned_text %>% filter(str_detect(line, "\\w+")) %>% mutate(sentiment = get_sentiment(line)) %>% 
    group_by(sentiment < 0) %>%
    slice_max(abs(sentiment), n = 15) %>% 
    ungroup() %>%
    mutate(line = reorder(line, sentiment)) %>%
    ggplot(aes(line, sentiment, fill = sentiment < 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() 
  
  
# topic modeling
  # 1.1 Pre-processing text
  library(stringr)
  
  # must occur after the first occurrence of an empty line,
  
  library(tidytext)
  clean_text <- lyrics_All[complete.cases(line, album, song_id), .(song_id, album, line),] %>% tibble()
  
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

    

  