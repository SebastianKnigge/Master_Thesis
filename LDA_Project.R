###########################################################
### --------------------------------------------------- ###
### ------------------ LDA with books ----------------- ###
### --------------------------------------------------- ###

# Chapter classificaion with books


library(gutenbergr)
library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(wordcloud)
library(withr)
# install.packages("udpipe")
library(udpipe)

# sample from the whole library
set.seed(1278)
books <- gutenberg_works() %>% 
  # select works with title
  filter(!is.na(title)) %>% 
  # set the sample sitze
  sample_n(50) %>%
  # set a special download link
  gutenberg_download(mirror = "http://mirrors.xmission.com/gutenberg/")

get_titles_dict <- function(books=books){
  # function to get the titles of the books and 
  # ti get the matching gutenberg IDs
  # books ... input is a tibble of the 
  #            gutenberg library
  # output list
  # gutenberg_ids ... named vector of the ids
  # titles .......... vector of titles 
  # len ............. length of the books red in
  gutenberg_ids <- books %>% 
    select(gutenberg_id) %>% 
    unique() %>% 
    unlist()
  titles <- gutenberg_works() %>% 
    filter(gutenberg_id %in% gutenberg_ids) %>% 
    select(title) %>% 
    unlist() %>% 
    as.vector() 
  # shorten very long book titles by setting 
  # a subset of characters of the first line
  # of the title
  sub_inds <- titles %>% 
    regexpr(pattern="\\n|\\r")-1
  sub_inds[sub_inds<0] <- nchar(titles)[sub_inds<0]
  titles <- titles %>% 
    substr(1,sub_inds)
  names(gutenberg_ids) <- titles
  ret <- list(
    gutenberg_ids=gutenberg_ids,
    titles=titles,
    len=length(titles))
  return(ret)
}

titles <- get_titles_dict(books)
titles$titles

by_chapter <- books %>%
  group_by(gutenberg_id) %>%
  # split in chapters
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  # exclude chapter 0
  filter(chapter > 0) 

 get_len_after <- function(by_chapter, len_before=titles$len){
   len <- by_chapter %>% 
     select(gutenberg_id) %>% 
     unique() %>% 
     nrow()
   if(len_before!=len) warning(paste("--- ",len_before-len, " books have 0 chapters --- "))
   len
   }

<<<<<<< HEAD
len_after <- get_len_after(by_chapter=by_chapter)
=======
len_after <- get_len_after(by_chapter)
>>>>>>> 6ed4319414734492c5e7a0d767d033790d2ebf3c


by_chapter <- by_chapter %>%
  # unite chapter and document title
  unite(document, gutenberg_id, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# import tibble stop words
data(stop_words)

# find document-word counts
word_counts <- by_chapter_word %>%
  # exclude stop words
  anti_join(stop_words) %>%
  # count each word by chapter
  count(document, word, sort = TRUE) %>%
  ungroup()

# vocabulary
vocab <- word_counts %>% 
  select(word) %>%
  unique() %>%
  c() %>% unlist()

# Term Document Matrix
chapters_dtm <- word_counts %>%
  # get the right format for
  # document_term_matrix
  rename(doc_id=document,
         term=word,
         freq=n) %>%
  document_term_matrix(vocab)



# use LDA to find the documents 
# k=4 since we have 4 books
chapters_lda <- LDA(chapters_dtm, k = len_after, control = list(seed = 1234), method="Gibbs")
chapters_lda


<<<<<<< HEAD
# test data split
test_data <- word_counts %>%
  filter(document=="18240_16") %>%
  # get the right format for
  # document_term_matrix
  rename(doc_id=document,
         term=word,
         freq=n) %>%
  document_term_matrix(vocab)

predict(chapters_lda, type="topics", newdata=chapters_dtm)

















=======
>>>>>>> 6ed4319414734492c5e7a0d767d033790d2ebf3c
# most frequent terms
{
# exclude beta matrix
chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

# find top 5 words for each topic
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


# plot top frequent terms via beta matrix
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  # group for topic
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  # plot for topics
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


{
  par(mfrow=c(2,2),mar=c(2,1,1,1)+0.1)
  chapter_topics %>%
    group_by(topic) %>%
    filter(topic==1) %>%
    with(wordcloud(term, beta, max.words = 10))
  chapter_topics %>%
    group_by(topic) %>%
    filter(topic==1) %>%
    with(wordcloud(term, beta, max.words = 10))
  chapter_topics %>%
    group_by(topic) %>%
    filter(topic==3) %>%
    with(wordcloud(term, beta, max.words = 10))
  chapter_topics %>%
    group_by(topic) %>%
    filter(topic==4) %>%
    with(wordcloud(term, beta, max.words = 10))
}
}



# get gamma matrix for chapter probabilities
chapters_gamma <- tidy(chapters_lda, matrix = "gamma")

# split joint name of book and chapter
chapters_gamma <- chapters_gamma %>%
  separate(document, c("gutenberg_id", "chapter"), sep = "_", convert = TRUE)
chapters_gamma

# get matrix with probabilities for each topic per chapter
gamma_per_chapter <- chapters_gamma %>%
  spread(topic, gamma)
# rounded results
cbind(gamma_per_chapter[,1:2], round(gamma_per_chapter[,3:ncol(gamma_per_chapter)], 2))



# reorder titles in order of topic 1, topic 2, etc before plotting
chapters_gamma %>%
  # order from index topic 1 to topic 4
  mutate(title = reorder(gutenberg_id, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  # single plot for each title
  facet_wrap(~ title)

#First we’d find the topic that was most associated with 
# each chapter using top_n(), which is effectively the 
# “classification” of that chapter
chapter_classifications <- chapters_gamma %>%
  group_by(gutenberg_id, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()
chapter_classifications

# We can then compare each to the “consensus” 
# topic for each book (the most common topic among its chapters), 
# and see which were most often misidentified.
book_topics <- chapter_classifications %>%
  count(gutenberg_id, topic) %>%
  group_by(gutenberg_id) %>%
  # just keep the most frequent one
  top_n(1, n) %>%
  ungroup() %>%
  # keep title called census and topic
  transmute(consensus = gutenberg_id, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  # missmatches
  filter(gutenberg_id != consensus)%>%
  nrow()/nrow(chapter_classifications)











