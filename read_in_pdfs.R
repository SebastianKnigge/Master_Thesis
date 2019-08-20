# Script for reading in pdf files in the given eurostat format
# install.packages("pdftools")
library(dplyr)
library(tidytext)
library(pdftools)
library(ggplot2)

getwd() %>% 
  file.path("TextDocs") %>% 
  setwd()
documents <- list.files()

documents





read_pdf_clean <- function(document){
  # This function loads the document given per name
  # and excludes the stop words inclusive numbers
  pdf1 <- pdf_text(document) %>% 
    strsplit(split = "\n") %>% 
    do.call("c",.) %>% 
    as_tibble() %>%
    unnest_tokens(word,value) %>%
    # apply a filter for ^
    filter(!grepl("ˆ",word))
  # load stopword library
  data(stop_words)
  # add own words to stop word library - here the numbers from 1 to 10
  new_stop_words <- tibble(word=as.character(0:9),
                           lexicon=rep("own",10)) %>%
    bind_rows(stop_words)
  
  pdf1 %>% 
    anti_join(new_stop_words)
}


pdf1 <- read_pdf_clean(documents[1])

plot_most_freq_words <- function(pdf, n=5){
  # plots a bar plot via ggplot
  pdf %>% count(word) %>% arrange(desc(n)) %>% head(n) %>% 
    ggplot(aes(x=word,y=n)) + 
    geom_bar(stat="identity")+
    # no labels for x and y scale
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank())
}

read_pdf_clean(documents[19]) %>% plot_most_freq_words(6)

# inital set up for the corpus
corpus <- tibble(document=1, word=pdf1$word)
# adding the documents iteratively
for (i in 2:length(documents)){
  pdf_i <- read_pdf_clean(documents[i])
  corpus <- tibble(document=i, word=pdf_i$word) %>% bind_rows(corpus,.)
}

dtm <- corpus %>% count(document, word, sort = TRUE) %>%
  select(doc_id=document, term=word, freq=n) %>%
  document_term_matrix()


library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(udpipe)
library(topicmodels)
library(ggplot2)
library(wordcloud2)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(RCurl)
library(XML)


documents_lda <- LDA(dtm, 
                    k = 5, control = list(seed = 1234))

attributes(documents_lda)

ext_gamma_matrix <- function(model){
  # get gamma matrix for chapter probabilities
  chapters_gamma <- tidy(model, matrix = "gamma")

  gamma_per_chapter <- chapters_gamma %>%
    spread(topic, gamma)
  return(gamma_per_chapter)
}

chapters_gamma <- ext_gamma_matrix(documents_lda)

prediction5 <- predict(documents_lda, newdata=dtm, type="topic")$topic

############### wordcloud
plot_wordcloud <- function(corpus, selection="ALL", max.words=25, i, freq="tfidf"){
  # setting up a tibble which returns tfidf and tf and frequency for 
  # the whole corpus
  tfidf <- corpus %>% count(document, word, sort = TRUE) %>% 
    bind_tf_idf(word, document, n)
  # include all documents for selection if selection="ALL"
  if (all(selection=="ALL")) {
    selection <- corpus %>% 
      select(document) %>% 
      unique() %>% 
      unlist() %>% 
      sort()}
  # filter for all selected documents
  # use either ft or tfidf
  if (freq=="tfidf"){
    dtm_selected <- tfidf %>% filter(document%in%selection) %>% 
      select(word, tf_idf) %>% count(word, wt=tf_idf, sort=TRUE)
  } else {
    dtm_selected <- tfidf %>% filter(document%in%selection) %>% 
      select(word, tf) %>% count(word, wt=tf, sort=TRUE)
  }
  wordcloud(words = dtm_selected$word, freq = dtm_selected$n, min.freq = 1,
            max.words=max.words, random.order=FALSE, 
            colors=brewer.pal(8, "Dark2"), scale=c(3,0.2), 
            main="Title", use.r.layout = TRUE)
  text(x=0.5, y=1, paste("Topic", i))
}

prediction5

# compare topic 1 with topic 2

ind1 <- which(prediction5==1)
ind2 <- which(prediction5==2)
ind3 <- which(prediction5==3)
ind4 <- which(prediction5==4)
ind5 <- which(prediction5==5)

par(mfrow=c(2,3))
par(mar=c(1,1,1,1))
plot_wordcloud(corpus, selection=ind1, i=1)
plot_wordcloud(corpus, selection=ind2, i=2)
plot_wordcloud(corpus, selection=ind3, i=3)
plot_wordcloud(corpus, selection=ind4, i=4)
plot_wordcloud(corpus, selection=ind5, i=5) 

library(stargazer)
data.frame(Group=i, Doc=get(paste0("ind",i))) %>%
  stargazer(summary=FALSE, header = F, rownames=FALSE)


