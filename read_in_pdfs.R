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
    unnest_tokens(word,value)
  # load stopword library
  data(stop_words)
  # add own words to stop word library - here the numbers from 1 to 10
  new_stop_words <- tibble(word=as.character(1:10),lexicon=rep("own",10)) %>%
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

plot_most_freq_words(pdf1,6)

corpus <- tibble(document=1, word=pdf1$word)

for (i in 2:length(documents)){
  pdf_i <- read_pdf_clean(documents[i])
  corpus <- tibble(document=i, word=pdf_i$word) %>% bind_rows(corpus,.)
}

dtm <- corpus %>% count(document, word, sort = TRUE) %>%
  select(doc_id=document, term=word, freq=n) %>%
  document_term_matrix()



