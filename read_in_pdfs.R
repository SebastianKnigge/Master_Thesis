# Script for reading in pdf files in the given eurostat format
# install.packages("pdftools")

getwd() %>% 
  file.path("TextDocs") %>% 
  setwd()
documents <- list.files()

documents

library(pdftools)



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
    bind_rows(stop_words, own_stop_words)
  
  pdf1 %>% 
    anti_join(new_stop_words)
  
  return(pdf1)
}


pdf1 <- read_pdf_clean(documents[6])
pdf1 %>% count(word) %>% arrange(desc(n))
