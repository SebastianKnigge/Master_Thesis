###########################################################
### --------------------------------------------------- ###
### ------------------ LDA with books ----------------- ###
### --------- Chapter classificaion with books--- ----- ###
### --------------------------------------------------- ###

# loading packages
library(gutenbergr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(udpipe)
library(topicmodels)
library(ggplot2)

sampling_books <- function(seed=1234, n=20){
  # sample n books from the whole library
  set.seed(seed)
  gutenberg_works() %>% 
    # select works with title
    dplyr::filter(!is.na(title)) %>% 
    # set the sample sitze
    sample_n(n) %>%
    # set a special download link
    gutenberg_download(
      mirror = "http://mirrors.xmission.com/gutenberg/")
}

n_books <- 6
books <- sampling_books(n=n_books, seed=54321)
# good seperation for 4 topics:
# seed=12345
# seed=54321
# for 6 books:
# seed=222
# seed 101
# for 10 books:
# seed=54321
# seed=123456



by_chapter <- books %>%
  group_by(gutenberg_id) %>%
  # split in chapters
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  # exclude books without chapters
  dplyr::filter(chapter > 0) 

shorten_titles <- function(titles){
  # shorten very long book titles by setting 
  # a subset of characters of the first line
  # of the title
  sub_inds <- titles %>% 
    regexpr(pattern="\\n|\\r")-1
  sub_inds[sub_inds<0] <- nchar(titles)[sub_inds<0]
  titles %>% 
    substr(1,sub_inds)
}

get_titles <- function(x, n_books){
  # get the sampled gutenberg_ids
  unique_ids <- x %>% 
    select(gutenberg_id) %>% 
    unique() %>% unlist()
  
  # get the titles
  titles <- gutenberg_works() %>% 
    dplyr::filter(gutenberg_id %in% unique_ids) %>% 
    select(gutenberg_id, title) %>% 
    mutate(title=shorten_titles(title))
  
  # get the number of gutenberg ids
  len <- nrow(titles)
  if(n_books!=len) warning(paste("--- ",n_books-len, 
                                 " books have 0 chapters --- "))
  
  # the output as a list  
  ret <- list(
    titles=titles,
    len=len
  )
  return(ret)
}

titles <- get_titles(by_chapter, n_books)
titles$titles

# append the books matrix until
# we get the desired number of books n_books
n <- titles$len
seed_index <- 1
while (n<n_books) {
  book2add <- sampling_books(n=1, seed=seed_index)
  by_chapter_add <- book2add %>%
    group_by(gutenberg_id) %>%
    # split in chapters
    mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
    ungroup() %>%
    # exclude books without chapters
    dplyr::filter(chapter > 0)
  titles2add <- get_titles(by_chapter_add, 1)
  # adding the book to by_chapter if there are chapters in the 
  # book plus it is not in the data already
  if (titles2add$len==1) if(!titles2add$titles$gutenberg_id%in%titles$titles$gutenberg_id) {
    by_chapter <- bind_rows(by_chapter, by_chapter_add)
  }
  n<-get_titles(by_chapter, n)$len
  seed_index <- seed_index+1
}

# get the titles of the full data set
titles <- get_titles(by_chapter, n_books)
titles$titles

# unite chapter and document title
by_chapter <- by_chapter %>%
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

# get into a format lda can handle
chapters_dtm <- word_counts %>%
  select(doc_id=document, term=word, freq=n) %>%
  document_term_matrix()

# reduce low frequencies
chapters_reduced_format <- chapters_dtm %>%
  dtm_remove_lowfreq(minfreq = 2)

# use LDA to classify the documents
chapters_lda <- LDA(chapters_reduced_format, 
                    k = titles$len, control = list(seed = 1234))
chapters_lda

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
rounded_gamma <- cbind(gamma_per_chapter[,1:2], round(gamma_per_chapter[,3:ncol(gamma_per_chapter)], 3))

#First we’d find the topic that was most associated with 
# each chapter using top_n(), which is effectively the 
# “classification” of that chapter
chapter_classifications <- chapters_gamma %>%
  group_by(gutenberg_id, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

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
book_topics

# check the fraction of missclassification
chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  # missmatches
  dplyr::filter(gutenberg_id != consensus)%>%
  nrow()/nrow(chapter_classifications)



# get pdfs
plotm <- rounded_gamma %>%
  group_by(gutenberg_id)%>%
  # note: pdfs are unnormalized
  summarise_at(2:(titles$len+1), sum)
# plot it
par(mfrow=c(3,2))
for (i in titles$titles$gutenberg_id) barplot(plotm%>%
                                                filter(gutenberg_id==i)%>%
                                                select(2:(titles$len+1)) %>% unlist())
# what is the most likely topic?
plotm %>%
  apply(1, function(x) which.max(x[2:(titles$len+1)]))%>%
  bind_cols(plotm,assign_topic=.)


# how many chapters has each book?
chapter_classifications %>% 
  group_by(gutenberg_id) %>%
  count()


## prediction
make_prediction <- function(lda=chapters_lda, documents){
  index <- which(rownames(chapters_dtm) %in% 
                   documents)
  testdata<- chapters_reduced_format[index,]
  predict(chapters_lda, newdata=testdata, type="topic")
}

# it seems like the evaluation via predict of the model is the same
# as the gamma matrix output
chapters_lda %>% make_prediction(c("8095_13","8095_12","8095_11")) -> N
pred <- N[,6:11] %>% round(3) %>% cbind(N[,1],.)
gammamatrix <- rounded_gamma%>%
  unite(document, gutenberg_id, chapter) %>%
  filter(document%in%c("8095_13","8095_12","8095_11"))
pred; gammamatrix


