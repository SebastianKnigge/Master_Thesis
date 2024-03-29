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

#n_books <- 5
#books <- sampling_books(n=n_books, seed=9119)
# good seperation for 4 topics:
# seed=12345
# seed=54321
# for 6 books:
# seed=222
# seed 101
# for 10 books:
# seed=54321
# seed=123456



set_up_books <- function(n_books=4, seed=1992){
  # initial book sample
  books <- sampling_books(n=n_books, seed=seed)
  by_chapter <- books %>%
    group_by(gutenberg_id) %>%
    # split in chapters
    mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
    ungroup() %>%
    # exclude books without chapters
    dplyr::filter(chapter > 0) 
  return(by_chapter)
}

n_books <- 5
by_chapter <- set_up_books(n_books=n_books, seed=133)



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



append_by_chapter <- function(x=by_chapter, n_books, seed_index=1){
  # append the books matrix until
  # we get the desired number of books n_books

  titles <- get_titles(x, n_books)
  n <- titles$len
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
      x <- bind_rows(x, by_chapter_add)
    }
    n<-get_titles(x, n)$len
    seed_index <- seed_index+1
  }
  return(x)
}

appended_by_chapter <- append_by_chapter(x=by_chapter, n_books = n_books)

# get the titles of the full data set
titles <- get_titles(appended_by_chapter, n_books)
titles$titles

exclude_stop_words <- function(x){
  # unite chapter and document title
  by_chapter_word <- x %>%
    unite(document, gutenberg_id, chapter) %>%
    # split into words
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
  return(word_counts)
}

word_counts <- exclude_stop_words(appended_by_chapter)

convert_to_dtm <- function(x, minfreq=2){
  # get into a format lda can handle
  chapters_dtm <- x %>%
    select(doc_id=document, term=word, freq=n) %>%
    document_term_matrix() %>%
    # reduce by low frequencies
    dtm_remove_lowfreq(minfreq = 2)
  return(chapters_dtm)
}

chapters_dtm <- convert_to_dtm(word_counts)

# use LDA to classify the documents
chapters_lda <- LDA(chapters_dtm, 
                    k = n_books, control = list(seed = 1234))
chapters_lda

ext_gamma_matrix <- function(model){
  # get gamma matrix for chapter probabilities
  chapters_gamma <- tidy(model, matrix = "gamma")
  # split joint name of book and chapter
  chapters_gamma <- chapters_gamma %>%
    separate(document, c("gutenberg_id", "chapter"), sep = "_", convert = TRUE)
  # get matrix with probabilities for each topic per chapter
  gamma_per_chapter <- chapters_gamma %>%
    spread(topic, gamma)
  return(chapters_gamma)
}

chapters_gamma <- ext_gamma_matrix(chapters_lda)


validate_LDAclassification <- function(x){
  #First we’d find the topic that was most associated with 
  # each chapter using top_n(), which is effectively the 
  # “classification” of that chapter
  chapter_classifications <- x %>%
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
  
  # check the fraction of missclassification
  chapter_classifications %>%
    inner_join(book_topics, by = "topic") %>%
    # missmatches
    dplyr::filter(gutenberg_id != consensus)%>%
    nrow()/nrow(chapter_classifications)
}

validate_LDAclassification(chapters_gamma)

# evaluation over many different book samples
n_books <- 5
n_sim <- 10
ratio <- rep(NA,n_sim)
for (i in 1:n_sim){
  ratio[i] <- set_up_books(n_books=n_books, seed=i) %>% 
    append_by_chapter( n_books = n_books, seed_index = i*i) %>% 
    exclude_stop_words() %>% 
    convert_to_dtm() %>% 
    LDA(k = n_books, control = list(seed = 1234)) %>% 
    ext_gamma_matrix() %>% 
    validate_LDAclassification()
}

ratio %>% mean


#### -------------------------------------
# Splitting and setting up
split_for_fit <- function(data, test_ratio=0.1, seed=1234){
  set.seed(seed)
  N <- nrow(data)
  n_test <- (N*test_ratio) %>% ceiling
  test_ind <- sample(1:N, n_test)
  train_ind <- (1:N)[-train_ind]
  ret <- list(train=data[train_ind,],
              test=data[test_ind,])
  return(ret)
}


fit_n_evaluate <- function(split, k=n_books){
  LDA_model <- LDA(split$train, 
                            k = k, control = list(seed = 1234))
  # use the predict function of udpipe
  # the topic predict funtion already extract the most likely topics
  prediction <- predict(LDA_model, newdata=split$test) %>% .$topic
  # get "consensus" via maximum likelihood
  # first extract the gamma matrix of the model fitted on the training
  # data
  chapters_gamma <- ext_gamma_matrix(LDA_model)
  spreaded_gamma <- chapters_gamma  %>% spread(topic, gamma)
  # get pdfs
  plotm <- spreaded_gamma %>%
    group_by(gutenberg_id) %>%
    # note: pdfs are unnormalized
    summarise_at(2:(titles$len+1), sum)
  topic_link <- plotm %>% 
    apply(1, function(x) which.max(x[2:length(x)])) %>% 
    cbind(plotm$gutenberg_id) %>%
    as.data.frame()
  # exclude the 
  consensus <- split$test %>% 
    rownames() %>% 
    substr(1,regexpr("_",.)-1) %>% 
    as.numeric() %>%
    as.data.frame() %>% 
    # merge it to the topic
    merge(topic_link, by.y="V2", sort=FALSE) %>%
    select(..y)
  # missclassification rate will be returned
  sum(consensus!=prediction)/length(prediction)
}


chapters_dtm %>% 
  split_for_fit(seed=123) %>% 
  fit_n_evaluate()

#-----------------------------------------------------------------
### Another approach ####
spreaded_gamma <- chapters_gamma  %>% spread(topic, gamma)
# get pdfs
plotm <- spreaded_gamma %>%
  group_by(gutenberg_id) %>%
  # note: pdfs are unnormalized
  summarise_at(2:(titles$len+1), sum)

topic_link <- plotm %>% apply(1, function(x) which.max(x[2:length(x)])) %>% cbind(plotm$gutenberg_id)

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
chapters_lda %>% make_prediction(c("7705_3","7705_2","7705_1")) -> N
pred <- N[,6:(n_books+5)] %>% round(3) %>% cbind(N[,1],.)
gammamatrix <- rounded_gamma%>%
  unite(document, gutenberg_id, chapter) %>%
  filter(document%in%c("7705_3","7705_2","7705_1"))
pred; gammamatrix


