###########################################################
### --------------------------------------------------- ###
### ------------------- NN with books ----------------- ###
### --------- Chapter classificaion with books--- ----- ###
### --------------------------------------------------- ###


# loading packages
library(keras)
library(gutenbergr)
library(dplyr)
library(tensorflow)
library(tidyr)
library(stringr)
library(tidytext)
library(udpipe)


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
# we get a certein number of books n_books
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

x_chapters <- apply(chapters_dtm, 1, function(x) as.matrix(x)) %>% t()

topics_categorical <- chapters_dtm %>% rownames() %>% as_tibble() %>%
  separate(value, c("gutenberg_id", "chapter"), sep = "_", convert = TRUE) %>%
  select(gutenberg_id) %>%
  # split joint name of book and chapter
  as.matrix %>% as.factor() %>% as.integer() %>% -1 %>%
  to_categorical()


splitting <- function(x=x_chapters, y=topics_categorical, 
                      n_testing=5, training_ratio=0.2, seed=1234){
  # function for splitting in train, val and test
  set.seed(seed)
  test_indices <- sample(1:nrow(y), n_testing)
  x_test <- x[test_indices,]
  y_test <- y[test_indices,]
  n_training <- nrow(y)-n_testing
  n_val_indices <- (n_training*training_ratio) %>% round
  val_indices <- sample(1:nrow(y), n_val_indices)
  x_val <- x[val_indices,]
  partial_x_train <- x[-c(val_indices,test_indices),]
  y_val <- y[val_indices,]
  partial_y_train <- y[-c(val_indices,test_indices),]
  ret <- list(partial_x_train=partial_x_train, partial_y_train=partial_y_train, 
              x_val=x_val, y_val=y_val, x_test=x_test, y_test=y_test)
  return(ret)
}


# The whole model is set up and trained within this function
set_up_n_fit <- function(split){
  # starting with 64 neurons and scaling it down to 46 in the 
  # mid layer turned out to be a well predicting model
  model <- keras_model_sequential() %>%
    layer_dense(units=64, activation="relu", input_shape=ncol(x_chapters)) %>%
    layer_dense(units=46, activation="relu") %>%
    # we want to classify for 6 categories
    layer_dense(units=6, activation="softmax")
  
  model %>% compile(
    optimizer="rmsprop",
    loss="categorical_crossentropy",
    metrics=c("accuracy"))
  
  history <- model %>% fit(
    split$partial_x_train,
    split$partial_y_train,
    # from experience the model tends to 
    # overfit for more than 5 epochs
    epochs=5,
    batch_size=512,
    validation_data=list(split$x_val,split$y_val)
  )
  return(
    list(history=history, 
         model=model))
}


# making a prediction on the test data and calculating the
# misspecification rate; we also want to save the true categories and the predicted ones
evaluate_model <- function(model_fit, y=split$y_test, x=split$x_test) {
  prediction <- model_fit %>% predict(x)
  pred <- apply(prediction, 1, which.max)
  true_value <- apply(y, 1, which.max)
  misspecified <- sum(!pred==true_value)/length(pred)
  ret <- list(misspecified=misspecified,
              # the function also dicloses the true and the predicted
              # values for exact evaluation, if needed
              pred=pred, true_value=true_value)
  return(ret)
}


# evaluate with several test set and training sets
# result vector is where the misspecification rate is saved
n <- 60
results <- rep(NA,n)
for(i in 1:n){
  # change the seed for every iteration to get different samples
  # putting all together
  results[i] <- splitting(n_testing=15, seed=101+i*2) %>% 
    set_up_n_fit() %>% .$model %>% 
    evaluate_model() %>% .$misspecified
}

# getting the mean of the misspecification rate
results %>% mean





