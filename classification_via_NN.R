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
library(sampling)

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

n_books <- 6
by_chapter <- set_up_books(n_books=n_books, seed=222)



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
      dplyr::filter(chapter > 2)
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

convert_to_dtm <- function(x, n=n, minfq = 2){
  # get into a format lda can handle
  chapters_dtm <- x %>%
    select(doc_id=document, term=word, freq=n) %>%
    document_term_matrix() %>%
    # reduce by low frequencies
    dtm_remove_lowfreq(minfreq = minfq)
  return(chapters_dtm)
}

chapters_dtm <- convert_to_dtm(word_counts, minfq=2)


# convert x matrix into a form such that it can be used for tensorflow
adjust_tensor_format <- function(x){
  x_chapters <- apply(x, 1, function(x) as.matrix(x)) %>% t()
  topics <- x %>% rownames() %>% as_tibble() %>%
    separate(value, c("gutenberg_id", "chapter"), sep = "_", convert = TRUE) %>%
    select(gutenberg_id) %>%
    # split joint name of book and chapter
    as.matrix %>% as.factor() %>% as.integer()
  # one hot encoding for the chapters (y)
  topics_categorical <- topics %>% -1 %>% 
    to_categorical()
  ret <- list(
    x=x_chapters,
    y=topics_categorical,
    topics=topics
  )
  return(ret)
}

adjusted_format <- adjust_tensor_format((chapters_dtm))



sample_cluster_wise <- function(data, test_ratio=0.1, val_ratio=0.2, seed=1234){
  X <- data$x; y <- data$y
  cluster=data$topics
  set.seed(seed)
  # setting the absolute number of observations for the sample of each cluster
  n_test <- (table(cluster)*test_ratio) %>% floor() %>% 
    # use at least one observation of each cluster
    sapply(., function(x) max(x,1))
  n_val <- (table(cluster)*val_ratio) %>% floor() %>% 
    # use at least one observation of each cluster
    sapply(., function(x) max(x,1))
  # function to get the correct sample indices for validation and test sample
  samp_ind <- function(i, n_list) which(cluster==i) %>% sample(n_list[i])
  test_indices <- unique(cluster) %>% sort() %>% 
    sapply(function(i) samp_ind(i, n_test)) %>% 
    unlist()
  val_indices <- unique(cluster) %>% sort() %>% 
    sapply(function(i) samp_ind(i, n_val)) %>% 
    unlist()
  ret <- list(partial_x_train=X[-c(val_indices,test_indices),], 
              partial_y_train=y[-c(val_indices,test_indices),], 
              x_val=X[val_indices,], y_val = y[val_indices,],
              
              x_test=X[test_indices,], y_test = y[test_indices,])
  return(ret)
}


sample_cluster_wise(adjusted_format)




splitting <- function(data=adjusted_format, 
                      n_testing=5, val_ratio=0.2, seed=1234){
  x <- data$x; y <- data$y
  # function for splitting in train, val and test
  set.seed(seed)
  test_indices <- sample(1:nrow(y), n_testing)
  x_test <- x[test_indices,]
  y_test <- y[test_indices,]
  n_training <- nrow(y)-n_testing
  n_val_indices <- (n_training*val_ratio) %>% round
  val_indices <- sample(1:nrow(y), n_val_indices)
  x_val <- x[val_indices,]
  partial_x_train <- x[-c(val_indices,test_indices),]
  y_val <- y[val_indices,]
  partial_y_train <- y[-c(val_indices,test_indices),]
  ret <- list(partial_x_train=partial_x_train, partial_y_train=partial_y_train, 
              x_val=x_val, y_val=y_val, x_test=x_test, y_test=y_test)
  return(ret)
}

splitting_for_CV <- function(data=adjusted_format, 
                      n_testing=5, val_ratio=0.2, seed=1234, CV_group){
  N <- nrow(data$x)
  # first check how many CV groups we need
  n_groups <- N/n_testing %>% floor()
  # function for splitting in train, val and test
  set.seed(seed)
  # set a index to shuffle the rows
  shuffle_index <- sample(1:N, N, replace = F)
  x <- data$x[shuffle_index,]; y <- data$y[shuffle_index,]
  # we get the samples for the test data just by the test index
  groups <- 1:N %% n_groups +1
  test_indices <- groups==CV_group
  x_test <- x[test_indices,]
  y_test <- y[test_indices,]
  n_training <- N-n_testing
  n_val_indices <- (n_training*val_ratio) %>% round
  val_indices <- sample((1:N)[-test_indices], n_val_indices)
  x_val <- x[val_indices,]
  partial_x_train <- x[-c(val_indices,test_indices),]
  y_val <- y[val_indices,]
  partial_y_train <- y[-c(val_indices,test_indices),]
  ret <- list(partial_x_train=partial_x_train, partial_y_train=partial_y_train, 
              x_val=x_val, y_val=y_val, x_test=x_test, y_test=y_test,
              n_groups=n_groups)
  return(ret)
}

splitting_for_CV(adjusted_format, n_testing = 5, CV_group = 1) %>% .$n_groups


# The whole model is set up and trained within this function
set_up_n_fit <- function(split, books_n=n_books){
  # starting with 64 neurons and scaling it down to 46 in the 
  # mid layer turned out to be a well predicting model
  model <- keras_model_sequential() %>%
    layer_dense(units=64, activation="relu", input_shape=ncol(split$partial_x_train)) %>%
    layer_dense(units=46, activation="relu") %>%
    # we want to classify for as many categories as books
    layer_dense(units=books_n, activation="softmax")
  
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



## evaluate with k-fold crossvalidation - k depending on number of test examples
#n <- splitting_for_CV(adjusted_format, n_testing = 10, CV_group = 1) %>% .$n_groups
## result vector is where the misspecification rate is saved
#results <- rep(NA,n)
#for(i in 1:n){
#  # change the crossvalidation group in each iteration
#  split <- splitting_for_CV(n_testing=5, seed=101, CV_group=i)
#  results[i] <- set_up_n_fit(split) %>% .$model %>% 
#    evaluate_model() %>% .$misspecified
#  print(paste(i, " of ", n))
#}
#
## getting the mean of the misspecification rate
#results %>% mean

tim1 <- Sys.time()
n <- 50
results <- rep(NA,n)
for(i in 1:n){
  split <- sample_cluster_wise(adjusted_format)
  results[i] <- set_up_n_fit(split) %>% .$model %>% 
    evaluate_model() %>% .$misspecified
  print(paste(i, " of ", n))
}
tim2 <- Sys.time()
results %>% mean
# mfreq=0
u1 <- tim2-tim1
# mfreq=2
u2 <- tim2-tim1

