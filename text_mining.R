#' ---
#' title: "Text Mining: Tidying, Sentiment Analysis, & Topic Modeling"
#' subtitle: RLadies- Columbus
#' author: "Katie Sasso"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---
#' 
#' # Getting Started 
#' 
#' ##Cleaing your text data
#' The examples we will cover today involve open source text datasets that have been mostly cleaned for us. We will mostly just have to work on reformatting these data sets to get them "Tidy".
#' 
#' When working with our own data this may not always be the case though. Often text data has inconsistent punctuation, spelling error, acronyms, and multiple ways of referring to the same "thing". Several key cleaning resources can be helpful for addressing these issues:
#' 
#'  * [stringr](https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html) is a very helpful package for cleaning text. 
#'  * [hunspell](https://github.com/ropensci/hunspell) package provides functions for spell check.
#'  * Regular expressions are often used to specify "patterns" in certain functions (i.e., stringr fxns). [This regex cheatsheet](https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf) provides some helpful  string patterns.
#'  * This [regex matcher website](https://regex101.com/) is the most helpful free resource I've found for checking and testing that your regular expression is _actually_ specifying the string you think it's specifying. 
#'  * Given the often cumbersome nature of working with regular expressions the [rex](https://github.com/kevinushey/rex) package allows you to build complex regular expressions in human readable format. 
#'  * [The quanteda package](https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html) and the [SnowballC package](https://cran.r-project.org/web/packages/SnowballC/SnowballC.pdf) allows for efficient stemming (i.e., the process of reducing inflected (or sometimes derived) words to their word stem) so that "cats" and "cat" are recongnized as the same "token". 
#' 
## ------------------------------------------------------------------------
#base functions work too!
?grepl
?gsub
#tidy versions
?str_extract
?str_detect

#' 
#' ##The Tidy Text Format
#' A table with one-token-per-row, where a token is just a meaningful "unit" of text (e.g., word, sentence, or paragraph). This format often makes the data easier to manipulate and visualize using standard  "tidy" tools.
#' 
#' Different from how text is typically stored in many analytic text mining approaches, which often work on a "corpus" and/or a "document-term matrix". However we will see that transfer among the types is easy if our data is first "tidy".
#' 
#' Let's try it ourselves.
#' 
## ----clean, results='hide', message=FALSE, warning= FALSE, error=FALSE----
#install these packages if you don't already have them
# install.packages("tidytext")
# install.packages("janeaustenr")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("magrittr")


library(janeaustenr)
library(plyr)
library(dplyr)
library(stringr)
library(magrittr)

#browse the related vignette or others
vignette("tidytext",package="tidytext")
?austen_books

original_books <- austen_books() %>%
  group_by(book) %>% #grouping df by "book"
  mutate(linenumber = row_number(), #adding row numbers as a new variable
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",  
                                                 ignore_case = TRUE)))) %>%
  ungroup()
# the above is creating a new chapter variable by searching the "text" character variable for the string "chapter" and Roman Numeral characters (as specified in regex language here "[\\divxlc]")

head(original_books)
tail(original_books)
slice(original_books, 1:500) %>% View

#restructuring as a one-token-per-row format. unnest() does that for us
library(tidytext)

tidy_books <- original_books %>%
  unnest_tokens(word, text)

#The default tokenizing is for words, but other options include characters, ngrams, sentences, lines, paragraphs, or separation around a regex pattern.

data("stop_words")
head(stop_words)

cleaned_books <- tidy_books %>%
  anti_join(stop_words)


#' 
#' ##Some Descriptives once Tidy
#' 
## ----descriptives, results='hide', message=FALSE, warning= FALSE, error=FALSE----
#most common words in all the books?
cleaned_books %>%
  count(word, sort = TRUE)

#Examining and Visualizing most common words across all books and by book: 

cleaned_books_2 <- cleaned_books  %>% 
  mutate(word = str_extract(word, "[[:alpha:]]+")) %>%
  count(word,sort = TRUE) %>%  #counting the number of occurrences by group (word in this case)
  left_join(cleaned_books,.) %>% 
  rename(n_allbooks = n)  %>% 
  group_by(book,word) %>% 
  mutate(n_bybook = n()) %>% 
  arrange(book, desc(n_bybook)) %>% 
  ungroup()

##str_extract = extract strings that match the patten spcified, in this case, extracting all alphabetic characters regardless of capitalization. So Numbers, symbols (i.e.,"_" and "*" often used to flag italics or bold will be removed-- we don't want don't want "Angry" to appear as a diff word than it's italicized form "_Angry_")

#Chart of top 5 words across all books
library(ggplot2)

#doing all in one tidy flow
cleaned_books_2 %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#using new var we created above
#no scientifict notation
options(scipen = 10000)
cleaned_books_2 %>% 
  filter(n_allbooks > 600, !duplicated(word)) %>% 
  mutate(word = reorder(word, n_allbooks)) %>% 
  ggplot(aes(word, n_allbooks)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#What about the top 5 words only in each book? 
cleaned_books_2 %>% 
  group_by(book) %>% 
  filter(!duplicated(word)) %>% 
  arrange(desc(n_bybook)) %>% 
  top_n(3, n_bybook) %>% 
  ungroup %>% 
  arrange(book, n_bybook) %>% 
  ggplot(aes(word, n_bybook)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~book)

#other ways to visualize?? 

#'  
#' #Sentiment Analysis
#' 
#' We can use the tools of text mining to assess the emotional content of text:
#' 
#'  * Consider text as a combination of its individual words.
#'  * Sentiment content = sum of the sentiment content of the individual words
#'  
#' ![Flowchart: using tidytext for sentiment analysis](http://tidytextmining.com/images/tidyflow-ch-2.png)
#'  
## ----lexicons, message=FALSE, warning= FALSE, error=FALSE----------------

#Four sentiment lexicons are in the tidytext package in the sentiments dataset.
head(sentiments)
table(sentiments$lexicon)
#How many distinct "emotions" are words categorized into?
table(sentiments$sentiment)

#' For more on the sentiment lexicons contained in this dataset and how they differ checkout some details [here](http://tidytextmining.com/sentiment.html#the-sentiments-dataset):
#'  
#' Keep in mind that these methods do not take into account qualifiers before a word, such as in “no good” or “not true”; a lexicon-based method like this is based on unigrams only. For many kinds of text (like the narrative examples below), there are not sustained sections of sarcasm or negated text, so this is not an important effect.
#' 
#' Also, the size of the chunk of text that we use to add up sentiment scores can effect analysis. A text the size of many paragraphs can often have positive and negative sentiment averaged out to about zero, while sentence-sized or paragraph-sized text often works better.
#'  
#' Let's try some sentiment analysis ourselves
## ----sentim, message=FALSE, warning= FALSE, error=FALSE------------------

#Let’s look at the words with a joy score from the NRC lexicon. What are the most common joy words in Emma?

nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

#"get_sentiments" is just a helper fxn that retrives a specific lexicon with only relevant columns
# equivalent too: filter(sentiments, lexicon == 'nrc' & sentiment == 'joy') %>%  select(word, sentiment)

emma_joy <- tidy_books %>%
  filter(book == "Emma") %>%
  semi_join(nrcjoy) %>%
  count(word, sort = TRUE)

head(emma_joy)
#noticed how we used the original tidy books dataset as opposed to to "cleaned_books_2" which we later made. Why? Stop words (i.e., "good", "young") are often overlapping with sentient phrases. 

#We could also examine how sentiment changes during each novel. 
#Let’s find a sentiment score for each word using the Bing lexicon, then count the number of positive and negative words in defined sections of each novel.

# a little on bing :
#categorizes words in a binary fashion to be "postiive" or "negative"
bing <- get_sentiments("bing")
head(bing)
library(tidyr)

janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>% #return tidy_book rows with matching values in bing sentiment df 
  count(book, index = linenumber %/% 100, sentiment) %>% #counting number of positive/neg lines wihtin each index chunk
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) #simple msr for each index as # positive words - # of neg words. 

#indexing will be used to parse up the novel and examine sentiment change throughout
#index 0 represents the first 100 lines, index 1 represents the 2nd 100 lines, etc. - and use the index as the atomic unit to evaluate the sentiment
#"%/%" does "integer" division for us 
5/2
5%/%2

ggplot(data = janeaustensentiment, aes(x = index, y = sentiment, fill = book)) +
    geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
    facet_wrap(facets = ~ book, ncol = 2, scales = "free_x")

#We could also examine the most common positive & negative words in the Jane Austen novels

bing_word_counts <- tidy_books %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = T) %>%
    ungroup()

#format for graphing 
tmp <- bing_word_counts %>%
    filter(n > 150) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>% #just making counts for "neg" words negative for graphing
    mutate(word = reorder(word, n))

ggplot(data = tmp, mapping = aes(x = word, y = n, fill = sentiment)) +
    geom_bar(alpha = 0.8, stat = "identity") +
    labs(y = "Contribution to sentiment", x = NULL) +
    coord_flip()


#' 
#' #Topic Modelling
#' 
#' ##Tidy text vs. Other formats
#' So far we've analyzed text in the tidy format (one-token-per-doc-per-row). 
#' Many useful descriptive analyses can be done from this format, but many NLP analyses require a different format:
#' 
#'  * [CRAN Task View of NLP](https://cran.r-project.org/web/views/NaturalLanguageProcessing.html) lists many packages that take other structures of input and provide non-tidy outputs. 
#'  * Useful for many text mining applications. 
#' 
#' ![Flowchart: Tidy Text to other Formats](http://tidytextmining.com/images/tidyflow-ch-5.png)
#' 
#' Let's examine one of the most common non-tidy structures text mining packages use: the document-term matrix (DTM). This is a matrix where: 
#'  * Each row represents one document (such as a book or article)
#'  * Each column represents one term
#'  * Each value (typically) contains the number of appearances of that term in that document.
#'  * "Sparse" matrics as  most pairings of doc and term don't occur
#'  * Can't be used directly with tidy tools, so tidytext package provides two verbs (tidy() and cast()) to convert betwen tidy and dtm formats. 
#'  
#' Let's convert our tidy_books df to a dtm so we can impliment one common NLP model: Latent Dirichlet allocation (i.e., LDA or topic modeling)
#'  
## ----dtmconvert, message=FALSE, warning= FALSE, error=FALSE--------------
#install the tm and topicmodels package if you don't have them
#install.packages("tm")
#install.packages("topicmodels")
library(tm)
library(topicmodels)

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm <- austen_books() %>%
  group_by(book) %>% 
  ungroup() %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  mutate(word = str_extract(word, "[[:alpha:]]+"))  %>% #taking out chp numbers (and others!)
  count(book, word) %>%
  cast_dtm(book, word, n)

#let's take a look at the cast_dtm fxn
?cast_dtm

#' ##A very brief overview of topic modeling (LDA)
#' 
#' Topic modeling helps us devide collections of documents into groups so we can understand their content better. 
#' 
#' Topic modeling is:
#' 
#'  * An unsupservised classification of documents, similar to clustering in numeric data.
#'  * LDA is a popular topic modeling method that treats:
#'   * Each doc as a mixture of topics (i.e., in a two-topic model we could say “Document 1 is 90% topic A and 10% topic B, while Document 2 is 30% topic A and 70% topic B.)
#'   * Each topic as a mixture of words (i.e., For example, we could imagine a two-topic model of American news, with one topic for “politics” and one for “entertainment.” The most common words in the politics topic might be “President”, “Congress”, and “government”, while the entertainment topic may be made up of words such as “movies”, “television”, and “actor”. Importantly, words can be shared between topics; a word like “budget” might appear in both equally.)
#' 
#' ##Using topic modeling to reconstruct content of four different books 
#' Suppose a vandal has broken into your study and torn apart four of your books: 
#' 
#'  * Great Expectations by Charles Dickens
#'  * The War of the Worlds by H.G. Wells
#'  * Twenty Thousand Leagues Under the Sea by Jules Verne
#'  * Pride and Prejudice by Jane Austen
#'  
#' This vandal has torn the books into individual chapters, and left them in one large pile. How can we restore these disorganized chapters to their original books? This is a challenging problem since the individual chapters are unlabeled: we don’t know what words might distinguish them into groups. We’ll thus use topic modeling to discover how chapters cluster into distinct topics, each of them (presumably) representing one of the books.
#' 
#' We’ll retrieve the text of these four books using the gutenbergr package
#' 
#' #Our first topic model 
## ----topics, message=FALSE, warning= FALSE, error=FALSE------------------
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")
#install.packages("gutenbergr")
library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

#tidy up, remove stop words, and divide into documents by chapter

library(stringr)

# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda
#setting the seed is important to make sure our results are reproducible

#Let's extract the per-topic-per-word probabilities.

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics
#turned the model into a one-topic-per-term-per-row format. For each combination, the model computes the probability of that term being generated from that topic. For example, the term “joe” has an almost zero probability of being generated from topics 1, 2, or 3, but it makes up 1.45% of topic 4

#what are the top 5 terms within each document? 
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

#let's visualize
library(ggplot2)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#' These topics are pretty clearly associated with the four books! There’s no question that the topic of “captain”, “nautilus”, “sea”, and “nemo” belongs to Twenty Thousand Leagues Under the Sea, and that “jane”, “darcy”, and “elizabeth” belongs to Pride and Prejudice. We see “pip” and “joe” from Great Expectations and “martians”, “black”, and “night” from The War of the Worlds. We also notice that, in line with LDA being a “fuzzy clustering” method, there can be words in common between multiple topics, such as “miss” in topics 1 and 4, and “time” in topics 3 and 4.
## ----topics_part2, results='hide', message=FALSE, warning= FALSE, error=FALSE----
#Let's extract the document-topic-probabilities 
#These may help us to see which topisc are associated with each document 
chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

#Each of these values is an estimated proportion of words from that document that are generated from that topic.  For example, the model estimates that each word in the Great Expectations_57 document has only a 0.00135% probability of coming from topic 1 (Pride and Prejudice).

#Now that we have these topic probabilities, we can see how well our unsupervised learning did at distinguishing the four books. We’d expect that chapters within a book would be found to be mostly (or entirely), generated from the corresponding topic.

#First we re-separate the document name into title and chapter, after which we can visualize the per-document-per-topic probability for each

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

#We notice that almost all of the chapters from Pride and Prejudice, War of the Worlds, and Twenty Thousand Leagues Under the Sea were uniquely identified as a single topic each.
#It does look like some chapters from Great Expectations (which should be topic 4) were somewhat associated with other topics. 

#Are there any cases where the topic most associated with a chapter belonged to another book? First we’d find the topic that was most associated with each chapter using top_n(), which is effectively the “classification” of that chapter.

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications

#We can then compare each to the “consensus” topic for each book (the most common topic among its chapters), and see which were most often misidentified.

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)

#We see that only two chapters from Great Expectations were misclassified, as LDA described one as coming from the “Pride and Prejudice” topic (topic 1) and one from The War of the Worlds (topic 3). 


#' 
#' Some additional resources to dive into later on your text mining journey:
#'  *As mentioned [the quanteda package](https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html) for managing and analyzing text. Built for efficiency and speed to allow for faster manipultion and processing. Quick conversion from dfm to non-quanteda format (i.e., dtm).  Good for tokenizing and stemming. 
#' 
#' #More advanced topics.. 
#' 
#' ## Predictive Analyses
## ----predicting, message=FALSE, warning= FALSE, error=FALSE--------------

#Document topic probabilities can be used as predictors
chapters_gamma <- tidy(chapters_lda, matrix = "gamma") %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>% 
  mutate(topic = paste0("topic","_",topic))

chps_topic_probability <- spread(chapters_gamma, topic, gamma) %>% 
  mutate(book = as.factor(str_replace_all(tolower(title)," ","_")))
#install.packages(MASS)

library(MASS)
# find the linear combinations of the original variables (the 13 chemical concentrations here) that gives the best possible separation between the groups
chps_topic_probability <- chps_topic_probability %>% 
  mutate(id = row.names(.))

train <- chps_topic_probability %>% 
  sample_frac(.80)
test <- chps_topic_probability %>% 
  filter(!id %in% train$id)

lda.fit <- lda(book ~ topic_1 + topic_2 + topic_3 + topic_4 , 
                  data = train)
#nice - the first discriminant function achieved 98% seperation!

lda.pred=predict(lda.fit, test)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,test$book)
lda.class[1:20]
test[1:20,'book']

mean(lda.class==test$book) #100% accuracy!! 




#' 
#' ##Interactive Topic Visualization
## ----visualizing_lda, results='hide', message=FALSE, warning= FALSE, error=FALSE----
##Handy Function for Topic Visualization

#' @title Interactive Tool for visualizing Topics and Per-Word-pertopic-probabilities
#' @param file Fitted LDA output, corpus file: row for each token-document combo with count of token within document ,document term matrix
#' @return Shiny visual of all topics and associated words
#interactive visualization of all topics

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  # Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)
  
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  #temp_frequency <- inspect(doc_term)
  freq_matrix <- data.frame(ST = colnames(doc_term),
                            Freq = colSums(as.matrix(doc_term)))
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  
  return(json_lda)
}

topics_json_lda <- topicmodels_json_ldavis(chapters_lda,
                                         tm::Corpus(tm::VectorSource(distinct(word_counts, document)$document)),
                                         chapters_dtm)
#serVis(topics_json_lda, open.browser = TRUE)


#' 
#' ##Finding the Optimal Number of Topics
#' We can take advantage of parrallel processing in R find the optimal number of topics
#' 
## ----optimal_topics, results='hide', message=FALSE, warning= FALSE, error=FALSE,eval = FALSE----
## 
#Selecting the optimal number of topics using 5-fold cross-validation with parallel processing
#Caution- this will take a few mins

#install the below packages before running if you do not already have them
library(parallel)
library(foreach)
library(doParallel)
cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave at least one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(topicmodels)
})

# set parameters for LDA below
folds <- 5
splitfolds <- sample(1:folds, n, replace = TRUE)
candidate_k <- c(2, 3, 4, 5, 6, 7, 10)
burnin = 1000
iter = 1000
keep = 50
n <- nrow(chapters_dtm) # doc term matrix created above
clusterExport(cluster, c("chapters_dtm", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k", "n"))

system.time({
 results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
   k <- candidate_k[j]
   results_1k <- matrix(0, nrow = folds, ncol = 2)
   colnames(results_1k) <- c("k", "perplexity")
   for(i in 1:folds){
     train_set <- chapters_dtm[splitfolds != i , ] #replace with approprite dtm
     valid_set <- chapters_dtm[splitfolds == i, ]
     fitted <- LDA(train_set, k = k, method = "Gibbs",
                   control = list(burnin = burnin, iter = iter, keep = keep, seed = 1234) )
     results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
   }
   return(results_1k)
 }
})
stopCluster(cluster)

results_df <- as.data.frame(results)
#Selecting optimal number of topics given lowest perplexity
ggplot(results_df, aes(x = k, y = perplexity)) +
 geom_point() +
 geom_smooth(se = FALSE) +
 ggtitle("5-fold cross-validation of topic modelling with the 'Chapters' data",
          "(ie five different models fit for each candidate number of topics)") +
 labs(x = "Candidate number of topics", y = "Perplexity when fit to the hold-out set")

#if you run you'll see 4/5 is the optimal number of topics!


