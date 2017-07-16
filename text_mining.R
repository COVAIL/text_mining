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
#' The examples we will cover today involve open source text datasets that have been mostly cleaned for us. We will just have to work on reformatting these data sets to get them "Tidy".
#' 
#' When working with our own data this may not always be the case though. Often text data has inconsistent punctuation, spelling error, acronyms, and multiple ways of referring to the same "thing". Let's do a bit of cleaning with an original text dataset I created. Keep in mind this is just a few of the steps you may have to take to clean your own data. 
#' 
#' INSERT ACTIVITY LOG EXAMPLE 
#' 
## ------------------------------------------------------------------------


#' ## Some additional cleaning reasources to check out
#' Regular expressions are often used to specify "patterns" in certain functions. [This regex cheatsheet](https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf) provides some helpful patterns 
#' you can use in certain functions.
#' 
## ------------------------------------------------------------------------
?grepl
?str_extract
?str_detect

#' 
#' 
#' ##The Tidy Text Format
#' A table with one-token-per-row, where a token is just a meaningful "unit" of text (e.g., word, sentence, or paragraph). This format often makes the data easier to manipulate and visualize using standard  "tidy" tools.
#' 
#' Different from how text is typically stored in many analytic text mining approaches, which often work on a "corpus" and/or a "document-term matrix". However we will see that transfer among the types is easy if our data is first "tidy".
#' 
#' Let's try it ourselves.
#' 
## ----setup, include=FALSE, echo= TRUE , results='hide'-------------------
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
## ------------------------------------------------------------------------
#most common words in all the books?
cleaned_books %>%
  count(word, sort = TRUE)

#Examining and Visualizing most common words across all books and by book: 

cleaned_books_2 <- cleaned_books  %>% 
  count(word,sort = TRUE) %>%  #counting the number of occurrences by group (word in this case)
  left_join(cleaned_books,.) %>% 
  rename(n_allbooks = n)  %>% 
  group_by(book,word) %>% 
  mutate(n_bybook = n()) %>% 
  arrange(book, desc(n_bybook)) %>% 
  ungroup()

#Chart of top 5 words across all books
library(ggplot2)

#doing al in one tidy flow
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
#' ##From Tidy Text to other formats
#' ![From Tidy Text Format to Others](http://tidytextmining.com/images/tidyflow-ch-5.png)
#' 
#' #Sentiment Analysis
#' LEFT OFF ON SENTIMENT ANALYSIS CAN BE DONE AS AN INNER JOIN IN INTRO TAB
#'  *look at other sentiment tabs and see if anything to add then move to topic modeling
#'  *maybe insert my own example 
#' 
#' ##Jane Austen Novels: Example
#' 
#' #Topic Modelling
#' TOPIC MODELING (chps 5 and 6 with optimization for finding right number of topics in seperate script )
#' 
#' 
#' ## Novels example
#' 
#' ## Real World Example
#' 
#' #More Advanced topics 
#' 
#' ##Handy Function for Topic Visualization
#' 
#' ##Finding the Optimal Number of Topics
#' We can use "unsupervised maching learning" algorthims to find the optimal number of topics
#'   *Take advantage of Parallel Processing in R
#' 
#' 
#' 
