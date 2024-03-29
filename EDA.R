# EDA part 

# packages
library(tm)
library(knitr)
library(kableExtra)
library(wordcloud)
library(quanteda)
library(stringr)
library(caret)

# Read csv file
mail <- read.csv("spam_ham_dataset.csv")

sum(is.na(mail))
mail$text = str_squish(mail$text)

names(mail)[1] <- "Index"

# Now you can refer to the column using mail$column1
Index <- mail$Index

# Preprocess text data
corpus <- Corpus(VectorSource(as.character(mail$text)))
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert to lowercase
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "subject", "re", "fw", "fwd"))  # Remove stopwords
corpus <- tm_map(corpus, stripWhitespace)  # Remove extra whitespaces
corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, stemDocument)

# Extract text from corpus
corpus_text <- sapply(corpus, as.character)

# Character count
char_count <- nchar(corpus_text)

# Combine ori data with preprocessed text and character count
processed_mail <- cbind(Index, Text_Processed = corpus_text, Char_Count = char_count)

# Convert the corpus to a document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Remove sparse terms (terms that appear in less than 5% of documents)
dtm <- removeSparseTerms(dtm, sparse = 0.95)  # Adjust sparse parameter as needed

# Convert the document-term matrix to a data frame
dtm_df <- as.data.frame(as.matrix(dtm))


# Display
kable(processed_mail, "html") %>%
  kable_styling()


# view col names 
colnames(spam_dtm)
colnames(ham_dtm)

# Obtain frequency of col names
colSums(spam_dtm)
colSums(ham_dtm) 


# Word cloud for spam messages
spam_indices <- which(mail$label_num == 1)
spam_dtm <- dtm_df[spam_indices, ]
wordcloud(colnames(spam_dtm), colSums(spam_dtm), max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
title("Word Cloud for Spam Messages")

# Word cloud for ham messages
ham_indices <- which(mail$label_num == 0)
ham_dtm <- dtm_df[ham_indices, ]
wordcloud(colnames(ham_dtm), colSums(ham_dtm), max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
title("Word Cloud for Ham Messages")


# Set the seed for reproducibility
set.seed(123)

# Combine the index with the preprocessed text
processed_mail <- cbind(Index, Text_Processed = corpus_text)

# Define the indices for splitting
train_indices <- createDataPartition(mail$label_num, p = 0.8, list = FALSE)

# Create training and testing datasets
train_data <- processed_mail[train_indices, ]
test_data <- processed_mail[-train_indices, ]






























tokens_unigrams <- unlist(tokenize_character(corpus))

# Tokenization - Bigrams
tokens_bigrams <- unlist(tokenize_character(corpus, n = 2))

# Tokenization (Unigrams and Bigrams)
tokens <- tokens(corpus, what = "word")
tokens <- tokens_ngrams(tokens, n = 1:2, concatenator = " ")






