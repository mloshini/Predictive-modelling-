
#-------------- PCA --------------------------

# packages
library(tm)
library(knitr)
library(kableExtra)
library(wordcloud)
library(quanteda)
library(stringr)
library(caret)
library(factoextra)

# Read csv file
mail <- read.csv("spam_ham_dataset.csv")


# Naming column 1
names(mail)[1] <- "Index"
Index <- mail$Index

sum(is.na(mail))
mail$text = str_squish(mail$text)

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
dtm_S <- removeSparseTerms(dtm, sparse = 0.95)  # Adjust sparse parameter as needed

# Convert the document-term matrix to a data frame
dtm_df <- as.data.frame(as.matrix(dtm_S))


# Display
kable(processed_mail, "html") %>%
  kable_styling()

# The dimension reductions 
dim(dtm)
dim(dtm_S)


# view col names 
colnames(spam_dtm)
colnames(ham_dtm)


# Obtain frequency of col names
colSums(spam_dtm)
colSums(ham_dtm) 
table(colnames(spam_dtm), colSums(spam_dtm))

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


set.seed(123)  # for reproducibility
train_indices <- createDataPartition(1:nrow(dtm_df), p = 0.8, list = FALSE)

# Split the data into training and testing sets
train_data <- dtm_df[train_indices, ]
test_data <- dtm_df[-train_indices, ]

# Checking dimension
dim(train_data)
dim(test_data)


# Calculate word frequencies in spam messages
spam_word_freq <- colSums(spam_dtm)

# Calculate word frequencies in ham messages
ham_word_freq <- colSums(ham_dtm)

# Identify words that are more frequent in spam messages compared to ham messages
sus_spam_words <- names(spam_word_freq[spam_word_freq >  ham_word_freq])  
prob_ham <- names(ham_word_freq[ham_word_freq > 20*spam_word_freq])
# Print suspected spam words
print(suspected_spam_words)


# PREPROCESS FOR PCA

pca_D <- read.csv("spam_ham_dataset.csv")
pca_D$punc_num <- str_count(pca_D$text, "[[:punct:]]")
pca_D$text  <- gsub("[[:punct:]]", "", pca_D$text)
pca_D$text  <- gsub("[[:digit:]]", "", pca_D$text)
pca_D$text <- str_squish(pca_D$text)
pca_D$text <- str_to_lower(pca_D$text)

# freq_diff <- ham_word_freq - spam_word_freq
# prob_ham <- head(sort(ham_word_freq, decreasing = TRUE), 7)

# ADD NEW VAR

pca_D$char_num <- nchar(pca_D$text)
pca_D$sus_spam <- str_count(pca_D$text, "email") + str_count(pca_D$text, "www") + 
  str_count(pca_D$text, "http") + str_count(pca_D$text, "click") + str_count(pca_D$text, "free")+
  str_count(pca_D$text, "best")+ str_count(pca_D$text, "company")

pca_D$sus_spam <- str_count(pca_D$text, "email") + str_count(pca_D$text, "www") + 
  str_count(pca_D$text, "http") + str_count(pca_D$text, "click") + str_count(pca_D$text, "free")+
  str_count(pca_D$text, "best")+ str_count(pca_D$text, "company")

pca_D$prob_ham <- str_count(pca_D$text, "ect") + str_count(pca_D$text, "hou") + 
  str_count(pca_D$text, "enron") + str_count(pca_D$text, "gas") + str_count(pca_D$text, "deal")+
  str_count(pca_D$text, "com")+ str_count(pca_D$text, "please")

pca_D$word_length = str_count(pca_D$text , "")+1 

# Display
kable(pca_D, "html") %>%
  kable_styling()

# Correlations and building PCA <3
Dt = pca_D[, 5:9]
cor(Dt)
mean(cor(Dt))


pca <- prcomp(Dt, scale = TRUE) 
summary(pca)
fviz_pca_biplot(pca, label = "var",
                habillage = pca_D$label)