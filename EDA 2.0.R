# EDA part 2.0 

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

# Check for missing values 
sum(is.na(mail))
# Check column names in dataset
names(mail)
# Assigning a name for column 1 in Excel 
names(mail)[1] <- "Index"
Index <- mail$Index
# Check dimension of dataset
dim(mail)

# Maximum and minimum number of character in subject
max(nchar(mail$text))
min(nchar(mail$text))

# Identify number of spam and ham mail
mail_counts <- data.frame(Label = c("Spam", "Ham"),
                          Count = c(sum(mail$label_num == 1), sum(mail$label_num == 0)))
# Plot graph for visualization 
ggplot(mail_counts, aes(x = Label, y = Count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Label", y = "Number of Messages", title = "Number of spam and Ham Messages")


# Preprocess text data
corpus <- Corpus(VectorSource(as.character(mail$text)))
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert to lowercase
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "subject", "re", "fw", "fwd", "can", "please"))  # Remove stopwords
corpus <- tm_map(corpus, stripWhitespace)  # Remove extra whitespaces
corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, stemDocument) , not used because : 
# it removes extra letters from words causing mispelled words 

# Extract text from corpus
corpus_text <- sapply(corpus, as.character)
# Character count
char_count <- nchar(corpus_text)
# Combine Index, preprocessed text and character count
processed_mail <- cbind(Index, Text_Processed = corpus_text, Char_Count = char_count)

# Display
kable(processed_mail, "html") %>%
  kable_styling()

# Convert the corpus to a document-term matrix
dtm <- DocumentTermMatrix(corpus)
# Remove sparse terms (terms that appear in less than 5% of documents)
dtm <- removeSparseTerms(dtm, sparse = 0.95)  # Adjust sparse parameter as needed
# Convert the document-term matrix to a data frame
dtm_df <- as.data.frame(as.matrix(dtm))


# Store spam word from dtm 
spam_indices <- which(mail$label_num == 1)
spam_dtm <- dtm_df[spam_indices, ]
# Calculate word frequencies in spam messages
spam_word_freq <- colSums(spam_dtm)
# Extracting spam words with frequency greater than or equal to 1
spam_words <- names(spam_word_freq[spam_word_freq >= 1])
# Viewing the extracted words
print(spam_words)


# Store ham word from dtm 
ham_indices <- which(mail$label_num == 0)
ham_dtm <- dtm_df[ham_indices, ]
# Calculate word frequencies in ham messages
ham_word_freq <- colSums(ham_dtm)
# Extracting ham words with frequency greater than or equal to 1
ham_words <- names(ham_word_freq[ham_word_freq >= 1])
# Viewing the extracted words
print(ham_words)

# Conver to dataframe
spam_word_freq_df <- data.frame(Frequency = spam_word_freq)
ham_word_freq_df <- data.frame(Frequency = ham_word_freq)
# Sort the data frame by frequency in descending order
sort_spam_word_freq_df <- spam_word_freq_df[order(-spam_word_freq_df$Frequency), ]
sort_ham_word_freq_df <- ham_word_freq_df[order(-ham_word_freq_df$Frequency), ]

kable(spam_word_freq_df, "html") %>%
  kable_styling()
kable(ham_word_freq_df, "html") %>%
  kable_styling()

# Word cloud for spam messages
wordcloud(colnames(spam_dtm), colSums(spam_dtm), max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
title("Word Cloud for Spam Messages")

# Word cloud for ham messages
wordcloud(colnames(ham_dtm), colSums(ham_dtm), max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
title("Word Cloud for Ham Messages")

# List of word frequent in spam and ham respectively 
sus_spam_words <- names(spam_word_freq[spam_word_freq >  ham_word_freq])  
prob_ham <- names(ham_word_freq[ham_word_freq > 30*spam_word_freq])

# Print suspected words and probable ham 
print(sus_spam_words)
print(prob_ham)


# Boxplot for word frequencies in spam messages
boxplot(spam_word_freq, main = "Word Frequency Distribution in Spam Messages", 
        ylab = "Frequency", col = "lightblue")

# Boxplot for word frequencies in ham messages
boxplot(ham_word_freq, main = "Word Frequency Distribution in Ham Messages", 
        ylab = "Frequency", col = "lightgreen")

common_words <- intersect(spam_words, ham_words)

# Remove common words from spam and ham word lists
spam_words_filtered <- setdiff(spam_words, common_words)
ham_words_filtered <- setdiff(ham_words, common_words)

