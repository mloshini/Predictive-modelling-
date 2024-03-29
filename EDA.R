# EDA part 

# packages
library(tm)
library(knitr)
library(kableExtra)
library(wordcloud)

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

# Display
kable(processed_mail, "html") %>%
  kable_styling()

# ------------FAV
spam_msg <-corpus[mail$label_num == 1]
ham_msg <- corpus[mail$label_num == 0]
wordcloud(spam_msg, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
title("Word Cloud for Spam Messages")
wordcloud(ham_msg, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
title("Word Cloud for Ham Messages")



# ------------NOT FAV
#spam_indices <- which(mail$label_num == 1)
#ham_indices <- which(mail$label_num == 0)
#spam_corpus <- processed_mail[spam_indices, "Text_Processed"]
#ham_corpus <- processed_mail[ham_indices, "Text_Processed"]
#wordcloud(spam_corpus, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
#title("Word Cloud for Spam Messages")
#wordcloud(ham_corpus, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
#title("Word Cloud for Ham Messages")


