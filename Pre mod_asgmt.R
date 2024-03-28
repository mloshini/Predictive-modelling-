# packages
install.packages("ggplot2")
install.packages("caret")
install.packages("wordcloud")
library(ggplot2)
library(wordcloud)
library(caret)
library(tm)
library(stringr)
library(knitr)
library(kableExtra)
# file path 
excel <- "C:/Users/sivan/OneDrive/Documents/TSA asgmt ~/archive/spam_ham_dataset.csv"

# Read csv file
mail <- read.csv(excel)


# -------------------------------------DATA ANALYSIS-------------------------------------------

# structure of dataset
names(mail)
dim(mail)
sapply(mail, class)
mail$label_num <- factor(mail$label_num)
table(mail$label_num)
summary(mail)
label_num <- mail$label_num
sum(is.na(spam))
mail$text = str_squish(mail$text)

# Frequency measure for mail and ham 
spam_count <- sum(mail$label_num == 1)
cat("Count of spam messages:", spam_count, "\n")

ham_count <- sum(mail$label_num == 0)
cat("Count of ham messages:", ham_count, "\n")


# Maximum and minimum number of character in subject
max(nchar(mail$text))
min(nchar(mail$text))
print(which(nchar(mail$text)==10)) # returns the indices

# Unique values
uniques <- lapply(mail, unique)

# Unique values for each column
textcol <- names(Filter(is.character, mail))

for (i in textcol) {
  cat("Unique values in column", i, "are:", unique(mail[[i]]), "\n")
}


# Define the count function
count <- function(text) {
  count <- 0
  c <- ''
  for (i in strsplit(text, "")[[1]]) {
    c <- paste(c, i, sep = "")
    count <- count + 1
  }
  return(count)
}


mail$char_count <- sapply(mail$text, function(x) count(x))

 table
kable(head(mail), "html") %>%
  kable_styling()


#------------------------------------ EXPLORATORY DATA ANALYSIS------------------------------------

# Word clouds mail and ham

# Preprocessing the Corpus
corpus <- Corpus(VectorSource(as.character(mail$text)))
corpus <- tm_map(corpus, tolower)  # Convert to 
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "subject", "re", "fw","fwd")) # Remove common English stopwords
corpus <- tm_map(corpus, stripWhitespace)  # Remove extra whitespaces
corpus <- tm_map(corpus, removePunctuation) 
corpus <- tm_map(corpus, stemDocument)

 # Extract text from corpus
 corpus_text <- sapply(corpus, as.character)
# Convert to data frame
 corpus_df <- data.frame(Text = corpus_text, stringsAsFactors = FALSE)
# Display the first few rows of the data frame
 head(corpus_df)


spam_msg <-corpus[mail$label_num == 1]
ham_msg <- corpus[mail$label_num == 0]

wordcloud(spam_msg, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
title("Word Cloud for Spam Messages")

wordcloud(ham_msg, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
title("Word Cloud for Ham Messages")


# Bar chart
mail_counts <- data.frame(Label = c("mail", "Ham"),
                          Count = c(sum(mail$label_num == 1), sum(mail$label_num == 0)))

ggplot(mail_counts, aes(x = Label, y = Count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Label", y = "Number of Messages", title = "Number of spam and Ham Messages")








# training and testing sets
split <- createDataPartition(y = mail$label_num, p = 0.7, list = FALSE)
train1 <- mail$text[split]
test2 <- mail$text[-split]
train2 <- mail$label_num[split]
test2 <- mail$label_num[-split]

train_bind <- rbind(train1, train2)
train_bind <- data.frame(train_bind)
model <- glm(label_num ~ text, data = train2, family = binomial)


# Predict outcomes on the testing set
predicted_probs <- predict(model, newmail = test_mail, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)  # Threshold at 0.5 for binary classification

# Step 4: Calculate accuracy
actual_classes <- test_mail$response
accuracy <- mean(predicted_classes == actual_classes)
print(paste("Accuracy:", accuracy))


