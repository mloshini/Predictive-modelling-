# ------------------------- Decision tree -----------------------------

library(rpart)
library(rpart)
library(rpart.plot)
library(tm)
library(caret)
library(C50)
library(partykit)

# Load dataset
data <- read.csv("spam_ham_dataset.csv")
df <- data[, c("text", "label")]

# Preprocess the text data
corpus <- VCorpus(VectorSource(as.character(df$text)))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "subject", "re", "fw", "fwd", "can", "please"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)

# Create document-term matrix
matrix <- DocumentTermMatrix(corpus)
matrix <- removeSparseTerms(matrix, 0.95)
mailA <- data.frame(as.matrix(matrix))

# Assign labels
mailA$label <- df$label
# Convert label to factor
mailA$label <- as.factor(mailA$label)


# Split data into training and testing sets
set.seed(55)
split <- sample(2, nrow(mailA), prob = c(0.8, 0.2), replace = TRUE)
train_set <- mailA[split == 1, ]
test_set <- mailA[split == 2, ]

## Rpart ##

# Train rpart decision tree model
decs_rpart <- rpart(label ~ ., data = train_set, method = "class")
# Plot decision tree
rpart.plot(decs_rpart)
# Make predictions
predict_rpart <- predict(decs_rpart, test_set, type = "class")
# Evaluate the model
confusionMatrix(predictions, test_set$label)
x=table(predict_rpart,test_set$label)
# Calculate precision, recall, and F1 score 
precision <- x[1,1] / (x[1,1] + x[2,1])
recall <- x[1,1] / (x[1,1] + x[1,2])
F1_x <- 2 * (precision * recall) / (precision + recall)
# Display the F1 score
F1_x


## C5.0 ##

# Train C5.0 decision tree model
decs_c50 <- C5.0(label ~ ., data = train_set, 
                 control = C5.0Control(winnow = TRUE, noGlobalPruning = TRUE,
                                       CF = 0.2, minCases = 30))
# Plot decision tree
plot(decs_c50)
# Make predictions 
predict_c50 <- predict(decs_c50, test_set, type = "class")
# Evaluate model 
confusionMatrix(predictions, test_set$label)
x1 <- table(predict_c50, test_set$label)
# Calculate precision, recall, and F1 score
precision <- x1[1,1] / (x1[1,1] + x1[2,1])
recall <- x1[1,1] / (x1[1,1] + x1[1,2])
F1_x1 <- 2 * (precision * recall) / (precision + recall)
# Display the F1 score
F1_x1


## Party ##

# Train Party decision tree model 
decs_party =ctree(label ~.,
                  data = train_set, 
                  control = ctree_control(minsplit = 10, minbucket = 5),
                  method = "class")
# Plot decision tree
plot(decs_party)
# Make predictions
predict_party=predict(decs_party ,newdata=test_set)
# Evaluate model 
confusionMatrix(predict_party,test_set$label)
x2 = table(predict_party,test_set$label)
# Calculate precision, recall, and F1 score
precision <- x2[1,1] / (x2[1,1] + x2[2,1])
recall <- x2[1,1] / (x2[1,1] + x2[1,2])
F1_x2 <- 2 * (precision * recall) / (precision + recall)
# Display the F1 score
F1_x2



