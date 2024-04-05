# ------------------------------------------- SVM ----------------------------------------------------

library(e1071)
library(caret)
# Data pre processing 

data = read.csv("spam_ham_dataset.csv")
df = data[, c("text", "label")]

corpus <- VCorpus(VectorSource(as.character(mail$text)))
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert to lowercase
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "subject", "re", "fw", "fwd", "can", "please"))  # Remove stopwords
corpus <- tm_map(corpus, stripWhitespace)  # Remove extra whitespaces
corpus <- tm_map(corpus, removePunctuation)

matrix= DocumentTermMatrix(corpus)
matrix= removeSparseTerms(matrix, 0.95)
mailA = data.frame(as.matrix(matrix))

dim(mailA)
mailA$label <- mail$label

train_set$label <- as.factor(train_set$label)
test_set$label <- as.factor(test_set$label)

set.seed(55)
split = sample(2,nrow(mailA),prob = c(0.8,0.2),replace = TRUE)
train_set = dataset_a[split == 1,]
test_set = dataset_a[split == 2,]

# Linear
svm.linear <- svm(label ~., data=train_set, scale=FALSE, kernel= "linear")
pred.linear <- predict(svm.linear, test_set)
conf_matrix <- confusionMatrix(data = pred.linear, reference = test_set$label)
print(conf_matrix)

# Polynomial
svm.poly <- svm(label ~., data=train_set, scale=FALSE, kernel="polynomial")
pred.poly <- predict(svm.poly, test_set)
conf_matrix <- confusionMatrix(data = pred.linear, reference = test_set$label)
print(conf_matrix)

# Radial
svm.radial <- svm(label~., data=train_set, scale=FALSE, kernel= "radial")
pred.radial <- predict(svm.radial, test_set)
conf_matrix <- confusionMatrix(data = pred.linear, reference = test_set$label)
print(conf_matrix)


# Sigmoid
svm.sigmoid <- svm(label ~., data=train_set, scale=FALSE, kernel= "sigmoid")
pred.sigmoid <- predict(svm.sigmoid,test_set)
conf_matrix <- confusionMatrix(data = pred.linear, reference = test_set$label)
print(conf_matrix)

# Cross validation ( SVM_linear to select best value for "C")
set.seed(666)
tuned=tune.svm(label ~., data = train_set, kernel = "linear", cost =c(0.001,0.01,0.1,1,10,100))
summary(tuned)
par(mfrow=c(1, 1))
plot(tuned)

# result after cross validation ( SVM_linear "C"=0.1)
svm.linear0.1<- svm(label ~. , data=train_set, scale=FALSE, kernel= "linear",cost=0.1)
print(svm.linear0.1)
conf_matrix <- confusionMatrix(data = pred.linear0.1, reference = test_set$label)
print(conf_matrix)
