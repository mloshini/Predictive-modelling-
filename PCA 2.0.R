library(textstem)
library(FactoMineR)
library(factoextra)
library(knitr)
library(quanteda)
library(stringr)
library(caret)


# Read the dataset
pca_D <- read.csv("spam_ham_dataset.csv")
pca_D <- pca_D[, 3:4]

# Count number of punctuation
pca_D$punc_num <- str_count(pca_D$text, "[[:punct:]]")

# Data preprocessing and lemmatization
pca_D$text <- gsub("[[:punct:]]", "", pca_D$text)
pca_D$text <- gsub("[[:digit:]]", "", pca_D$text)
pca_D$text <- str_squish(pca_D$text)
pca_D$text <- str_to_lower(pca_D$text)
pca_D$text <- gsub(0, "Ham", pca_D$text)
pca_D$text <- gsub(1, "Spam", pca_D$text)
pca_D$text <- stem_words(pca_D$text)  # Perform lemmatization

# Count number of characters
pca_D$char_num <- nchar(pca_D$text)

# Add new variables
pca_D$sus_spam <- str_count(pca_D$text, "email|www|http|click|free|best|company")
pca_D$prob_ham <- str_count(pca_D$text, "ect|hou|enron|gas|deal|com|meter")
pca_D$word_length <- str_count(pca_D$text, "\\s+") + 1  # Count number of words

# Create dataset with no label
Dt <- pca_D[, c("punc_num", "sus_spam", "prob_ham", "word_length", "char_num")]

# Standardize the data
Dt <- scale(Dt)

# Remove low variance variables
Dt <- Dt[, apply(Dt, 2, var) > 0.2]  # Adjust the threshold as needed

# Correlation matrix
cor(Dt)
#Check Eigenvector condition 
pca$rotation

# PCA
pca <- prcomp(Dt)
summary(pca)

# Plotting PCA biplot
fviz_pca_biplot(pca, label = "var", habillage = pca_D$label_num)