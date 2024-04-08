#--------------------- Heirachical cluster -------------------------

library(dendextend)
library(tm)
library(knitr)
library(kableExtra)
library(wordcloud)
library(quanteda)
library(stringr)
library(caret)
library(lsa)
library(SnowballC)
library(MASS)
library(proxy)

mails = read.csv("spam_ham_dataset.csv")
mails1 = mails[,c("text", "label")]
mails1$label = factor(mails1$label)

replace_word_elongation <- function(text) { text <- gsub("\\b(\\w*?)(\\w)\\2{2,}\\b", 
                                                         "\\1\\2", text)
  return(text)
}


corpus1 <- Corpus(VectorSource(as.character(mails1$text)))
corpus1 = tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, removeNumbers)
corpus1 <- tm_map(corpus1, removeWords, c(stopwords("en"), "subject", "re", "fw", "fwd", "can", "please"))  # Remove stopwords
corpus1 <- tm_map(corpus1, stripWhitespace)  # Remove extra whitespaces
corpus1 <- tm_map(corpus1, removePunctuation)
corpus1 = tm_map(corpus1, 
                 content_transformer(function(x) replace_word_elongation(x)))

# Convert the corpus1 to a document-term matrix
dtm1 <- DocumentTermMatrix(corpus1)
# Remove sparse terms (terms that appear in less than 5% of documents)
dtm1 <- removeSparseTerms(dtm1, sparse = 0.955)  # Adjust sparse parameter as needed
# Convert the document-term matrix to a data frame
dtm1 <- as.matrix(dtm1)
# Normalize dtm 
dtm1 <- scale(dtm1)

maildata_hc = as.data.frame(dtm1)

# Compare different dissimilarity measure 

dist_mat_euc = dist(maildata_hc, method = "euclidean")
dist_mat_manh = dist(maildata_hc, method = "manhattan")
#dist_mat_cosine = cosine(maildata_hc)
dist_mat_cosine <- 1 - proxy::simil(as.matrix(maildata_hc), method = "cosine")
set.seed(737)
#------------------------------- Euclidean distance -------------------------------------

par(mfrow=c(2,2))
# complete linkage
hc.complete = hclust(dist_mat_euc, method = "complete")
#plot(hc.complete, main = "Complete Linkage", labels = FALSE)
CutComp <- cutree(hc.complete, k =2)
comp.dend <- as.dendrogram(hc.complete)
comp.dends <- color_branches(comp.dend, k =2)
plot(comp.dends, main = "Complete Linkage")
table(CutComp, mails1$label)

# Average Linkage
hc.average = hclust(dist_mat_euc, method="average")
cutAvg <- cutree(hc.average, k = 2)
avg.dend <- as.dendrogram(hc.average)
avg.dends <- color_branches(avg.dend, k = 2)
plot(avg.dends , main = "Average Linkage")
table(cutAvg, mails1$label)

# Single Linkage
hc.single = hclust(dist_mat_euc, method="single")
# plot(hc.single, main="Single Linkage", labels = FALSE)
cutSgl <- cutree(hc.single, k = 2)
single.dend <- as.dendrogram(hc.single)
plot(single.dend, main = "Single Linkage", k = 2)
table(cutSgl, mails1$label)

# Ward Linkage
hc.ward = hclust(dist_mat_euc , method="ward.D2")
cutWard <- cutree(hc.ward, k = 2)
ward.dend <- as.dendrogram(hc.ward)
ward.dends <- color_branches(ward.dend, k = 2)
plot(ward.dends, main = "Ward Linkage")
table(cutWard, mails1$label)


# ------------------------------ Manhattan distance --------------------------------------

par(mfrow=c(2,2))
# complete linkage
hc.complete1 = hclust(dist_mat_manh, method = "complete")
#plot(hc.complete1, main = "Complete Linkage", labels = FALSE)
CutComp1 <- cutree(hc.complete1, k =2)
comp.dend1 <- as.dendrogram(hc.complete1)
comp.dends1 <- color_branches(comp.dend1, k =2)
plot(comp.dends1, main = "Complete Linkage")
table(CutComp1, mails1$label)

# Average Linkage
hc.average1 = hclust(dist_mat_manh, method="average")
cutAvg1 <- cutree(hc.average1, k = 2)
avg.dend1 <- as.dendrogram(hc.average1)
#avg.dends1 <- color_branches(avg.dend1, k = 2)
plot(avg.dend1 , main = "Average Linkage", k=2)
table(cutAvg1, mails1$label)

# Single Linkage
hc.single1 = hclust(dist_mat_manh, method="single")
# plot(hc.single, main="Single Linkage", labels = FALSE)
cutSgl1 <- cutree(hc.single1, k = 2)
single.dend1 <- as.dendrogram(hc.single1)
#single.dends1 <- color_branches(single.dend1 , k = 2)
plot(single.dend1, main = "Single Linkage", k = 2)
table(cutSgl1, mails1$label)

# Ward Linkage
hc.ward1 = hclust(dist_mat_manh, method="ward.D2")
cutWard1 <- cutree(hc.ward1, k = 2)
ward.dend1 <- as.dendrogram(hc.ward1)
ward.dends1 <- color_branches(ward.dend1, k = 2)
plot(ward.dends1, main = "Ward Linkage")
table(cutWard1, mails1$label)


# --------------------------------- Cosine distance ------------------------------------

par(mfrow = c(2,2))

hc.complete2 = hclust(dist_mat_cosine, method="complete")
# plot(hc.complete,main="Complete Linkage", labels = FALSE)
CutComp2 <- cutree(hc.complete2, k = 2)
comp.dend2 <- as.dendrogram(hc.complete2) # visualize dendrogram with color branches
comp.dends2 <- color_branches(comp.dend2, k = 2)
plot(comp.dends2, main = "Complete Linkage")
table(CutComp2, mails1$label)

##### Average Linkage
hc.average2 = hclust(dist_mat_cosine, method="average")
cutAvg2 <- cutree(hc.average2, k = 2)
avg.dend2 <- as.dendrogram(hc.average2)
avg.dends2 <- color_branches(avg.dend2, k = 2)
plot(avg.dends2, main = "Average Linkage")
table(cutAvg2, mails1$label)

##### Single Linkage
hc.single2 = hclust(dist_mat_cosine, method="single")
# plot(hc.single2, main="Single Linkage", labels = FALSE)
cutSgl2 <- cutree(hc.single2, k = 2)
single.dend2 <- as.dendrogram(hc.single2)
# single.dends2 <- color_branches(single.dend2, k = 2)
plot(single.dend2, main = "Single Linkage", k = 2)
table(cutSgl2, mails1$label)

##### Ward Linkage
hc.ward2 = hclust(dist_mat_cosine, method="ward.D2")
cutWard2 <- cutree(hc.ward2, k = 2)
ward.dend2 <- as.dendrogram(hc.ward2)
ward.dends2 <- color_branches(ward.dend2, k = 2)
plot(ward.dends2, main = "Ward Linkage")
table(cutWard2, mails1$label)




