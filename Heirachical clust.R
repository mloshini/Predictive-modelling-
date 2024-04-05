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

mails = read.csv("spam_ham_dataset.csv")
mails1 = mails[,c("text", "label")]
mails1$label = factor(mails1$label)

replace_word_elongation <- function(text) { text <- gsub("\\b(\\w*?)(\\w)\\2{2,}\\b", 
                                                         "\\1\\2", text)
  return(text)
}


corpus1 <- Corpus(VectorSource(as.character(mail$text)))
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
dtm1 <- removeSparseTerms(dtm1, sparse = 0.95)  # Adjust sparse parameter as needed
# Convert the document-term matrix to a data frame
dtm1 <- as.matrix(dtm1)
# Normalize dtm 
dtm1 <- scale(dtm1)

maildata_hc = as.data.frame(dtm1)

# Compare different dissimilarity measure 

dist_mat_euc = dist(maildata_hc, method = "euclidean")
dist_mat_manh = dist(maildata_hc, method = "manhattan")
dist_mat_cosine = dist_mat_cosine <- cosine(dtm1)

#------------------------------- Euclidean distance -------------------------------------

par(mfrow=c(2,2))
# complete linkage
hc.complete = hclust(dist_mat_euc, method = "complete")
plot(hc.complete, main = "Complete Linkage", labels = FALSE)
CutComp <- cutree(hc.complete, k =2)
comp.dend <- as.dendrogram(hc.complete)
comp.dends <- color_branches(comp.dend, k =2)
plot(comp.dends, main = "Complete Linkage")
table(CutComp, mails$label)

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
single.dends <- color_branches(single.dend , k = 2)
plot(single.dends, main = "Single Linkage")
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
hc.complete = hclust(dist_mat_manh, method = "complete")
#plot(hc.complete, main = "Complete Linkage", labels = FALSE)
CutComp <- cutree(hc.complete, k =2)
comp.dend <- as.dendrogram(hc.complete)
comp.dends <- color_branches(comp.dend, k =2)
plot(comp.dends, main = "Complete Linkage")
table(CutComp, mails1$label)

# Average Linkage
hc.average = hclust(dist_mat_manh, method="average")
cutAvg <- cutree(hc.average, k = 2)
avg.dend <- as.dendrogram(hc.average)
avg.dends <- color_branches(avg.dend, k = 2)
plot(avg.dends , main = "Average Linkage")
table(cutAvg, mails1$label)

# Single Linkage
hc.single = hclust(dist_mat_manh, method="single")
# plot(hc.single, main="Single Linkage", labels = FALSE)
cutSgl <- cutree(hc.single, k = 2)
single.dend <- as.dendrogram(hc.single)
single.dends <- color_branches(single.dend , k = 2)
plot(single.dends, main = "Single Linkage")
table(cutSgl, mails1$label)

# Ward Linkage
hc.ward = hclust(dist_mat_manh, method="ward.D2")
cutWard <- cutree(hc.ward, k = 2)
ward.dend <- as.dendrogram(hc.ward)
ward.dends <- color_branches(ward.dend, k = 2)
plot(ward.dends, main = "Ward Linkage")
table(cutWard, mails1$label)


# --------------------------------- Cosine distance ------------------------------------

par(mfrow = c(2,2))

hc.complete = hclust(dist_mat_cosine, method="complete")
# plot(hc.complete,main="Complete Linkage", labels = FALSE)
CutComp <- cutree(hc.complete, k = 2)
comp.dend <- as.dendrogram(hc.complete) # visualize dendrogram with color branches
comp.dends <- color_branches(comp.dend, k = 2)
plot(comp.dends, main = "Complete Linkage")
table(CutComp, mails1$label)

##### Average Linkage
hc.average = hclust(dist_mat_cosine, method="average")
cutAvg <- cutree(hc.average, k = 2)
avg.dend <- as.dendrogram(hc.average)
avg.dends <- color_branches(average.dend.object, k = 2)
plot(average.dend, main = "Average Linkage")
table(cut_average, mails1$label)

##### Single Linkage
hc.single = hclust(dist_mat_cosine, method="single")
# plot(hc.single, main="Single Linkage", labels = FALSE)
cutSgl <- cutree(hc.single, k = 2)
single.dend <- as.dendrogram(hc.single)
single.dends <- color_branches(single.dend, k = 2)
plot(simple.dend, main = "Single Linkage")
table(cutSgl, mails1$label)

##### Ward Linkage
hc.ward = hclust(dist_mat_cosine, method="ward.D2")
cutWard <- cutree(hc.ward, k = 2)
ward.dend <- as.dendrogram(hc.ward)
ward.dends <- color_branches(ward.dend, k = 2)
plot(ward.dends, main = "Ward Linkage")
table(cutWard, mails1$label)




