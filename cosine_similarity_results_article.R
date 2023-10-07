
# Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE)   

library(tm)
library(stringr)
library(dplyr)
library(CircStats)
library(ggplot2) 

# Reads a folder with the whole corpus. Each document is a sentence (marked here by a stop . in the text)
pinto <- VCorpus(DirSource(directory = "data/", encoding = "UTF-8"), readerControl = list(language = "pt")) # this line requires a collection of documents. Place the documents you want to work with here.


stopwords<-read.table("data/results_test2/morestop1.txt")
stopwords1<-as.vector(stopwords$V1)
stopwords1<-as.vector(stopwords$V1)

docs <- tm_map(pinto, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeWords, stopwords1)
docs <- tm_map(docs, stripWhitespace) 


# Create a matrix with documents as rows, terms as columns #

dtm <- DocumentTermMatrix(docs)   



# Term to term matrix #
# http://stackoverflow.com/questions/10622730/constructing-a-co-occurrence-matrix-from-dummycoded-observations-in-r
# https://rpubs.com/ivan_berlocher/79849

out<-dtm
out[out>1]<-1  # Count 1 occurrence by sentence, as cross prod gets matrix product, which modifies results if occur>1
out <- crossprod(as.matrix(out))  # Same as: t(X) %*% X
diag(out) <- 0       # (b/c you don't count co-occurrences of an aspect with itself)



# Tests. Find particular associations #  
# The following lines will work with the original collection of documents. Replace the terms with whatever terms you may want to experiment in your own collection #

cidade<-sort(out["cidade",], decreasing=TRUE)
ilha<-sort(out["ilha",], decreasing=TRUE)


# Compare selected prototypes for islands and cities

compare<-c( "iaoa","çamatra", "pequim", "martauão", "odiaa",  "tanixumaa")

compare<-sort(compare)
cidade1<-cidade[compare]
ilha1<-ilha[compare]
ilha1[6]<-ilha1[6]+1  #This version of the script after updating TM on the 7th of November 2023 gives one occurrence less for Tanixumaa. 
# If you are following this script reading from a paper commenting this script, you may want to keep this line to check against previous results it is corrected
# Otherwise, you can remove the last line of code above


# Calculate angles

pairs<-length(cidade1)
angles2<-c(1:length(ilha1))

for (i in 1:pairs){
  angles2[i]<-atan(ilha1[i]/cidade1[i])   # bring angle in radians
  print(deg(angles2[i]))
}

degangles<-deg(angles2) # show angles in degrees



# Calculate cosine distance among entities

deganglesd<-diag(degangles)  # Identity matrix

for(i in 1:length(degangles)){
  for(g in 1:length(degangles)){
    deganglesd[i,g]<-abs(degangles[i]-degangles[g])
  }
}
rownames(deganglesd)<- namesgr
colnames(deganglesd)<-namesgr

cosanglesd<-cos(rad(deganglesd))



