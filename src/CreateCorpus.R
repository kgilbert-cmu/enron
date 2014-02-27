#############################################
## Code was used to create the enronCorpus 
## Last modified: Feb 26, 2014
#############################################

rm(list=ls())

## Files enronwords.txt and vocab.enron.txt are found in 
## Professor Nugent's webpage. Download and save in your
## working directory.

## Read simple triplet matrix
stm <- read.table(file = "enronwords.txt", col.names = c("i","j","v"))
head(stm)
dim(stm)
## Number of documents:
length(unique(stm$i))
## Number of terms:
length(unique(stm$j))
max(unique(stm$j))

## Read words key
voc <- as.character(read.table(file = "vocab.enron.txt")$V1)
length(voc)

## Substitute the j column for the actual word
stm$j <- voc[stm$j]
head(stm)

## Create list with emails:
## *** Note *** 
## On a decent computer it should take <30 mins to run
enron <- list()
N <- length(unique(stm$i))
start <- proc.time()[3]
for (ii in 1:N) {
	if (ii %% 200 == 0) {
		cat("Progress: doc. # ", ii, "\n", sep="")
	}
	doc <- stm[which(stm$i == ii),]
	enron[[ii]] <- paste(rep(doc$j, times = doc$v), collapse = " ")
	rm(doc)
}
end <- proc.time()[3] - start
cat("Elapsed time: ", signif(end/60, digits = 2), " min. \n", sep="")

## Make corpus
library(tm)
enronCorpus <- Corpus(VectorSource(enron))
load(file = "enronCorpus.RData")

q(save = "no")