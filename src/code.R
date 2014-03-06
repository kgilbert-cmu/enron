# I am an R code file. Please put code in me.
enronwords <- read.table("data/enronwords.txt", quote="\"")
# each row of enronwords is a tuple (x,y,z)
# x is the document number (which email it is from)
# y is the word itself (cross-reference with 'vocab.enron.txt')
# z is how many times the word y appeared in email x

library(tm); library(slam)
stm = as.simple_triplet_matrix(enronwords) # slam
DocumentTermMatrix(stm, control=list(weighting=weightTfIdf)) # tm

stringify = function(df) {
    paste(rep(df[2], df[3]), collapse=" ")
}
strings = apply(enronwords,1,stringify)
docify = function(document_number) {
    paste(strings[which(enronwords[,1] == document_number)], collapse=" ")
}
docs = lapply(1:39861, docify)
corp = Corpus(VectorSource(docs))
dtm_test = DocumentTermMatrix(corp)

library(parallel)
library(tm)
library(slam)
library(skmeans)
clist = list(weighting=weightTfIdf, stemming=T, removeNumbers=T, removePunctuation=T)
dtm = DocumentTermMatrix(enronCorpus,control=clist)
save(dtm, file="dtm.RData")

wordList = c("skill", "legal", "rick", "meet")
x = findAssocs(dtm,wordList, rep(.1, length(wordList)))
write(x, "wordAssocs.txt", append=T)

get = function(wordList,dtm) {
    i = dtm[,stemDocument(wordList)]$i
    rows = unique(i[which(table(i) > 1)])
    return(rows)
}

redoc = function(docList) {
    return(DocumentTermMatrix(enronCorpus[docList], clist))
}

words = function(num, wordList) {
    return(rev(sort(col_sums(redoc(get(wordList, dtm)))))[1:num])
}
