# analysis of database

# libraries ----
library(vecsets)
# text mining packages; these are a big install, only do it if you need them  
# Needed <- c("tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE) 
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
library(SnowballC)
library(RColorBrewer)
library(ggplot2)
library(tm)
library(wordcloud)
library(biclust)
library(cluster)
library(igraph)
library(fpc)
library(Rcampdf)
library(cluster)  
library(fpc)
library(igraph)
library(EcoSimR)    #EcoSimR library only used for looking at co-occurance, not in paper

# read in data 
data <- read.csv(file = "/home/rachael/Dropbox/Papers/accepted/LSI_pointing_stats/methods.csv",
                 sep = "\t", na.strings=c("","NA"), header = T) 
data <- read.csv(file = "/Users/Rachael/Dropbox/Papers/accepted/LSI_pointing_stats/methods.csv",
                 sep = "\t", na.strings=c("","NA"), header = T) 
journals <- read.csv(file = "/home/rachael/Dropbox/Papers/accepted/LSI_pointing_stats/journals.csv",
                     sep = "\t", na.strings=c("","NA"), header = T)
head(data)
head(journals)

individualMethods <- read.csv(file="Dropbox/Papers/accepted/LSI_pointing_stats/IndividualMethods.csv", header = F)
head(individualMethods)

# general analysis and summery of data ----

# count the number of article w/ and w/out stats by journal 
hadStatsbyJournal <- table(data$Journal, data$No.stats)
sort(hadStatsbyJournal[,2]) #journals with many qualitative studies at end

# all papers with no statistics 
noMethods <- data[data$No.stats == "TRUE",]

# select all papers with only discriptive stats
discriptOnlyPapers <- intersect(data[is.na(data$Modelling),4], 
                                data[is.na(data$Inferential),4])
discriptOnlyPapers <- intersect(discriptOnlyPapers,
                                data[!is.na(data$Discriptive),4])
discriptOnlyPapers <- data[is.element(data$Article, discriptOnlyPapers),]
dim(discriptOnlyPapers)[1] #number of disct. only papers
sort(table(discriptOnlyPapers$Journal)) # journals that publish them

# select papers with both discriptive and inferential stats
discAndInf <- intersect(data[!is.na(data$Inferential),4], 
                        data[!is.na(data$Discriptive),4])
discAndInf <- data[is.element(data$Article, discAndInf),]
dim(discAndInf)[1] #number of disct + inferential papers
sort(table(discAndInf$Journal)) # journals that publish them

# modelling papers
modelling <- data[!is.na(data$Modelling),]
dim(modelling)[1] #number of modelling papers
sort(table(modelling$Journal)) # journals that publish them

# table of sorts of methods 
methodTypesByJournal <- cbind(qualitative = table(noMethods$Journal),
      discriptive = table(discriptOnlyPapers$Journal),
      inferential = table(discAndInf$Journal), 
      modelling = table(modelling$Journal))
methodTypesByJournal <- data.frame(methodTypesByJournal)
colSums(methodTypesByJournal) #oveall use of stats
barplot(colSums(methodTypesByJournal))
colSums(methodTypesByJournal)/sum(methodTypesByJournal)

# now let's sort types of methods by subfield
subfields <- levels(journals$Subfield)
# print a summary table journals in each subfield, as well as counts and percentages
for(i in 1:length(subfields)){
  listOfJournals <- journals[journals$Subfield == subfields[i],]
  summaryTable <- methodTypesByJournal[is.element(rownames(methodTypesByJournal), listOfJournals$Journal),]
  print(subfields[i])
  print(summaryTable)
  print("Total counts:")
  print(colSums(summaryTable))
  print("Percent by type:")
  print((colSums(summaryTable)/sum(summaryTable))*100)
}
# now just the summary tables
bySubfield <- NULL
for(i in 1:length(subfields)){
  listOfJournals <- journals[journals$Subfield == subfields[i],]
  summaryTable <- methodTypesByJournal[is.element(rownames(methodTypesByJournal), listOfJournals$Journal),]
  rbind(c(subfields[i], NA, NA, NA, NA), summaryTable)
}

for(i in 1:length(subfields)){
  listOfJournals <- journals[journals$Subfield == subfields[i],]
  print(listOfJournals$Journal)
}

# how many papers use some form of regression?
regressionList <- grepl("*regression*", data$Inferential)
linerList <- grepl("*linear*", data$Inferential)
lmerList <- grepl("*lmer*", data$Inferential)

tableOfLm <- cbind(regressionList, linerList, lmerList) 
count(apply (tableOfLm, 1, any)) # count of rows with at least one serach term

# Common methods by subfield
phoneticsJournals <- journals[journals$Subfield == "Syntax",]$Journal
for(i in 1:length(phoneticsJournals)){
  print(data[data$Journal == phoneticsJournals[i],])
}

# text mining ----
# looking at all the different individual methods using text mining I'm
# partially following this guide:
# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html

docs <- Corpus(DirSource("/home/rachael/Dropbox/Papers/accepted/LSI_pointing_stats/Untitled Folder/"))
docs <- Corpus(DirSource("C:/Users/Rachael/Dropbox/Papers/accepted/LSI_pointing_stats/Untitled Folder/"))
summary(docs)
inspect(docs[2])

# preprocessing. This is where I'm concatinating terms
for (j in seq(docs))
{
  docs[[j]] <- gsub("linear model", "lm", docs[[j]])
  docs[[j]] <- gsub("sd", "stand_deviation", docs[[j]])
  docs[[j]] <- gsub("tests", "test", docs[[j]])
  docs[[j]] <- gsub("models", "model", docs[[j]])
  docs[[j]] <- gsub("means", "mean", docs[[j]])
  docs[[j]] <- gsub("t test", "t_test", docs[[j]])
  docs[[j]] <- gsub("wilcoxons", "wilcoxon", docs[[j]])
  docs[[j]] <- gsub("anovas", "anova", docs[[j]])
  docs[[j]] <- gsub("correlations", "correlation", docs[[j]])
  docs[[j]] <- gsub("pearsons", "pearson", docs[[j]])
  docs[[j]] <- gsub("ratios", "ratio", docs[[j]])
  docs[[j]] <- gsub("regressions", "regression", docs[[j]])
  docs[[j]] <- gsub("samples", "sample", docs[[j]])
}

# text pre-processing to clean up analysis 
docs <- tm_map(docs, removePunctuation)  # remove punc.
docs <- tm_map(docs, removeNumbers) # remove numbers
docs <- tm_map(docs, tolower)   # to lower case
#docs <- tm_map(docs, stemDocument)   # remove morphology
docs <- tm_map(docs, removeWords, stopwords("english"))   # remove funciton owrds
docs <- tm_map(docs, stripWhitespace)   # remove extra spaces
docs <- tm_map(docs, PlainTextDocument)  # treat as texts docs

dtm <- DocumentTermMatrix(docs)  # this is a data term matrix
dtm

freq <- colSums(as.matrix(dtm)) # frequency of terms  
ord <- order(freq)  
sort(freq) # most frequent terms are at the end
freq[tail(ord)]   # most common terms
multipleOcurrance <- freq[freq[] > 1] # all terms which occur more than once
methodsMultipleOccurance <- names(multipleOcurrance) #vector of the names of all methods used more than once. 


dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)
freq1 <- colSums(as.matrix(dtms))   # ferquency after removing sparse terms
freq1
freq2 <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq2, 50) # fifty most common (non sparse) terms

# word clouds! 
set.seed(142)   
wordcloud(names(freq), freq, min.freq=5, )   

set.seed(142)   
wordcloud(names(freq), freq, min.freq=5, scale=c(8, 1), colors=brewer.pal(6, "Dark2"))   

# making cooccurance matrix ----
# ok, now we want to go through and make a column for every repeated term
# then, using fuzzy matching, find the rowqs that contain each term and put that info in a new column
# finally, use the columns we've just made to make a co-occurance thigns
# and do some visualizations

# fucntion to trim leading and following white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# some pre-processing for cleaner results
data$allMethods <- gsub("sd", "standard_deviation", data$allMethods)
data$allMethods <- gsub("s ", " ", data$allMethods)
data$allMethods <- removePunctuation(as.character(data$allMethods))
data$allMethods <- removeNumbers(as.character(data$allMethods))
data$allMethods <- tolower(as.character(data$allMethods))
methodsMultipleOccurance <- unique(methodsMultipleOccurance)

# if stemming
data$allMethods <- stemDocument(as.character(data$allMethods))
methodsMultipleOccurance <- unique(stemDocument(as.character(methodsMultipleOccurance)))
  
# now create a matrix which looks at each term 
x = rep(1:length(data$allMethods))
matrixForCocurrance = rep(1:length(data$allMethods))
for(i in 1:length(methodsMultipleOccurance)){
  y <- grep(paste("*",methodsMultipleOccurance[i],"*", sep = ""), data$allMethods) #look for occurance fo string 
  newCol <- x %in% y # set all the rows that had a match to "true", those that iddn't to flase
  matrixForCocurrance <- cbind(matrixForCocurrance, newCol) # add data 
  colnames(matrixForCocurrance)[i +1] <- methodsMultipleOccurance[i] # rename column with term
}
# if everything's going right, there should be no coulmn with a max of 0 in the summary:
table(matrixForCocurrance)

# write this puppy to disk so we can mess with it later 
write.table(matrixForCocurrance, file = "/home/rachael/Dropbox/Papers/accepted/LSI_pointing_stats/matrixForCocurrance")

# visualize co-vatiation -----
# this code taken from http://planspace.org/2013/01/30/visualize-co_occurrence/
topMatrix <- matrixForCocurrance[, colSums(matrixForCocurrance) > 10]
topMatrix <- topMatrix[,2:dim(topMatrix)[2]]

total_occurrences <- colSums(matrixForCocurrance)
data_matrix <- as.matrix(matrixForCocurrance)
co_occurrence <- t(data_matrix) %*% data_matrix

total_occurrences2 <- colSums(topMatrix)
data_matrix2 <- as.matrix(topMatrix)
co_occurrence2 <- t(data_matrix2) %*% data_matrix2

# visualization 
graph <- graph.adjacency(co_occurrence,
                         weighted=TRUE,
                         mode="undirected",
                         diag=FALSE)
graph2 <- graph.adjacency(co_occurrence2,
                         weighted=TRUE,
                         mode="undirected",
                         diag=FALSE)
plot(graph)
plot(graph2)
# plot with curved edges
plot(graph2, 
     vertex.size=total_occurrences2 * .1, 
     edge.width=E(graph2)$weight * .1,
     main = "Co-occurance of Common Statistical Methods", 
     vertex.color = "plum2", 
     vertex.label.cex = 1.1, 
     vertex.label.dist = .4, 
     vertex.label.color= "black", 
     edge.curved = T,
     margin = -0.01)
# plot with streight edges
plot(graph2, 
     vertex.size=total_occurrences2 * .1, 
     edge.width=E(graph2)$weight * .1,
     main = "Co-occurance of Common Statistical Methods", 
     vertex.color = "plum2", 
     vertex.label.cex = 1.1, 
     vertex.label.dist = .4, 
     vertex.label.color= "black",
     margin = -0.01)

# messing around with graph stuff
subcomponent(graph2, "anova")
subcomponent(graph2, "sum")
is_separator(graph2, "model")
sapply(colnames(topMatrix), FUN = is_separator, graph = graph2)
length(colnames(topMatrix))

limin_separators(graph2)
articulation_points(graph2) # there are no articulation points, so the vertex connectivity is at least 2
graph.cohesion(graph2)
vertex.connectivity(graph2)

# this takes a while to load, interactive plot
tkplot(graph2, vertex.color = "green", 
       vertex.size=total_occurrences2 * .5, 
       edge.width=E(graph2)$weight * .2)

# quantifying covaritation ----
# ok, now that we have a high-level idea of what's going on in terms of co-occurance, let's quantify it a bit
# this is based on https://cran.r-project.org/web/packages/EcoSimR/vignettes/CoOccurrenceVignette.html

# first lets rank the most connected edges
diag(co_occurrence2) <- 0 #number of cooccurences with self set to 0
max(co_occurrence2) 

# function to change numbers to that number item in a list
rename <- function(list, num){
  newName <- list[num]
  return(newName)
}
matrixNames <- colnames(co_occurrence2) # row and column names
triIndex <- which(lower.tri(co_occurrence2), arr.ind = T) #index of all items in the lower half of the matrix
countsOfCooccur <- co_occurrence2[lower.tri(co_occurrence2)] # value of all the items in the lower half of the matrix
cooccurSummary <- cbind(triIndex, countsOfCooccur)
cooccurSummary[,1] <- rename(matrixNames, cooccurSummary[,1])
cooccurSummary[,2] <- rename(matrixNames, as.integer(cooccurSummary[,2])) # doesn't work without cast to int

OrderOfCoccur<- cooccurSummary[order(-countsOfCooccur),] # descending order of commonness of cooccurance 
head(OrderOfCoccur)

# Ignore this next bit. It works but not great. 
# matrixNames <- colnames(co_occurrence2) # row and column names
# n <- length(halfMatrix) 
# newMatrix <- NULL
# 
# # find and return the 20 most commong items
# # this loop returns a matrix of the 20 most common items with thier counts
# while(n >= (length(halfMatrix)-20)){ #
#   newItem <- sort(halfMatrix,partial=n-1)[n-1] # finds the next highest value inthe matrix 
#   n <- n-1 # iterate n so we don't get stuck forever
#   x <- which(halfMatrix == newItem, arr.ind = TRUE)
#   x[1:length(x)] <- rename(matrixNames, x)
#   x <- cbind(x, rep(newItem, dim(x)[1]))
#   newMatrix <- rbind(newMatrix, x)
# }
# newMatrix <- newMatrix[!duplicated(newMatrix), ] #remove duplicate rows
# head(newMatrix)

v_ratio(topMatrix) # this calculates "average covariance in association between all possible pairs" 
v_ratio(matrixForCocurrance) # In a random matrix, this ratio should be close to 1.0, 
# The V-ratio will equal the minimum value of 0.0 when all sites contain identical numbers of species

c_score(topMatrix)
c_score_var(topMatrix)
c_score_skew(topMatrix)
c_score(matrixForCocurrance)

myModel <- cooc_null_model(speciesData=topMatrix,suppressProg=TRUE)
summary(myModel)
plot(myModel,type = "hist")
plot(myModel,type = "cooc") 
plot(myModel,type = "burn_in")

plot(topMatrix)
checker(topMatrix)

# getting articles for specfic methods ----
# part of cirriculum design 

methodsForClass <- read.csv("Dropbox/Papers/accepted/LSI_pointing_stats/listOfMethodsForClass", sep = "\t",
                              header = T)
methodsForClass
head(methodsForClass)
journalList <- data.frame(Article=character(), 
                          Authors=character(),
                          Journal=character(),
                          Method=character(), 
                          stringsAsFactors=FALSE) 
for(i in 1:length(methodsForClass$InDatabase)){
  term <- paste("*", methodsForClass$InDatabase[i], "*", sep = "")
  
  # serach for method
  findList <- grepl(term, data$allMethods)
  if(sum(findList) > 0 ){
    journalListAdd <- cbind.data.frame(Article = data$Article[findList],
                                       Authors = data$Author.s.[findList],
                                       Journal = data$Journal[findList],
                                       Method= methodsForClass$Term[i])
    journalList <- rbind(journalList, journalListAdd)
  }
  else{
    print("Error, term not found:")
    print(term)
  }
}

head(journalList)
dim(journalList)

write.csv(journalList, "PapersByMethods.csv")
