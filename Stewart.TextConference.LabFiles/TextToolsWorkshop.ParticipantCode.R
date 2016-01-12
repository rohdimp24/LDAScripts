######################################################################################
# Tools for Text Workshop
# Unsupervised Learning in R Tutorial
# Brandon Stewart (bstewart@fas.harvard.edu)
# *Additional code graciously supplied by Justin Grimmer (Stanford)
# This is accompanied by a worksheet which provides additional details and citations.
######################################################################################
#Version: This is the preliminary participant version with limited preprocessing

rm(list=ls(all=TRUE)) #Clear out our workspace

####
####
## Exercise 1: Simple K-Means for Participant Papers
####
####


##STEP 1: Import and Inspect the Data

##Import and minimally format the Document-Term Matrix
file <- read.csv("english_count.csv")      #Read in the matrix, open this in Excel if you are interested.
papersDTM <- as.matrix(file[,-1])          #This removes the filenames from the first column
rownames(papersDTM) <- file[,1]            #This adds the filenames as row names 
filenames <- as.character(file[,1])        #This adds an extra vector that we will use later for labeling
rm(file)                                   #Removes this file that we no longer need

##Look at the two items that we have in our workspace
head(papersDTM)                            #Our Document-Term Matrix
head(filenames)                            #Our List of Filenames


##STEP 2: Choose a Representation for the DATA

##Standardize the document lengths
rowTotals <- apply(papersDTM, 1, sum)      #Find the sum of words in each Document
input <- papersDTM/rowTotals                   #Divide each row by those totals


##STEP 3: Choose a Model

set.seed(12345)                            #This sets the random seed.

##Now we run the model using kmeans()
results <- kmeans(input,                   #Our input term document matrix
                  centers=10,              #The number of clusters
                  nstart=25)               #The number of starts chosen by the algorithm (Don't worry about this)


##STEP 4: Label your Clusters

##Look at Most Informative Words  (Don't worry about this too much: see reference sheet)
for (i in 1:length(results$withinss)) {    
  inGroup <- which(results$cluster==i)     #For each cluster, this defines the documents in that cluster
  within <- papersDTM[inGroup,]            
  if(length(inGroup)==1) within <- t(as.matrix(within)) #This is a formatting correction when there is only one doc in a cluster
  out <- papersDTM[-inGroup,]              
  words <- apply(within,2,mean) - apply(out,2,mean) #Take the difference in means for each term
  print(c("Cluster", i), quote=F)
  labels <- order(words, decreasing=T)[1:20] #Take the top 20 Labels
  print(names(words)[labels], quote=F)     #From here down just labels
  if(i==length(results$withinss)) { 
    print("Cluster Membership")
    print(table(results$cluster))
    print("Within cluster sum of squares by cluster")
    print(results$withinss)
    }
  }

##Read some documents

clusterNum <- 1          #Set the Cluster Number you want to look at
#See list of all documents
filenames[which(results$cluster==clusterNum)]
#See a random sample of one document
filenames[which(results$cluster==clusterNum)][sample(sum(results$cluster==clusterNum),1)]

##STEP 5: Validate!










###
###
# Exercise 2: Literature Review Using Latent Dirichlet Allocation (Grimmer 2010)
###
###

rm(list=ls(all=TRUE)) #Clear out our workspace


##STEP 1: Import and Inspect the Data

#Load the Files
load("Exercise2.RData")
library(lda)
library(ggplot2)

#Take a look at the files
summary(ldadata)            #This is a strange format needed for this model.
head(file.order)            #This is our normal Term-Document Matrix
head(data.order)            #This is information on the articles we are analyzing 


##STEP 2: Choose a Representation for the DATA

#This is assumed in our model, so we move to step 3



##STEP 3: Choose a Model

set.seed(12345)                    #This sets the random seed

#Now we run the Latent Dirichlet Allocation Model
K <- 50
result <- lda.collapsed.gibbs.sampler(
                                       ldadata$documents,  #This is the set of documents
                                       K,                 #This is the number of clusters
                                       ldadata$vocab,      #This is the vocab set
                                       25,                 #These are additional model parameters
                                       0.1,
                                       0.1)


##STEP 4: LABEL

##A built in labeling function for this model
top.words <- top.topic.words(result$topics, 5, by.score=TRUE) #The "5" is the number of labels
top.words 


##This is a graphical display from the lda() package tutorial.  Note that the code was written by Jonathan Chang the package's author not myself.

N <- 10 #Choose the number of documents to display

topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
topic.proportions <- topic.proportions[sample(1:dim(topic.proportions)[1], N),]
topic.proportions[is.na(topic.proportions)] <-  1 / K
colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")
topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
                                    document=factor(1:N)),
                              variable_name="topic",
                              id.vars = "document")  
qplot(topic, value, fill=document, ylab="proportion",
       data=topic.proportions.df, geom="bar") +
   opts(axis.text.x = theme_text(angle=90, hjust=1)) +  
   coord_flip() +
   facet_wrap(~ document, ncol=5)














#################################################################################

###
###
# Exercise 3: Expressed Agenda
###
###

##################################################################################


rm(list=ls(all=TRUE)) #Clear out our workspace


##STEP 1: Import and Inspect the Data

#Load the Files
load("Exercise3.RData")
library(MCMCpack)

#Take a look at the files
author  #A listing of the authors of the documents and which index they start and end at
head(filenames) #A listing of file names
head(pressTDM) #Our typical term-document matrix




##STEP 2: Choose a Representation for the DATA

#This is assumed in this version of the model.  Other versions of the model will take different formats.



##STEP 3: Choose a Model

set.seed(12345)                    #This sets the random seed

#Run the Model
i <- 15                            #This sets the number of categories 
results <- exp.agenda.vonmon(pressTDM, author, i)


##STEP 4: LABEL


words <- colnames(pressTDM) #create a separate vector of word titles

#Assign Labels (Don't worry about this too much)
x <- c()
for (j in 1:i) {
	temp <- words[order(results$mus[,j] - apply(results$mus[,-j], 1, mean), decreasing=T)[1:20]]
	x <- rbind(x, temp)
}
rownames(x) <- NULL
t(x)   #Displays our Labels
categories <- x


#Show the Expressed Priorities by Simulating from the Posterior
hist(rdirichlet(10000, 
                 alpha=results$thetas[2,])[,1], #The first index "2" is the senator, the second index "1" is the topic number
                 main="Posterior Distribution of Clinton2007 on Topic 1") #Label

#Assign Each Document to Its Categories and add its FileName
topics<- apply(results$rs, 1, which.max)
names(topics) <- filenames

##Optionally remove the comment sign to write out a spreadsheet of results.
#write.csv(topics, file = "TopicModels.csv", quote = FALSE)













