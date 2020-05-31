##this code just shows the final cleaned wine dataframe:
#Read in winemag_data-130K csv as training data set
Wine<- read.csv("winemag-data-130k-v2.csv")

#############CLEAN DATA###########################
#Remove special characters &1st column /call it Wine_clean
for (i in names(Wine)) {
  Wine[,i]<- str_replace_all(Wine[,i], "[^a-zA-Z0-9]", " ")
}

#remove unncessary columns
Wine<-Wine[,-1] #remove ID
Wine<-Wine[,-3] #remove designations
Wine<-Wine[,-7]#remove region_2
Wine<-Wine[,-8] #remove taster twitter handle
View(Wine)

#Clean data
stopwords<-c("riesling","wine", "now", "flavors", "notes", "the", "one", "will", "also", "give", "like", "drink", "with", "it s", "there s", "to", "this", "in", "and", "a", "is")
Wine$description<-removeWords(Wine$description, stopwords )
#remove punctuation
Wine$description<-gsub("[[:punct:]]", "", Wine$description)
#remove Numbers
Wine$description<-removeNumbers(Wine$description)
#to lower case
Wine$description<- tolower(Wine$description)

Wine[Wine==""] <- NA #Change blanks to NA
Wine_veryclean <- na.omit(Wine) #remove all blanks
View(Wine_veryclean) #now have 77,267 rows with 10 columns
Wine<-as.data.frame(Wine_veryclean)