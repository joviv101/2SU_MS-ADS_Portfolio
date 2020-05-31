#bnew libraries
install.packages("AMR")
install.packages("sentimentr")
#libraries
library(tidytext)
library(tidyverse)
library(stringr)
library(plotly)
library(dplyr)
library(plyr)
library(AMR)
library(wordcloud)
library(slam)
library(sentimentr)
library(DT)
library(reshape2)
library(arules)
library(tm)
library(rpart)
library(rpart.plot)
library(caret)
#Import wine data
?
setwd("C:/Users/Brittney Sherman/OneDrive - Syracuse University/IST 707 Group 2/Input")

Wine<- read.csv("winemag-data-130k-v2.csv")

#remove 1st col
Wine<-Wine[,-1]

#remove special chars
Wine$designation<- str_replace_all(Wine$designation, "[^a-zA-Z0-9]", " ")


##Fix the rest of the cols
for (i in names(Wine)) {
  Wine[,i]<- str_replace_all(Wine[,i], "[^a-zA-Z0-9]", " ")
}

Wine$description<- as.character(Wine$description)
### Visualize scores


#min(as.numeric(Wine$points))
#80
#max(as.numeric(Wine$points))
#100


#hist of  scores
plot_ly(Wine, type = "histogram", x = as.numeric(Wine$points))

#hist ofcountries
plot_ly(Wine, type = "histogram", x= Wine$country)


#how many unique regions?
unique(Wine$province)
#yikes 426


#how many varieties
unique(Wine$variety)
#big yikes -> 700+

## different reviewers??
unique(Wine$taster_name)
#oh man only 20??
plot_ly( Wine, x= Wine$taster_name, type = "histogram")


### I want to see if the wine reviewers have favorite wineries
#quick list of the different tasters
Tasters<- unique(Wine$taster_name)

#Percent function instead of another library https://stackoverflow.com/questions/7145826/how-to-format-a-number-as-percentage-in-r
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#for loop
#for each of the names in Tasters it will create a DT table
#with the top 5 most reviewed wineries for each
for(i in Tasters){
  a<-Wine %>%
    select(taster_name, winery)%>%
    filter(taster_name == i)%>%
    freq(winery)
  a$percent<-percent(a$percent)
  print(datatable(a[1:5,1:3], colnames = c("Winery", "Count", "Percentage"),  options = list(dom = "t", ordering=F), caption = paste(i,"'s top 5 wineries")))
}


#frequencies ft. AMR 
WineFrequencies<-(Wine %>%
  freqvariety))
#maybe just look at top 50 wines????
#not confident in training models with less than 200 samples

WineMenu<-data.frame((WineFrequencies$item[WineFrequencies$count > 270]))
WineMenu<- as.character(WineMenu$X.WineFrequencies.item.WineFrequencies.count...270..)

#prime a list before loop
#rm(t)
t<-setNames(vector("list", length(WineMenu)), WineMenu)

#loop to get all the rows with our data
for (i in (WineMenu)) {
t[[i]]<-which(Wine$variety == i)
}
#Unlist all the row numbs
T<-unlist(t)

#isolate the 61 wines that are cool
WineTasting<-Wine[T,]

#remove uneeded stuff
rm(t)
rm(WineFrequencies)
rm(i)
rm(T)

################## Clean up the reviews. #####################################

#remove punctuation
WineTasting$description<-gsub("[[:punct:]]", "", WineTasting$description)
#remove Numbers
WineTasting$description<-removeNumbers(WineTasting$description)
#to lower case
WineTasting$description<- tolower(WineTasting$description)


words<-c("wine", "now", "flavors", "notes", "the", "one", "will", "also", "give", "like", "drink", "with", "it s", "there s", "to", "this", "in", "and", "a", "is","the", "this", "to", "in", "by", "on","it")


WineTasting$description<-removeWords(WineTasting$description, words )
WineTasting$description<-removeWords(WineTasting$description, stopwords("en") )
WineTasting$description<- removeWords(WineTasting$description, WineMenu)#hold up these are still upper case gotta go to lower to remove from the descriptions
WineTasting$description<- stripWhitespace(WineTasting$description)
WineMenuLower<- tolower(WineMenu)

WineTasting$description<-removeWords(WineTasting$description, WineMenuLower )


wordcloud(WineTasting$description, max.words = 100)
#neat!
WineTasting$description[1:10]

#######################################
#sentiment analysis
#sentimentr package
#because there are a lot of words it needs to run through get_Sentences first


Feels<-sentiment_by(get_sentences(WineTasting$description))

#most negative review:
WineTasting[71007,]
Wine[117007,2]

#most positive review
WineTasting[51237,]
Wine[22364,2]





#############################
WineCount<-Wine %>%
  group_by(variety) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
WineCount<- WineCount[order(WineCount$count, decreasing = TRUE),]

Top60<- WineCount$variety[1:60]
Top60<-rev(Top60)



plot_ly(WineCount, y=WineCount$variety[1:60], x=WineCount$count[1:60], type = "bar", marker = list(color = "maroon"))%>%
  layout(yaxis = list(categoryorder = "array",
      categoryarray = Top60 )
  )


################### Code from Erin  
## Check for missing values
missing_data <- colSums(is.na(Wine) | Wine == "" | Wine == " ")
#Create a data frame with missing values, expected values, and percent missing
all_data <- c(rep.int(129971, 13))
percent_missing <- (missing_data/all_data)*100
df_missing <- data.frame("Missing" = missing_data, "Expected" = all_data, "PercentMissing" = percent_missing )
#visualize missing
##
plot_ly(df_missing, x= row.names(df_missing), y=df_missing$PercentMissing, type="bar", marker = list(color = "maroon"))


############################
#counting strings
# Trying to figure out a way to do AR when you have completely different lengthed
#reviews. The shortest Review was just 3 words.
#this code is counting the number of spaces (and presumably words) to get an 
#idea of how the reviews are structured


head(str_count(WineTasting$description, pattern = " "))
#lol what a terrible review
min(str_count(WineTasting$description, pattern = " "))
which(str_count(WineTasting$description, pattern = " ") == 3)
WineTasting$description[17691]

## woah that's long
max(str_count(WineTasting$description, pattern = " "))
which(str_count(WineTasting$description, pattern = " ") ==154)
WineTasting$description[26976]

## Hold up
## tidy text can remove JUST adverbs and adjectives
## https://stackoverflow.com/questions/55463594/how-to-extract-adjectives-and-adverbs-from-vector-of-text-in-r

unnest_tokens(tibble(txt = WineTasting$description[1]), word, txt)%>%
  left_join(parts_of_speech)%>%
  filter(pos %in% c("Adjective", "Adverb"))%>%
  pull(word)%>%
  unique
#cool, so were going to build a DF as an attempt for rule mining
# col 1 is going to be the wine
#cols 2 are going to be adjectives

MiningDF<-data.frame(Wine$variety)

unnest_tokens(tibble(txt = Wine$description[1]), word, txt)%>%
  left_join(parts_of_speech)%>%
  filter(pos %in% c("Adjective", "Adverb", "Noun"))%>%
  pull(word)%>%
  unique
## tried below with just the sort it didn't like it and it was ugly
#custom function:
gimmeDescriptions<- function(textIN){
  unnest_tokens(tibble(txt = textIN), word, txt)%>%
    left_join(parts_of_speech)%>%
    filter(pos %in% c("Adjective", "Adverb", "Noun"))%>%
    pull(word)%>%
    unique
}

##prime list before loop is run otherwise it wont work
#in this case it doesn't matter if you set the length right
#the list name is the number so it will add it
#making a list of just the reviews
###VERY TIME CONSUMING DO NOT RUN UNLESS YOU MEAN IT. Like for real 3hrs.
s<-c("Q")

for (i in rownames(Wine[3])) {

  s[i]<-paste((gimmeDescriptions(as.character(Wine[i,3]))), collapse = " ")
  
  
}
#this is not going to work because there are different length of terms for each review,
#the format needs to be more similar to the Kumar groceries example where
#there is a col to indicate the transaction number and a col for the data
#to get this to work we *Still* need to extract adjectives and adverbs, 
#as well as the Wine Variety and then store them in this type of structure 
#so they can be read in as a single transaction with the arules package

#first order of business, how to get these items into that list format

#this works to get the items Vertical, Still has col name which I don't need
View(melt(gimmeDescriptions(as.character(Wine[1,2]))))

#try and use a loop to get these stored into a vector
#the row can be assigned to a variable
#Then the rownumber (i) can be sampled with replacement to
#fill the transaction names
vert<-melt(gimmeDescriptions(as.character(Wine[1,2])))

cbind(sample(1,  length(vert[,1]), replace =T ),melt(gimmeDescriptions(as.character(Wine[1,2]))))
#subset of wine because I'm tired of crashing R
SubbedWine<- Wine[1:30,]

rm(t)
t<-vector(mode = "list", length = length(SubbedWine[,1]))


for (i in rownames(SubbedWine)) {
  
  vert<-melt(gimmeDescriptions(as.character(SubbedWine[i,2])))
  
  t[[i]]<-cbind(sample(i,  length(vert[,1]), replace =T ), melt(gimmeDescriptions(as.character(SubbedWine[i,2]))))
  
 
}

# unlist it now

t<-(rbind.fill((t)))

write.csv(t,"wineTrans.csv")


#import as transactions hopefully

trans<- read.transactions("wineTrans.csv",
                          header = FALSE,
                          rm.duplicates = FALSE,
                          format = "single",
                          sep = ",",
                          cols = c(2,3))

rules<-apriori(trans)
arules::inspect(rules)

#FML, they are all literally "the", "this", "to", "in", "by" "on"
#going back to remove these, omg
#Also going to add the wine varieties into the dataset to see if the rhs can be constrained.
#going to try above with WineTastingDF that has been text cleaned of stopwords and other nonsense

DescVariety<- c("description", "variety")
WineTasting<- WineTasting[,DescVariety]
#ideally we would be able to hold the wines on the RHS of the equations, right now they have spaces
#remove spaces so that we would be able to set them to the RHS
#cool, that is ridiculously time consuming but neat
#fix the variety col
#lowercase and no spaces in the names
WineTasting$variety<- gsub(" ", "", tolower(WineTasting$variety))
#combine the cols and
rm(descriptions)
descriptions<-c("Q")


for (i in rownames(WineTasting)) {
  descriptions[i]<- do.call(paste, c(WineTasting[i,c(1,2)]))
  
}
do.call(paste, c(WineTasting[i,c(1,2)]))


melt(gimmeDescriptions(descriptions[2]))
#lawd 3 days later...
#write descriptions to csv so I don't have to run the ugly code again

write.csv(descriptions, "combinedDescriptionVariety.csv")

#okay, so now we are going to run the code from above:
#try and use a loop to get these stored into a vector
#the row can be assigned to a variable
#Then the rownumber (i) can be sampled with replacement to
#fill the transaction names

rm(t)
t<-setNames(vector(mode = "list", length = length(descriptions[2:129972])), 2:129972)


for (i in 1:NROW(descriptions[2:129972])) {
  
  vert<-melt(gimmeDescriptions(as.character(descriptions[i])))
  
  t[[i]]<-cbind(rep(i, each=length(vert[,1])), melt(gimmeDescriptions(as.character(descriptions[i]))))
  
  
}

# unlist it now

t<-(rbind.fill((t)))

write.csv(t,"wineTrans1.csv")


#import as transactions

trans<- read.transactions("wineTrans1.csv",
                          header = FALSE,
                          rm.duplicates = FALSE,
                          format = "single",
                          sep = ",",
                          cols = c(2,3))

rules<-apriori(trans)
arules::inspect(rules)


head(backup)




#maybe rule mining who knows at this point
WineDesc<-read.csv("wineAdjectives.csv")
WineDesc<- data.frame(WineDesc[,-1])
#add wine type
WineDesc$type<- as.factor(Wine$variety)




############## Visuals for paper ################
countCountry<-count(Wine$country)
countCountry<-countCountry[order(countCountry$freq, decreasing = T),]
plot_ly(countCountry, y = as.character(countCountry[1:5,1]), x= countCountry[1:5,2] , type = "bar",
        marker = list(color = "maroon")) %>%
  layout(title = "Number of Reviews per Country",
         yaxis = list( title ="Country"),
         xaxis = list( title ="Count"))

plot_ly(Wine, x= (as.numeric(Wine$points)), marker = list(color = "maroon"))%>%
  layout(title = "Wine Ratings",
         xaxis = list(title = "Rating"))

############################DT########################
#decison tree 
#data: sentiment, price, reviewer, country, variety, 

#create col in wine tastin for sentiment
Wine$fsentiment<- (sentiment_by(get_sentences(Wine$description)))[,4]
sel<-c(4,1,5,9,12,14)
Sent_P_R<- Wine[,sel]

str(Sent_P_R)
#need to clean up the set
#remove "" reviews
Sent_P_R<- Sent_P_R[-which(Sent_P_R$taster_name ==""),]

#want more factor data

Sent_P_R$points<-as.numeric(Sent_P_R$points)
Sent_P_R$country<-as.factor(Sent_P_R$country)
Sent_P_R$taster_name<- as.factor(Sent_P_R$taster_name)
Sent_P_R$variety<- as.factor(Sent_P_R$variety)
Sent_P_R$fsentiment<- unlist(Sent_P_R$fsentiment)

length(unlist(Sent_P_R$fsentiment))
#fill na prices with median
#make numeric first so we can get median

Sent_P_R$price<- as.numeric(Sent_P_R$price)
median(na.omit(Sent_P_R$price))
#$29 dollaridos

length(na.omit(Sent_P_R$price))
#24496/26244*100 there is about 93% of the data so fine with replacing with median
Sent_P_R$price[is.na(Sent_P_R$price)]<- median(Sent_P_R$price, na.rm = T)

#lastly remove the "" from countries
Sent_P_R<- Sent_P_R[-which(Sent_P_R$country ==""),]


## cool we are left with 103644 observations we need to sample into test and training sets of data.  lets get it.

trainRows<-sample(nrow(Sent_P_R),(dim(Sent_P_R)[1]*2/3))
train<- Sent_P_R[trainRows,]
test<-Sent_P_R[-trainRows,]

str(train)
# set aside points labels for test data
testlabels<- test[,"taster_name"]
#remove from test set
test<-test[,-4]

length(testlabels)

#DT Time
#test on smol subset of data
Tree<- rpart(taster_name~ fsentiment+variety, train[1:1000,], method = "class")
summary(Tree)
rpart.plot::rpart.plot(Tree)

TreeResults<-predict(Tree, test)

confusionMatrix(TreeResults, testlabels)
rm(Tree)
