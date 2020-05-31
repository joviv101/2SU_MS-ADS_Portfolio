###################################################################
#                                                                 #
#IST 707 Term Project                                             #
#                                                                 #
#Jo Vivian                                                        #
#                                                                 #
#This project is to create a predictive model to identify wines   #   
#based on tasting notes, regions, sommelier descriptions          #
#                                                                 #
###################################################################

#clear all objects
rm(list=ls())


#install packages and open libraries
library(tm)
library(stringr)
library(dplyr)
library(arules)
library(arulesViz)
library(plotly)
library(ggplot2)
library(tidyverse)
library(caret)
library(knitr)
library(lubridate)
library(qdap)
library(wordcloud)
#install.packages('DT')
library(DT)
library(plotly)
#install.packages('wordcloud2')
library(wordcloud2)
library(RColorBrewer)
library(quanteda)
library(factoextra)
library(grid)
library(gridExtra)

##set digits to 2 for readability and set the seed for random number generator for predictability
options(digits = 2)
set.seed(100)

##################Set working directory and Read in the datasets#############################
#
#Working Directory
setwd('C:/Users/joviv/Documents/College/IST 707 Data Analytics/Final Project')
#
#
#Read in Wine dataset
filename = 'winemag-data-130k-v2.csv'
Wine = read.csv(filename, header = TRUE, na.strings = "NA")

View(Wine)
str(Wine)
summary(Wine)

##############################Clean Data#####################################################
#
## Check for missing values
missing_data <- colSums(is.na(Wine) | Wine == "" | Wine == " ")
#Create a data frame with missing values, expected values, and percent missing
all_data <- c(rep.int(129971, 14))
percent_missing <- (missing_data/all_data)*100
df_missing <- data.frame("Missing" = missing_data, "Expected" = all_data, "PercentMissing" = percent_missing )
#visualize missing
plot_ly(df_missing, x= row.names(df_missing), y=df_missing$PercentMissing, type="bar", marker = list(color = "maroon"))

## Check each numerical variable to see that it is >= 0
for(varname in names(Wine)){
  ## Only check numeric variables
  if(sapply(Wine[varname], is.numeric)){
    cat("\n", varname, " is numeric\n")
    ## Get median
    (Themedian <- sapply(Wine[varname],FUN=median))
    ##print(Themedian)
    ## check/replace if the values are <=0 
    Wine[varname] <- replace(Wine[varname], Wine[varname] < 0, Themedian)
  }
  
}

#Remove special characters
for (i in names(Wine)) {
  Wine[,i]<- str_replace_all(Wine[,i], "[^a-zA-Z0-9 ]", "")
}

#Remove ID, Designation, Taster Name, and Taster Twitter Handle columns
Wine <- Wine[ , -which(names(Wine) %in% c("taster_twitter_handle", "designation", "region_2"))]

#Check the structure of the data frame again
str(Wine)

#Convert columns to appropriate data types
Wine$points <- as.numeric(Wine$points)
Wine$price <- as.numeric(Wine$price)
Wine$ID <- as.integer(Wine$ID)
Wine$country <- as.factor(Wine$country)
Wine$description <- as.factor(Wine$description)
Wine$province <- as.factor(Wine$province)
Wine$region_1 <- as.factor(Wine$region_1)
Wine$taster_name <- as.factor(Wine$taster_name)
Wine$title <- as.factor(Wine$title)
Wine$variety <- as.factor(Wine$variety)
Wine$winery <- as.factor(Wine$winery)

View(Wine)
str(Wine)
summary(Wine)

#####################################Explore Data############################################

############Explore the Wine Varieties########################
#
#what are the top 60 varieties and how many of each?
variety.count<- Wine %>%
  group_by(variety) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
View(variety.count[1:60,1:2])
Top60<- variety.count$variety[1:60]
Top60<-rev(Top60)


#Let's face it - I'm a lush. So, if I haven't ever heard of the variety, then it's probably rare
#enough that we can just drop it from our data analysis.
#Hence, I am going to do further data exploration on the top 60 wines.

wine.data <- Wine[Wine$variety %in% variety.count[1:60,1:2][['variety']],]
#Check to make sure that worked
dim(wine.data)
names(wine.data)
#

#adding the word count to the data analysis data frame
wine.data$Word_count = sapply(gregexpr("\\S+",wine.data$description),length)

#
#Function to calculate word frequencies
FUNC.word.freq <- function(col){
  
  clean.text = VectorSource(wine.data[[col]])
  
  clean.text = VCorpus(clean.text)
  
  tm_map(clean.text, removeNumbers)
  tm_map(clean.text,removePunctuation)
  
  clean.tm = TermDocumentMatrix(clean.text)
  
  clean.text.matrix = as.matrix(clean.tm)
  
  fq = rowSums(clean.text.matrix)
  
  #Word fequency for plotting data
  
  word_freq = data.frame(term = names(fq), num = fq)
  
  wordcloud(word_freq$term, word_freq$num,  colors=brewer.pal(8, "Dark2"),
            random.color=TRUE,random.order=FALSE)
}

#find word frequencies in Variety columns
FUNC.word.freq('variety')

#Histogram for word frequency in Variety
plot_ly(variety.count, y=variety.count$variety[0:60], x=variety.count$count[0:60], type = "bar", marker = list(color = "maroon"))%>%
  layout(yaxis = list(categoryorder = "array",
                      categoryarray = Top60 )
  )

#############Explore the Provinces########################

#Taking the top 60 wine varieties above, look at the province where they're grown
top60wine.usage <- wine.data %>% 
  select(province,variety)%>%
  group_by(province,variety) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
#Check the work
dim(top60wine.usage)
View(top60wine.usage[1:60,])
datatable(top60wine.usage[1:60,1:3], colnames = c("Province", "Variety", "Count"),  
          options = list(dom = "t", ordering=F), caption = "Variety by Province" )

ggplot(top60wine.usage[1:30,], aes(x = province,y = variety)) +
  geom_tile(aes(fill = count)) +
  labs(x ="province",y="variety",title="Top 30 Wine variety usage")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust = 1))

ggplot(top60wine.usage[31:60,], aes(x = province,y = variety)) +
  geom_tile(aes(fill = count)) +
  labs(x ="province",y="variety",title="Next top 30 Wine variety usage")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust = 1))

###############Explore the points###########################
#
#First just a overview of how the points are distributed
ggplot(data = wine.data, aes(x= points, alpha = 0.6, fill = I('#099DD9')))+
  geom_histogram(binwidth = 1)+
  labs(x = "Points", y= "Frequency", title = "Distribution of points")
#
#Then how are they distributed across the top 60 wines?
variety.vs.points <- wine.data %>%
  select(variety,points) %>%
  group_by(variety) %>%
  summarise(mean_point = mean(points))

View(variety.vs.points)
datatable(variety.vs.points[1:60,1:2], colnames = c("Variety", "Average Points"),  
          options = list(dom = "t", ordering=F, pageLength = 60), caption = "Average Points per Variety" )

#Visualize points against variety
ggplot(data = variety.vs.points, aes(x= variety,y = mean_point))+
  geom_bar(fill="#99CCFF",stat = "identity")+
  labs(x ="Type",y="Frequency",title="Average Points by Variety")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,hjust = 1))

##Now Determine if there is a tendency for some tasters to score lower than others
points.vs.taster <- wine.data %>%
  select(taster_name,points) %>%
  group_by(taster_name) %>%
  summarise(mean_point = mean(points))

View(points.vs.taster)
datatable(points.vs.taster[1:20,1:2], colnames = c("Taster Name", "Average Points"),  
          options = list(dom = "t", ordering=F, pageLength = 20), caption = "Average Points by Taster" )

##Visualize Taster by Points
ggplot(data = points.vs.taster, aes(x= taster_name,y = mean_point))+
  geom_bar(fill="deepskyblue4",stat = "identity")+
  labs(x ="Type",y="Frequency",title="Average Points by Taster")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90,hjust = 1))
################################Begin Association Rule Mining#################################
#
#Read in a new file with vintage year as its own column
ARfilename = 'Wine_plus_vintage.csv'
ARWine = read.csv(ARfilename, header = TRUE, na.strings = "NA")

#Clean that up
#Remove special characters
for (i in names(ARWine)) {
  ARWine[,i]<- str_replace_all(ARWine[,i], "[^a-zA-Z0-9 ]", "")
}

#Remove everything I don't care about for Association Rule Mining
ARWine <- ARWine[ , -which(names(ARWine) %in% c("taster_twitter_handle", "designation", "region_2", "ID", "title", "taster_name", "X"))]

#Convert columns to appropriate data types
ARWine$points <- as.numeric(ARWine$points)
ARWine$price <- as.numeric(ARWine$price)
ARWine$country <- as.factor(ARWine$country)
ARWine$description <- as.factor(ARWine$description)
ARWine$province <- as.factor(ARWine$province)
ARWine$region_1 <- as.factor(ARWine$region_1)
ARWine$vintage_year <- as.numeric(ARWine$vintage_year)
ARWine$variety <- as.factor(ARWine$variety)
ARWine$winery <- as.factor(ARWine$winery)

View(ARWine)
str(ARWine)
summary(ARWine)

#Now limit the set to just the top 60 varieties
#what are the top 60 varieties and how many of each?
ARvariety.count<- ARWine %>%
  group_by(variety) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
View(ARvariety.count[1:60,1:2])

ARWine <- ARWine[ARWine$variety %in% ARvariety.count[1:60,1:2][['variety']],]

##discretize the ratings by Wine Spectator Classifications
#
ARWine$points <- cut(ARWine$points, breaks = c(80, 85, 90, 95, Inf),labels=c("Good", "Very Good", "Outstanding", "Classic"),include.lowest = TRUE, right = FALSE)
ARWine$vintage_year <- cut(ARWine$vintage_year, breaks = c(1996, 1999, 2004, 2009, 2014, Inf), labels=c('older', 'old', 'new', 'newer', 'newest'))

View(ARWine)
str(ARWine)

#subset out only description, points, variety and year
ARWinesub <- ARWine[, which(names(ARWine) %in% c("description", "points", "variety", "vintage_year"))]
View(ARWinesub)

################Subset the wines into red and white###############################
ARWhite <- ARWinesub[which(ARWinesub$variety == 'White Blend' | ARWinesub$variety == 'Riesling'| 
                             ARWinesub$variety == 'Gewrztraminer' | ARWinesub$variety == 'Pinot Gris'|
                             ARWinesub$variety == 'Chardonnay'| ARWinesub$variety == 'Chenin Blanc' |
                             ARWinesub$variety == 'Sauvignon Blanc' | ARWinesub$variety == 'Albario' |
                             ARWinesub$variety == 'Grner Veltliner' | ARWinesub$variety == 'Portuguese White' |
                             ARWinesub$variety == 'Bordeauxstyle White Blend' | ARWinesub$variety == 'Pinot Grigio' |
                             ARWinesub$variety == 'Viognier' | ARWinesub$variety == 'Pinot Blanc' |
                             ARWinesub$variety == 'Moscato' | ARWinesub$variety == 'Melon'),]
View(ARWhite)
str(ARWhite)

ARRed <- ARWinesub[-which(ARWinesub$variety == 'White Blend' | ARWinesub$variety == 'Riesling'| 
                            ARWinesub$variety == 'Gewrztraminer' | ARWinesub$variety == 'Pinot Gris'|
                            ARWinesub$variety == 'Chardonnay'| ARWinesub$variety == 'Chenin Blanc' |
                            ARWinesub$variety == 'Sauvignon Blanc' | ARWinesub$variety == 'Albario' |
                            ARWinesub$variety == 'Grner Veltliner' | ARWinesub$variety == 'Portuguese White' |
                            ARWinesub$variety == 'Bordeauxstyle White Blend' | ARWinesub$variety == 'Pinot Grigio' |
                            ARWinesub$variety == 'Viognier' | ARWinesub$variety == 'Pinot Blanc' |
                            ARWinesub$variety == 'Moscato' | ARWinesub$variety == 'Melon'),]
View(ARRed)
str(ARRed)

#Create some rules from the subsets###########

#Rules for the white wines
whiterules <- apriori(ARWhite, parameter = list(supp = 0.02, conf = 0.6, maxlen = 5))

##Sort the rules by lift
whiterules <- sort(whiterules, by = 'lift', decreasing = TRUE)

#Take a look
inspect(whiterules)

#Rules for the red wines
redrules <- apriori(ARRed, parameter = list(supp = 0.02, conf = 0.6, maxlen = 5))
##Sort the rules by lift
redrules <- sort(redrules, by = 'lift', decreasing = TRUE)
inspect(redrules)

#visualize
#Scatterplots
plot(whiterules, measure = c("support", "lift"), shading = "confidence", jitter =0, interactive = FALSE)
plot(redrules, measure = c("support", "lift"), shading = "confidence", jitter =0, interactive = FALSE)

#graphs
plot(whiterules, method = 'graph')
plot(redrules, method = 'graph')


###############################Clustering###################################################
#
#First add a "Type" column to the dataframe
wine.data[wine.data$variety == "Red Blend", "type"] <- "red"
wine.data[wine.data$variety == "White Blend", "type"] <- "white"
wine.data[wine.data$variety == "Pinot Noir", "type"] <- "red"
wine.data[wine.data$variety == "Chardonnay", "type"] <- "white"
wine.data[wine.data$variety == "Cabernet Sauvignon", "type"] <- "red"
wine.data[wine.data$variety == "Bordeauxstyle Red Blend", "type"] <- "red"
wine.data[wine.data$variety == "Riesling", "type"] <- "white"
wine.data[wine.data$variety == "Sauvignon Blanc", "type"] <- "white"
wine.data[wine.data$variety == "Syrah", "type"] <- "red"
wine.data[wine.data$variety == "Ros", "type"] <- "rose"
wine.data[wine.data$variety == "Merlot", "type"] <- "red"
wine.data[wine.data$variety == "Nebbiolo", "type"] <- "red"
wine.data[wine.data$variety == "Zinfandel", "type"] <- "red"
wine.data[wine.data$variety == "Sangiovese", "type"] <- "red"
wine.data[wine.data$variety == "Malbec", "type"] <- "red"
wine.data[wine.data$variety == "Portuguese Red", "type"] <- "red"
wine.data[wine.data$variety == "Sparkling Blend", "type"] <- "sparkling"
wine.data[wine.data$variety == "Tempranillo", "type"] <- "red"
wine.data[wine.data$variety == "Rhnestyle Red Blend", "type"] <- "red"
wine.data[wine.data$variety == "Rhnestyle White Blend", "type"] <- "white"
wine.data[wine.data$variety == "Pinot Gris", "type"] <- "white"
wine.data[wine.data$variety == "Champagne Blend", "type"] <- "sparkling"
wine.data[wine.data$variety == "Cabernet Franc", "type"] <- "red"
wine.data[wine.data$variety == "Grner Veltliner", "type"] <- "white"
wine.data[wine.data$variety == "Portuguese White", "type"] <- "white"
wine.data[wine.data$variety == "Bordeauxstyle White Blend", "type"] <- "white"
wine.data[wine.data$variety == "Pinot Grigio", "type"] <- "white"
wine.data[wine.data$variety == "Gamay", "type"] <- "red"
wine.data[wine.data$variety == "Gewrztraminer", "type"] <- "red"
wine.data[wine.data$variety == "Viognier", "type"] <- "white"
wine.data[wine.data$variety == "Shiraz", "type"] <- "red"
wine.data[wine.data$variety == "Petite Sirah", "type"] <- "red"
wine.data[wine.data$variety == "Sangiovese Grosso", "type"] <- "red"
wine.data[wine.data$variety == "Barbera", "type"] <- "red"
wine.data[wine.data$variety == "Glera", "type"] <- "red"
wine.data[wine.data$variety == "Port", "type"] <- "red"
wine.data[wine.data$variety == "Grenache", "type"] <- "red"
wine.data[wine.data$variety == "Corvina Rondinella Molinara", "type"] <- "red"
wine.data[wine.data$variety == "Chenin Blanc", "type"] <- "white"
wine.data[wine.data$variety == "Tempranillo Blend", "type"] <- "red"
wine.data[wine.data$variety == "Grenache", "type"] <- "red"
wine.data[wine.data$variety == "Carmenre", "type"] <- "red"
wine.data[wine.data$variety == "Albario", "type"] <- "white"
wine.data[wine.data$variety == "Pinot Blanc", "type"] <- "white"
wine.data[wine.data$variety == "Nero dAvola", "type"] <- "red"
wine.data[wine.data$variety == "Aglianico", "type"] <- "red"
wine.data[wine.data$variety == "Moscato", "type"] <- "white"
wine.data[wine.data$variety == "Garnacha", "type"] <- "red"
wine.data[wine.data$variety == "Sauvignon", "type"] <- "red"
wine.data[wine.data$variety == "Verdejo", "type"] <- "red"
wine.data[wine.data$variety == "Melon", "type"] <- "white"
wine.data[wine.data$variety == "Garganega", "type"] <- "red"
wine.data[wine.data$variety == "Petit Verdot", "type"] <- "red"
wine.data[wine.data$variety == "Meritage", "type"] <- "red"
wine.data[wine.data$variety == "Torronts", "type"] <- "red"
wine.data[wine.data$variety == "Prosecco", "type"] <- "sparkling"
wine.data[wine.data$variety == "Blaufrnkisch", "type"] <- "red"
wine.data[wine.data$variety == "Vermentino", "type"] <- "red"
wine.data[wine.data$variety == "Mourvdre", "type"] <- "red"
wine.data[wine.data$variety == "Primitivo", "type"] <- "red"
wine.data[wine.data$variety == "Montepulciano", "type"] <- "red"

#Check to make sure I got them all
didigetall <- sum(is.na(wine.data$type) == TRUE)
didigetall
which(is.na(wine.data$type))

#Visualize
ggplot(wine.data, aes(x = type)) +
  geom_bar(fill = "darkorchid") +
  coord_flip()

##########
#
#Only using the data with type red or white
new.data <- wine.data[wine.data$type %in% c("red", "white"), ]
redIndex <- which(new.data$type == "red")
whiteIndex <- which(new.data$type == "white")

#Create a token matrix of the description column
#
#first convert to character vector
wine.data$description <- as.character(wine.data$description)
description.token <- tokens(wine.data$description, what = "word",
                              remove_numbers = TRUE, remove_punct = TRUE,
                              remove_symbols = TRUE, remove_hyphens = TRUE)
#Lower case, no stopwords
description.token <- tokens_tolower(description.token)
description.token <- tokens_select(description.token, stopwords(),
                                     selection = "remove")
#See what's left
(description.token)

#I'm OK with that - create a matrix and take some measurements
description.dfm <- dfm(description.token)
description.dfm <- description.dfm[, colSums(description.dfm) > 99]
description.matrix <- t(as.matrix(description.dfm))
description.matrix <- apply(description.matrix, 1, function(x) x / sum(x))


#For the red and white wines, take the cross products of the matices to get a matrix with an
#indication of the correlation between words
cProdRed <- crossprod(description.matrix[redIndex, ], description.matrix[redIndex, ])
cProdRed <- cProdRed[!is.na(cProdRed[, 1]), !is.na(cProdRed[1, ])]
cProdWhite <- crossprod(description.matrix[whiteIndex, ], description.matrix[whiteIndex, ])
cProdWhite <- cProdWhite[!is.na(cProdWhite[, 1]), !is.na(cProdWhite[1, ])]

#Dividing each row by sum of row. Then some operations to get a more suitable measurement. For both red and white.
cProdRed <- t(apply(cProdRed, 1, function(x) x / sum(x)))
cProdRed <- log(cProdRed)
cProdRed[cProdRed == -Inf | is.na(cProdRed)] <- 0
rAdd <- abs(min(cProdRed))
cProdRed[cProdRed != 0] <- cProdRed[cProdRed != 0] + rAdd

cProdWhite <- t(apply(cProdWhite, 1, function(x) x / sum(x)))
cProdWhite <- log(cProdWhite)
cProdWhite[cProdWhite == -Inf | is.na(cProdWhite)] <- 0
wAdd <- abs(min(cProdWhite))
cProdWhite[cProdWhite != 0] <- cProdWhite[cProdWhite != 0] + wAdd

#Get  clusters of these correlation vectors using k-means clustering

set.seed(100)
redClusters <- kmeans(cProdRed, 50)
whiteClusters <- kmeans(cProdWhite, 50)

#Getting the clusters with more than 5 members
rClusterIndexes <- which(redClusters$size > 5)

#By dividing the withing cluster variation with log size, get a measurement of how close the cluser in question is.
rClusterImportance <- (redClusters$withinss / log(redClusters$size))[rClusterIndexes]
rTenthGreatest <- sort(rClusterImportance, partial = length(rClusterImportance) - 9)[length(rClusterImportance) - 9]

#Taking the 10 best clusters
rChosen <- rClusterIndexes[which(rClusterImportance >= rTenthGreatest)]

#Repeating the above selection procedure for white wines
wClusterIndexes <- which(whiteClusters$size > 5)
wClusterImportance <- (whiteClusters$withinss / log(whiteClusters$size))[wClusterIndexes]
wTenthGreatest <- sort(wClusterImportance, partial = length(wClusterImportance) - 9)[length(wClusterImportance) - 9]
wChosen <- wClusterIndexes[which(wClusterImportance >= wTenthGreatest)]

#Selecting the centers of the chosen clusters
rCenters <- redClusters$centers[rChosen, ]
#Selecting the 20 words in which direction the vector is pointing the strongest. Indicating what the cluster is about.
rWords <- apply(rCenters, 1, function(x) names(sort(x, decreasing = TRUE))[1:20])

#Doing the same for white wines                
wCenters <- whiteClusters$centers[wChosen, ]
wWords <- apply(wCenters, 1, function(x) names(sort(x, decreasing = TRUE))[1:20])

#Let's see how it looks
rWords

#Some headers for the red word clusters
rHeaders <- c("Intense", "Light  & Fruity", "Complex", "Rich", 
              "Terroir", "Fruit Forward", "Fresh", "Aromatic",
              "Oaky", "Refined")

datatable(rWords[1:20,1:10], colnames = rHeaders,  
          options = list(dom = "t", ordering=F, pageLength = 20), caption = "Red Wine Descriptions" )

#Repeat for white
wWords

wHeaders <- c("Ripe Fruit", "Lush", "Floral", "Structured", 
              "Rich", "Aromatic", "Complex", "Oaky", "Fruit Forward",
              "Refined")

datatable(wWords[1:20,1:10], colnames = wHeaders,  
          options = list(dom = "t", ordering=F, pageLength = 20), caption = "White Wine Descriptions" )

#visualize
p1 <- fviz_cluster(redClusters, geom = "point", data = cProdRed) + ggtitle("Red Wine Clusters")
p2 <- fviz_cluster(whiteClusters, geom = "point", data = cProdWhite) + ggtitle("White Wine Clusters")

grid.arrange(p1, p2, nrow = 2) # arrange all plots in same grid

dp1 <- fviz_cluster(redClusters, data = cProdRed, geom = "point", stand = FALSE, ellipse.type = "norm") + theme_bw() + ggtitle("Red Wine Clusters")
dp2 <- fviz_cluster(whiteClusters, data = cProdWhite, geom = "point", stand = FALSE, ellipse.type = "norm") + theme_bw() + ggtitle("White Wine Clusters")

grid.arrange(dp1, dp2, nrow = 2) # arrange all plots in same grid


###############################Decision Trees#####################################################

#First create a corpus from the description column
#
#From Kaggle##Predict wine and vineyar
#
#Read in data with new variable name
file2 <- 'CleanedWineTasting.csv'
Winevineyard = read.csv(file2, header = TRUE, sep = ';', na.strings = "NA")

# Create corpus from description
corpus = VCorpus(VectorSource(Winevineyard$description))

#Clean up corpus by making everything lower case, removing punctuation, and stopwords, and lematizing
#
#Lowercase
corpus = tm_map(corpus, content_transformer(tolower))
#
#Punctuation
corpus = tm_map(corpus, removePunctuation)
#
##removing stop words and the word wine as it won't be much helpful
corpus = tm_map(corpus, removeWords, c("wine", stopwords("english")))
#
#Lematizing
corpus = tm_map(corpus, stemDocument)

#Now turn that into a DTM
frequencies = DocumentTermMatrix(corpus)
#
#Create another DTM without "little" words. 
#The closer the number gets to 1.0, the "smaller" the word
#setting the value to 0.97 should only remove very small words
sparse = removeSparseTerms(frequencies, 0.97)

#Now turn that into a dataframe
wineSparse = as.data.frame(as.matrix(sparse))
#
#Check it out
View(wineSparse)

wineLabels <- Winevineyard[ , which(names(Winevineyard) %in% c('variety'))]

WineDTM <- cbind(wineLabels, wineSparse)
View(WineDTM)

#Create Training and Testing sets

#Cut sample by 1/7
every7_indexes<-seq(1,nrow(WineDTM),7)

#Create Test and Train Dataframes for Decision Tree analysis
TrainWineDTM<-WineDTM[every7_indexes, ]
TestWineDTM<-WineDTM[-every7_indexes, ]

#Check them out - Training First
View(TrainWineDTM)
str(TrainWineDTM)
summary(TrainWineDTM)

#Check them out - Now Test
View(TestWineDTM)
str(TestWineDTM)
summary(TestWineDTM)

#Remove the labels from Test and put them in its own data frame

Testlabels <- as.factor(TestWineDTM[[1]])
TestWineDTM <- TestWineDTM[, -1]

#Now let's run the decision tree
library(rpart)
library(rattle)
WineTree <- rpart(TrainWineDTM$wineLabels ~ ., data = TrainWineDTM, method="class")
#summarize the tree
summary(WineTree)
#visualize the tree
fancyRpartPlot(WineTree, main="Decision Tree Graph")

#Make predictions
(DTWine_Pred<-predict(WineTree, TestWineDTM, type="class"))
(Testlabels)


### CONFUSION MATRIX
(CMWine <- table(Testlabels,DTWine_Pred))
(accuracy <- sum(diag(CMWine))/sum(CMWine))


#That didn't work so great - let's try splitting reds and whites

#
#First rename winevineyard to make a little easier
wine.data2 <- Winevineyard

#Now repeat what we did above to add a "Type" column to the dataframe
wine.data2[wine.data2$variety == "Red Blend", "type"] <- "red"
wine.data2[wine.data2$variety == "White Blend", "type"] <- "white"
wine.data2[wine.data2$variety == "Pinot Noir", "type"] <- "red"
wine.data2[wine.data2$variety == "Chardonnay", "type"] <- "white"
wine.data2[wine.data2$variety == "Cabernet Sauvignon", "type"] <- "red"
wine.data2[wine.data2$variety == "Bordeauxstyle Red Blend", "type"] <- "red"
wine.data2[wine.data2$variety == "Riesling", "type"] <- "white"
wine.data2[wine.data2$variety == "Sauvignon Blanc", "type"] <- "white"
wine.data2[wine.data2$variety == "Syrah", "type"] <- "red"
wine.data2[wine.data2$variety == "Ros", "type"] <- "rose"
wine.data2[wine.data2$variety == "Merlot", "type"] <- "red"
wine.data2[wine.data2$variety == "Nebbiolo", "type"] <- "red"
wine.data2[wine.data2$variety == "Zinfandel", "type"] <- "red"
wine.data2[wine.data2$variety == "Sangiovese", "type"] <- "red"
wine.data2[wine.data2$variety == "Malbec", "type"] <- "red"
wine.data2[wine.data2$variety == "Portuguese Red", "type"] <- "red"
wine.data2[wine.data2$variety == "Sparkling Blend", "type"] <- "sparkling"
wine.data2[wine.data2$variety == "Tempranillo", "type"] <- "red"
wine.data2[wine.data2$variety == "Rhnestyle Red Blend", "type"] <- "red"
wine.data2[wine.data2$variety == "Rhnestyle White Blend", "type"] <- "white"
wine.data2[wine.data2$variety == "Pinot Gris", "type"] <- "white"
wine.data2[wine.data2$variety == "Champagne Blend", "type"] <- "sparkling"
wine.data2[wine.data2$variety == "Cabernet Franc", "type"] <- "red"
wine.data2[wine.data2$variety == "Grner Veltliner", "type"] <- "white"
wine.data2[wine.data2$variety == "Portuguese White", "type"] <- "white"
wine.data2[wine.data2$variety == "Bordeauxstyle White Blend", "type"] <- "white"
wine.data2[wine.data2$variety == "Pinot Grigio", "type"] <- "white"
wine.data2[wine.data2$variety == "Gamay", "type"] <- "red"
wine.data2[wine.data2$variety == "Gewrztraminer", "type"] <- "red"
wine.data2[wine.data2$variety == "Viognier", "type"] <- "white"
wine.data2[wine.data2$variety == "Shiraz", "type"] <- "red"
wine.data2[wine.data2$variety == "Petite Sirah", "type"] <- "red"
wine.data2[wine.data2$variety == "Sangiovese Grosso", "type"] <- "red"
wine.data2[wine.data2$variety == "Barbera", "type"] <- "red"
wine.data2[wine.data2$variety == "Glera", "type"] <- "red"
wine.data2[wine.data2$variety == "Port", "type"] <- "red"
wine.data2[wine.data2$variety == "Grenache", "type"] <- "red"
wine.data2[wine.data2$variety == "Corvina Rondinella Molinara", "type"] <- "red"
wine.data2[wine.data2$variety == "Chenin Blanc", "type"] <- "white"
wine.data2[wine.data2$variety == "Tempranillo Blend", "type"] <- "red"
wine.data2[wine.data2$variety == "Grenache", "type"] <- "red"
wine.data2[wine.data2$variety == "Carmenre", "type"] <- "red"
wine.data2[wine.data2$variety == "Albario", "type"] <- "white"
wine.data2[wine.data2$variety == "Pinot Blanc", "type"] <- "white"
wine.data2[wine.data2$variety == "Nero dAvola", "type"] <- "red"
wine.data2[wine.data2$variety == "Aglianico", "type"] <- "red"
wine.data2[wine.data2$variety == "Moscato", "type"] <- "white"
wine.data2[wine.data2$variety == "Garnacha", "type"] <- "red"
wine.data2[wine.data2$variety == "Sauvignon", "type"] <- "red"
wine.data2[wine.data2$variety == "Verdejo", "type"] <- "red"
wine.data2[wine.data2$variety == "Melon", "type"] <- "white"
wine.data2[wine.data2$variety == "Garganega", "type"] <- "red"
wine.data2[wine.data2$variety == "Petit Verdot", "type"] <- "red"
wine.data2[wine.data2$variety == "Meritage", "type"] <- "red"
wine.data2[wine.data2$variety == "Torronts", "type"] <- "red"
wine.data2[wine.data2$variety == "Prosecco", "type"] <- "sparkling"
wine.data2[wine.data2$variety == "Blaufrnkisch", "type"] <- "red"
wine.data2[wine.data2$variety == "Vermentino", "type"] <- "red"
wine.data2[wine.data2$variety == "Mourvdre", "type"] <- "red"
wine.data2[wine.data2$variety == "Primitivo", "type"] <- "red"
wine.data2[wine.data2$variety == "Montepulciano", "type"] <- "red"

#Check to make sure I got them all
didigetall <- sum(is.na(wine.data2$type) == TRUE)
didigetall
which(is.na(wine.data2$type))

#
#Only using the data with type red or white
whitewine <- wine.data2[wine.data2$type %in% c("white"), ]
redwine <- wine.data2[wine.data2$type %in% c("red"), ]

#Now do a corpus for each
#
#white first

# Create corpus from description
whitecorpus = VCorpus(VectorSource(whitewine$description))

#Clean up corpus by making everything lower case, removing punctuation, and stopwords, and lematizing
#
#Lowercase
whitecorpus = tm_map(whitecorpus, content_transformer(tolower))
#
#Punctuation
whitecorpus = tm_map(whitecorpus, removePunctuation)
#
##removing stop words and the word wine as it won't be much helpful
whitecorpus = tm_map(whitecorpus, removeWords, c("wine", stopwords("english")))
#
#Lematizing
whitecorpus = tm_map(whitecorpus, stemDocument)

#Now turn that into a DTM
whitefrequencies = DocumentTermMatrix(whitecorpus)
#
#Create another DTM without "little" words. 
#The closer the number gets to 1.0, the "smaller" the word
#setting the value to 0.97 should only remove very small words
whitesparse = removeSparseTerms(whitefrequencies, 0.97)

#Now turn that into a dataframe
whitewineSparse = as.data.frame(as.matrix(whitesparse))
#
#Check it out
View(whitewineSparse)

whitewineLabels <- as.character(whitewine[ , 11])
whitewinelabels <- as.factor(whitewineLabels)

whiteWineDTM <- cbind(whitewineLabels, whitewineSparse)
View(whiteWineDTM)

#Now for the reds

# Create corpus from description
redcorpus = VCorpus(VectorSource(redwine$description))

#Clean up corpus by making everything lower case, removing punctuation, and stopwords, and lematizing
#
#Lowercase
redcorpus = tm_map(redcorpus, content_transformer(tolower))
#
#Punctuation
redcorpus = tm_map(redcorpus, removePunctuation)
#
##removing stop words and the word wine as it won't be much helpful
redcorpus = tm_map(redcorpus, removeWords, c("wine", stopwords("english")))
#
#Lematizing
redcorpus = tm_map(redcorpus, stemDocument)

#Now turn that into a DTM
redfrequencies = DocumentTermMatrix(redcorpus)
#
#Create another DTM without "little" words. 
#The closer the number gets to 1.0, the "smaller" the word
#setting the value to 0.97 should only remove very small words
redsparse = removeSparseTerms(redfrequencies, 0.97)

#Now turn that into a dataframe
redwineSparse = as.data.frame(as.matrix(redsparse))
#
#Check it out
View(redwineSparse)

redwineLabels <- as.character(redwine[ , which(names(redwine) %in% c('variety'))])
redwineLabels <- as.factor(redwineLabels)

redWineDTM <- cbind(redwineLabels, redwineSparse)
View(redWineDTM)


#Create Training and Testing sets
#
#White First

#Create Test and Train Dataframes for Decision Tree analysis
whiteTrainWineDTM<-whiteWineDTM[every7_indexes, ]
whiteTestWineDTM<-whiteWineDTM[-every7_indexes, ]

#Remove the labels from Test and put them in its own data frame

whiteTestlabels <- as.factor(whiteTestWineDTM[[1]])
whiteTestWineDTM <- whiteTestWineDTM[, -1]

#Now Red
#Create Test and Train Dataframes for Decision Tree analysis
redTrainWineDTM<-redWineDTM[every7_indexes, ]
redTestWineDTM<-redWineDTM[-every7_indexes, ]

#Remove the labels from Test and put them in its own data frame

redTestlabels <- as.factor(redTestWineDTM[[1]])
redTestWineDTM <- redTestWineDTM[, -1]

#Now let's run the decision tree
#
#White first
whiteWineTree <- rpart(whiteTrainWineDTM$whitewineLabels ~ ., data = whiteTrainWineDTM, method="class")
#summarize the tree
summary(whiteWineTree)
#visualize the tree
fancyRpartPlot(whiteWineTree, main="White Decision Tree Graph")

#Make predictions
(DTWine_Pred<-predict(whiteWineTree, whiteTestWineDTM, type="class"))
(whiteTestlabels)

#Now Reds
redWineTree <- rpart(redTrainWineDTM$redwineLabels ~ ., data = redTrainWineDTM, method="class")
#summarize the tree
summary(redWineTree)
#visualize the tree
fancyRpartPlot(redWineTree, main="Red Decision Tree Graph")

#Make predictions
(redDTWine_Pred<-predict(redWineTree, redTestWineDTM, type="class"))
(redTestlabels)

### CONFUSION MATRIX
(CMredWine <- table(redTestlabels,redDTWine_Pred))
(accuracy <- sum(diag(CMredWine))/sum(CMredWine))

#Better on the whites, still low on the reds. What if we only take the top 3 wines?
subwhite <- whitewine[whitewine$variety %in% c("Chardonnay", "Riesling", "Sauvignon Blanc"), ]
subred <- redwine[redwine$variety %in% c("Pinot Noir", "Red Blend", "Sauvignon"), ]

#Corpus
# Create corpus from description
subredcorpus = VCorpus(VectorSource(subred$description))
subwhitecorpus = VCorpus(VectorSource(subwhite$description))

#Now turn that into a DTM
subredfrequencies = DocumentTermMatrix(subredcorpus)
subwhitefrequencies = DocumentTermMatrix(subwhitecorpus)
#
#Create another DTM without "little" words. 
#The closer the number gets to 1.0, the "smaller" the word
#setting the value to 0.97 should only remove very small words
subredsparse = removeSparseTerms(subredfrequencies, 0.97)
subwhitesparse = removeSparseTerms(subwhitefrequencies, 0.97)

#Now turn that into a dataframe
subredwineSparse = as.data.frame(as.matrix(subredsparse))
subwhitewineSparse = as.data.frame(as.matrix(subwhitesparse))

subredLabels <- as.character(subred[ , which(names(subred) %in% c('variety'))])
subredLabels <- as.factor(subredLabels)

subwhiteLabels <- as.character(subwhite[ , which(names(subwhite) %in% c('variety'))])
subwhiteLabels <- as.factor(subwhiteLabels)

subredDTM <- cbind(subredLabels, subredwineSparse)
subwhiteDTM <- cbind(subwhiteLabels, subwhitewineSparse)

#Create Training and Testing sets
#
#White First

#Create Test and Train Dataframes for Decision Tree analysis
subwhiteTrainWineDTM<-subwhiteDTM[every7_indexes, ]
subwhiteTestWineDTM<-subwhiteDTM[-every7_indexes, ]

subredTrainWineDTM<-subredDTM[every7_indexes, ]
subredTestWineDTM<-subredDTM[-every7_indexes, ]

#Remove the labels from Test and put them in its own data frame

subwhiteTestlabels <- as.factor(subwhiteTestWineDTM[[1]])
subwhiteTestWineDTM <- subwhiteTestWineDTM[, -1]


subredtestlabels <- as.factor(subredTestWineDTM[[1]])
subredTestWineDTM <- subredTestWineDTM[, -1]

#Now let's run the decision tree - White
#
subwhiteWineTree <- rpart(subwhiteTrainWineDTM$subwhiteLabels ~ ., data = subwhiteTrainWineDTM, method="class")
#summarize the tree
summary(subwhiteWineTree)
#visualize the tree
fancyRpartPlot(subwhiteWineTree, main="Subset White Decision Tree Graph")

#Make predictions
(subwhiteDTWine_Pred<-predict(subwhiteWineTree, subwhiteTestWineDTM, type="class"))
(subwhiteTestlabels)

### CONFUSION MATRIX
(subCMwhiteWine <- table(subwhiteTestlabels,subwhiteDTWine_Pred))
(accuracy <- sum(diag(subCMwhiteWine))/sum(subCMwhiteWine))

#Red Tree

#
subredWineTree <- rpart(subredTrainWineDTM$subredLabels ~ ., data = subredTrainWineDTM, method="class")
#summarize the tree
summary(subredWineTree)
#visualize the tree
fancyRpartPlot(subredWineTree, main="Subset Red Decision Tree Graph")

#Make predictions
(subredDTWine_Pred<-predict(subredWineTree, subredTestWineDTM, type="class"))
(subredtestlabels)

### CONFUSION MATRIX
(subCMredWine <- table(subredtestlabels,subredDTWine_Pred))
(accuracy <- sum(diag(subCMredWine))/sum(subCMredWine))

###############Enough of the Decision Trees. Let's Try some other models###################
#
#######################Naive Bayes##############################
#
#run the algorithm
WhiteNB <- naivebayes::naive_bayes(whiteTrainWineDTM$whitewineLabels~., data=whiteTrainWineDTM)
#take a look
(summary(WhiteNB))

#
#Make predictions
WhiteNBPred <- predict(WhiteNB, whiteTestWineDTM)

### CONFUSION MATRIX
(WhiteNBconfMat <- table(whiteTestlabels,WhiteNBPred))
(accuracy <- sum(diag(WhiteNBconfMat))/sum(WhiteNBconfMat))

#YIKES! 1.3% Let's try with a subset
#
#Whites First
SubWhiteNB <- naivebayes::naive_bayes(subwhiteTrainWineDTM$subwhiteLabels~., data=subwhiteTrainWineDTM)
#take a look
(summary(SubWhiteNB))

#
#Make predictions
SubWhiteNBPred <- predict(SubWhiteNB, subwhiteTestWineDTM)

### CONFUSION MATRIX
(SubWhiteNBconfMat <- table(subwhiteTestlabels,SubWhiteNBPred))
(accuracy <- sum(diag(SubWhiteNBconfMat))/sum(SubWhiteNBconfMat))

#Naive Bayes Red
SubredNB <- naivebayes::naive_bayes(subredTrainWineDTM$subredLabels~., data=subredTrainWineDTM)
#take a look
(summary(SubredNB))

#
#Make predictions
SubredNBPred <- predict(SubredNB, subredTestWineDTM)

### CONFUSION MATRIX
(SubredNBconfMat <- table(subredtestlabels,SubredNBPred))
(accuracy <- sum(diag(SubredNBconfMat))/sum(SubredNBconfMat))


#############SVM##################################
library(MASS)
library(e1071)
library(class)
mySVM <- svm(subwhiteTrainWineDTM$subwhiteLabels ~., data=subwhiteTrainWineDTM, 
             kernel="linear", cost=100, 
             scale=FALSE)
## Kernels can be radial, linear, and polynomial. Costs can range from
## between .001 to 1000. I tested many costs with many kernels and this
## was the best I could get to.

predSVM <- predict(mySVM, subwhiteTestWineDTM, type="class")


### CONFUSION MATRIX
(SVMconfMat <- table(subwhiteTestlabels, predSVM))
(accuracy <- sum(diag(SVMconfMat))/sum(SVMconfMat))

#Reds
redSVM <- svm(subredTrainWineDTM$subredLabels ~., data=subredTrainWineDTM, 
             kernel="linear", cost=100, 
             scale=FALSE)
## Kernels can be radial, linear, and polynomial. Costs can range from
## between .001 to 1000. I tested many costs with many kernels and this
## was the best I could get to.

predredSVM <- predict(redSVM, subredTestWineDTM, type="class")


### CONFUSION MATRIX
(SVMredconfMat <- table(subredtestlabels, predredSVM))
(accuracy <- sum(diag(SVMconfMat))/sum(SVMconfMat))
#############################################################
cwtrain <- na.omit(subwhiteTrainWineDTM)
cwtest <- na.omit(subwhiteTestWineDTM)
cwtestlabels <- na.omit(subwhiteTestlabels)

crtrain <- na.omit(subredTrainWineDTM)
crtest <- na.omit(subredTestWineDTM)
crtestlabels <- na.omit(subredtestlabels)

#####################Random Forest Decision Tree##############
#
#Build the tree
library(randomForest)
numTrees <- 50

#for random forest, the datasets have to match - so, remove the labels from training 
cwtrain2 <- cwtrain[,-1]
cwtrainlabels = cwtrain[,1]
crtrain2 <- crtrain[,-1]
crtrainlabels = crtrain[,1]

#now build the tree
rf <- randomForest(cwtrain2, cwtrainlabels, ntree = numTrees, keep.inbag = TRUE, keep.forest = TRUE, xtest = cwtest, proximity = TRUE)
rrf <- randomForest(crtrain2, crtrainlabels, ntree = numTrees, keep.inbag = TRUE, keep.forest = TRUE, xtest = crtest, proximity = TRUE)

#Make predictions
predictions <- levels(cwtrainlabels)[rf$test$predicted]
predictionIsCorrect = cwtestlabels == predictions

rpredictions <- levels(crtrainlabels)[rrf$test$predicted]
rpredictionIsCorrect = crtestlabels == rpredictions

### CONFUSION MATRIX
(RFconfMat <- table(cwtestlabels, predictions))
(accuracy <- sum(diag(RFconfMat))/sum(RFconfMat))

(rRFconfMat <- table(crtestlabels, rpredictions))
(accuracy <- sum(diag(rRFconfMat))/sum(rRFconfMat))

##################KNN########################################
wKNN <- knn(train = cwtrain2, test = cwtest, cl = cwtrainlabels, k=11)

(wknnConfmat <- table(cwtestlabels, wKNN))
(accuracy <- sum(diag(wknnConfmat))/sum(wknnConfmat))

rKNN <- knn(train = crtrain2, test = crtest, cl = crtrainlabels, k=11)

(rknnConfmat <- table(crtestlabels, rKNN))
(accuracy <- sum(diag(rknnConfmat))/sum(rknnConfmat))

#######################Naive Bayes Again with NAs removed##############################

wNB <- naivebayes::naive_bayes(cwtrain$subwhiteLabels~., data=cwtrain)
rNB <- naivebayes::naive_bayes(crtrain$subredLabels~., data=crtrain)


#
#Make predictions
wNBPred <- predict(wNB, cwtest)
rNBPred <- predict(rNB, crtest)

### CONFUSION MATRIX
(wNBconfMat <- table(cwtestlabels,wNBPred))
(accuracy <- sum(diag(wNBconfMat))/sum(wNBconfMat))

(rNBconfMat <- table(crtestlabels,rNBPred))
(accuracy <- sum(diag(rNBconfMat))/sum(rNBconfMat))

#No Difference - NB does NOT like the red database!

################################################################################################

###############################Time to do something new########################################
####Let's try to predict Country based off points and price###################################
#
#I think it still makes sense to keep it separated between white and red
#
#White wine dataframe = whitewine
#Red wine dataframe = redwine
#
#################################################################################################
#
#First make new dataframes
view(whitewine)
view(redwine)


#Cut the columns I don't want to consider
newwhite <- whitewine[ , which(names(whitewine) %in% c("country", "points", "price"))]
newwhite[newwhite == "" | newwhite == " "] <- NA
newwhite <- newwhite[complete.cases(newwhite),]


(missing_data <- colSums(is.na(newwhite) | newwhite == "" | newwhite == " "))


newred <- redwine[ , which(names(redwine) %in% c("country", "points", "price"))]
newred[newred == "" | newred == " "] <- NA
newred <- newred[complete.cases(newred),]

#Now make testing and training stuff

#Create Test and Train Dataframes 
newwhitetrain<-newwhite[every7_indexes, ]
newwhitetest<-newwhite[-every7_indexes, ]

newredtrain<-newred[every7_indexes, ]
newredtest<-newred[-every7_indexes, ]

#Remove the labels from Test and put them in its own data frame

newwhiteTestlabels <- as.factor(newwhitetest[[1]])
newwhitetest <- newwhitetest[, -1]


newredtestlabels <- as.factor(newredtest[[1]])
newredtest <- newredtest[, -1]

str(newwhitetrain)
str(newwhitetest)
str(newredtrain)
str(newredtest)

############Decision Tree####################################

nwtree <- rpart(newwhitetrain$country~ ., data = newwhitetrain, method="class")
nrtree <- rpart(newredtrain$country~., data = newredtrain, method="class")


#visualize the tree
fancyRpartPlot(nwtree, main="Country Decision Tree Graph - White Wine")
fancyRpartPlot(nrtree, main="Country Decision Tree Graph - Red Wine")

#Make predictions
(nwDTWine_Pred<-predict(nwtree, newwhitetest, type="class"))

### CONFUSION MATRIX
(nwCM <- table(newwhiteTestlabels,nwDTWine_Pred))
(accuracy <- sum(diag(nwCM))/sum(nwCM))

