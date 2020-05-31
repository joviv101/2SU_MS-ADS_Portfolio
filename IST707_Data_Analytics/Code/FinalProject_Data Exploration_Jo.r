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
##discretize the ratings by Wine Spectator Classifications
Wine$points <- cut(Wine$points, breaks = c(80, 85, 90, 95, Inf),labels=c("Good", "Very Good", "Outstanding", "Classic"),include.lowest = TRUE, right = FALSE)


