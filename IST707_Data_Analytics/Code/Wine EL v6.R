#IST707 Final Project
#set ggplot default to center titles
theme_update(plot.title = element_text(hjust = 0.5))

install.packages("arulesViz")
install.packages("tidyverse")
install.packages("DT")
install.packages("stringr")
install.packages("kableExtra")
library(kableExtra)
library(arulesViz)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(knitr)
library(ggmap)
library(tidytext)
library(tm)
library(readr)
library(DT)
library(arulesViz)
library(stringr)
install.packages("plotly")
library(plotly)

#Read in winemag_data-130K csv as training data set
Wine<-Wine<- read.csv("winemag-data-130k-v2.csv")

str(Wine)
View(Wine)
colnames(Wine)

##################Data Exploration#################################
#Start to view data
(colnames(Wine))
head(Wine$designation) #we see some odd characters that we may want to remove
head(Wine$country)
head(Wine$description) #let's do a word cloud or bar chart on most common words
head(Wine$points)
head(Wine$price) 
head(Wine$province) #We see odd characters again
head(Wine$region_1)
head(Wine$region_2)
head(Wine$title)
head(Wine$variety) #odd characters again
head(Wine$winery)

#look at how many unique ##Does anyone know how to create a summary table of just the counts of each of these?
uniqueprov<-unique(Wine$province) #426 provinces
unique(Wine$variety) #708 varieties
unique(Wine$country) #44 countries
unique(Wine$region_1) #1230 regions
unique(Wine$winery) #~17,000

##datatable(Wine, colnames = c("province,variety,country,region_1,winery"),
         # options = list(dom = "t", ordering=F), caption = "table name here" )

#Plot the variables
str(Wine$country)
countrytable<-table(Wine$country) #creates table w columns var & freq
sort(countrytable) #this sorts alphabetically
View(countrytable)
country<-data$country[order(-data$country)]
p<-barplot(country)

p <- ggplot(Wine, aes(x = reorder(country, -country), y = price))
p <- p + geom_bar(stat="identity", color='skyblue',fill='steelblue')
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p




plot(Wine$country,main="# of Reviews by Country")

#Create tables all at once to further view variables
#for(i in 1:ncol(Wine)){
 # print(table(Wine[i]))
#}

##Check for Missing Values
(missing_data<-colSums(is.na(Wine) | Wine == "" | Wine == " "))
kable(missing_data) #I'd like to be able to do a bar plot showing variables by missing values (sorted high to low)
View(missing_data)

#How do we want to handle missing values? (I think we have enough data if we ignore all missing)
na.omit(Wine)
str(Wine)
mean(Wine$price, na.rm=TRUE) 

##Descriptive Stats
as.numeric(Wine$price) #convert price to numeric 
as.numeric(Wine$points)#convert points to numerc
mean(Wine$price, na.rm=TRUE) 
range(Wine$price,na.rm=TRUE)
mean(Wine$points,na.rm=TRUE)
range(Wine$points,na.rm=TRUE)


#hist ofcountries
plot_ly(Wine, type = "histogram", x= Wine$country)

#############CLEAN DATA###########################
#Remove special characters &1st column /call it Wine_clean
for (i in names(Wine)) {
  Wine[,i]<- str_replace_all(Wine[,i], "[^a-zA-Z0-9]", " ")
}

#remove 1st col
Wine<-Wine[,-1]

stopwords<-c("wine", "now", "flavors", "notes", "the", "one", "will", "also", "give", "like", "drink", "with", "it s", "there s", "to", "this", "in", "and", "a", "is")
Wine$description<-removeWords(Wine$description, stopwords )
#remove punctuation
Wine$description<-gsub("[[:punct:]]", "", Wine$description)
#remove Numbers
Wine$description<-removeNumbers(Wine$description)
#to lower case
Wine$description<- tolower(Wine$description)

###Limit to US only
WineUS<-Wine[which(Wine$country=='US'),]
View(WineUS)

##Limit to NON US
WineNonUS<-Wine[which(Wine$country!='US'),]
View(WineNonUS)

#Limit to France
Wine_france<-Wine[which(Wine$country=='France'),]
View(Wine_france)


#Word Clouds
wordcloud(WineUS$description,max.words = 50,
          colors=brewer.pal(3, "Set1")) # Top 50 most frequent words in US wine description

wordcloud(WineNonUS$description, max.words = 50,
          colors=brewer.pal(3, "Set1")) # 50 most frequent words in non US wine description

wordcloud(Wine_france$description, max.words = 50,
          colors=brewer.pal(3, "Set1")) # 50 most frequent words in non US wine description

unique(WineUS$province)


#look at missing data in WineUS
(missing_dataUS<-colSums(is.na(WineUS) | WineUS == "" | WineUS == " "))
kable(missing_dataUS) #I'd like to be able to do a bar plot showing variables by missing values (sorted high to low)
View(missing_dataUS)

View(WineUS)

##Remove unnecessary columns (designation, region_2)
WineUS<-WineUS[,-3] #remove designation
WineUS<-WineUS[,-7] #remove region_2
WineUS<-WineUS[,-8] #remove twitter handle
View(WineUS)
str(WineUS)


#####DATA VIZ########################################
#Groups by country
countries = Wine %>%
  group_by(country) %>%
  count() 

#limits to countries with >1,000 reviews
top_countries = countries %>%
  filter(n>5000)

#Plots by country >500 reviews (US and France have most)
p<-ggplot(top_countries, aes(reorder(country,n),n)) +geom_bar(stat = "identity")  + coord_flip() + labs(title = "Countries with at Least 5,000 Reviews by Number of Reviews", y = "Number of Reviews", x = "Country")
  

#Look at Wines by Score
Best_Wines = Wine %>%
  filter(country %in% top_countries$country) %>%
  select(country,points)

Best_Wines %>%
  group_by(country) %>%
  summarise(Mean_Score = mean(points)) %>%
  arrange(desc(Mean_Score))%>%
  kable()

Best_Worst = Best_Wines %>%
  filter(country %in% c("Austria","Chile"))

ggplot(Best_Worst, aes(points, colour = country, fill = country)) + geom_density(alpha = .4) +
  labs(title ="Point Densities of Top and Bottom Countries with at Least 500 Reviews", x = "Points Given", y = "Density") 

##Correlation btw points and price (does $$matter?/Does this really show us anything?
I_Wines = Wine %>%
  select(price, points) %>%
  filter(!is.na(price)) %>%
  filter(!is.na(points))

ggplot(I_Wines, aes(x=price, y=points)) +    geom_jitter(shape=1)    + coord_cartesian(xlim = c(0, 4000), ylim = c(75, 100)) +
  labs(title ="Score vs Price", x = "Price", y = "Score")

##Looking at wineries/I think we would need to add region or country since by winery is too specific
wineries = Wine %>%
  group_by(winery) %>%
  count() 

Biggest_Wineries = wineries %>%
  filter(n>100)

Best_Wineries = Wine %>%
  filter(winery %in% Biggest_Wineries$winery) %>%
  select(winery,points)

Best_Wineries %>%
  group_by(winery) %>%
  summarise(Mean_Score = mean(points)) %>%
  arrange(desc(Mean_Score))%>%
  kable()

######################MODELS on US only###############################
View(WineUS)




