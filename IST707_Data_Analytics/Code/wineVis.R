## Wine Visuals
install.packages("rworldmap")
install.packages("countrycode")
library(countrycode)
library(DT)
library(rworldmap)
library(dplyr)
library(AMR)
library(RColorBrewer)
library(plotly)
str(CleanedWineTasting)

##Create summary viz
ColsWine<- colnames(CleanedWineTasting)
Datatypes<-c("Factor", "Chr", "Chr", "int", "int", "Factor", "Factor", "Factor","Chr", "Factor", "Chr")
Unique_Values<-c(length(unique(CleanedWineTasting$country)),length(unique(CleanedWineTasting$description)),length(unique(CleanedWineTasting$designation)), length(unique(CleanedWineTasting$points)),
                length(unique(CleanedWineTasting$price)), length(unique(CleanedWineTasting$province)),length(unique(CleanedWineTasting$region)), length(unique(CleanedWineTasting$taster_name)),
                length(unique(CleanedWineTasting$title)), length(unique(CleanedWineTasting$variety)), length(unique(CleanedWineTasting$winery)))

SummaryDF<-data.frame(cbind(ColsWine,Datatypes,Unique_Values))
View(SummaryDF)
## make DT Table output

datatable(SummaryDF, colnames = c("Variable", "Data Type", "# of Unique"),  options = list(dom = "t", ordering=F), caption ="Data Structures", rownames = FALSE)
str(SummaryDF)

############
##Map
###########
countryfreq<- CleanedWineTasting %>%
  freq(as.factor(country))
countryfreq<-data.frame(countryfreq[,1:2])
#remove "" ugh
countryfreq[countryfreq==""]<- NA
countryfreq<- na.omit(countryfreq)
###### Graph time #
mapcountries<- c("DEU", "COD", "BFA")

countryfreq$item<-(countrycode(as.character(countryfreq$item), 'country.name','iso3c'))
# england didnt work doing that manually
countryfreq$item[18]<- c("GBR")
WineMap<- joinCountryData2Map(countryfreq, joinCode = "ISO3",nameJoinColumn = "item")
colorPala
mapCountryData(WineMap, nameColumnToPlot = "count", catMethod = "categorical", missingCountryCol = gray(.9), addLegend = FALSE, colourPalette= brewer.pal(9,'Reds') )
?brewer.pal('Reds')


## box plot for price
prices<-data.frame(na.omit(as.numeric(CleanedWineTasting$price)))
##Price
plot_ly(CleanedWineTasting, x=as.numeric(na.omit(CleanedWineTasting$price)), type = "box", name =  "Wine Prices",  color = "darkRed", marker = list(color = "maroon")) %>%
  layout(title = "Wine Price Boxplot")
##Points   
plot_ly(CleanedWineTasting, x=as.numeric(na.omit(CleanedWineTasting$points)), type = "box", name =  "Wine Score", color = "maroon",marker = list(color = "red")) %>%
  layout(title = "Wine Points Boxplot")


############
#top 10 varietals table
varietalfreq<- CleanedWineTasting %>%
  freq(as.factor(variety))
varietalfreq<- data.frame(varietalfreq[,1:2])

datatable(varietalfreq, colnames = c("Wine Variety", "Frequency"),caption = "Count of Wine Varieties", options = list(dom = "t", ordering=F), rownames = FALSE )

#top 10 wineries table
wineriesfreq<- CleanedWineTasting %>%
  freq(as.factor(winery))
wineriesfreq<- data.frame(wineriesfreq[,1:2])

datatable(wineriesfreq, colnames = c("Winery", "Frequency"),caption = "Count of Wineries", options = list(dom = "t", ordering=F), rownames = FALSE )
##############


which(Wine$price == "3000")
unique(CleanedWineTasting[which(as.numeric(CleanedWineTasting$points) == 100),])


length(which(Wine$taster_name == ""))

length(which(Wine$points == ""))

w<-na.omit(CleanedWineTasting)
