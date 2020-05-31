library(stringi)
Wine<- winemag.data.130k.v2
Wine<- Wine[,-1]

##Fix the rest of the cols
for (i in names(Wine)) {
  Wine[,i]<- str_replace_all(Wine[,i], "[^a-zA-Z0-9]", " ")
}

Wine$description<- as.character(Wine$description)


#frequencies ft. AMR 
WineFrequencies<-(Wine %>%
                    freq(variety))
View(WineFrequencies)
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


#remove stupid 1-2 word part that have beel left over by the above cleaning
WineTasting$description<-gsub('\\b\\w{1,2}\\s','',WineTasting$description)

#one last removal of types"
wines<- c("pinot", "chardonnay","cabernet", "sauvignon", "red", "blend", "champagne", "cab", "pinots", "pinotage", "pinotesque", "campinoti",  "pinotphiles",
          "chardonnays", "reds", "blends", "blended", "blending", "zin", "zinfandels", "zinfully", "zinfandal", "sauv", "blanc", "sauvs", "sauve", "sauvignons",
          "sauvage","sauvigon", "sauvies", "sauvignonesque", "sauvy", "sauvignonasse", "sauvig non", "sauvion", "sauvgnon", "sauvingon", "sauvingnon", "ssauvignon",
          "cabernert", "cabernets", "cabs", "moscatos", "merlots", "shirazes", "tempranillos", "malbecs", "gris", "gew rztraminer", "sangioveses", "ros", "nebbiolos",
          "malbech ", "franc", "caberent", "francs", "grigios", "grigio", "white", "whites", "blancs", "barberas")

WineTasting$description<- removeWords(WineTasting$description, wines)




#okay - while running that there are a bunch of duplicates oh my lord. 


CleanedWineTasting<-dplyr::distinct(WineTasting)

CleanedWineTasting<- CleanedWineTasting[,c(-8,-10)]

write.csv2(CleanedWineTasting, "CleanedWineTasting.csv")


DescVariety<- c("description", "variety")
WineTasting<- CleanedWineTasting[,DescVariety]
#ideally we would be able to hold the wines on the RHS of the equations, right now they have spaces
#remove spaces so that we would be able to set them to the RHS
#cool, that is ridiculously time consuming but neat
#fix the variety col
#lowercase and no spaces in the names
WineTasting$variety<- gsub(" ", "", toupper(CleanedWineTasting$variety))
#maybe need to make the names a little more distinct



#combine the cols and
rm(descriptions)
descriptions<-c("Q")


for (i in rownames(WineTasting)) {
  descriptions[i]<-paste(c(WineTasting[i,1], WineTasting[i,2]), collapse = " ")
  
}



rm(t)
t<-setNames(vector(mode = "list", length = length(descriptions)), 1:106625)


for (i in 1:NROW(descriptions)) {
  
  vert<-melt(str_split(descriptions[i], pattern = " "))
  vert$value<- as.character(vert$value)
  t[[i]]<-data.frame(cbind(rep(i, each=length(vert[,1])), (vert[,"value"])))
  
  
}

t<-(rbind.fill((t)))
tt<-t
#theres a lot of ""
tt[tt==""]<- NA


#now remove the na rows

tt<- na.omit(tt)

write.csv(tt,"wineTrans1.csv")

checkcsv<-read.csv("wineTrans1.csv")
head(checkcsv)
#import as transactions

trans<- read.transactions("wineTrans1.csv",
                          header = TRUE,
                          rm.duplicates = FALSE,
                          format = "single",
                          sep = ",",
                          cols = c(2,3))

WineVARS<-(unique(WineTasting$variety))
rules<-apriori(trans, parameter = list(supp=.001, conf=.01, minlen=5),
               appearance = list(default="lhs", rhs = c(paste(WineVARS))),
               control=list(verbose=FALSE))
arules::inspect(rules)

rules<- sort(rules, decreasing=TRUE, by = "confidence")

#########Still needs a little cleaning before running true rules

wineRawTrans<- read.csv("wineTrans1.CSV",
                        na.strings = "NA")

paste(WineVARS)
View(wineRawTrans)

#theres a lot of ""
wineRawTrans[wineRawTrans==""]<- NA


#now remove the na rows

wineRawTrans<- na.omit(wineRawTrans)

#########################################################################

PinotNoir<- apriori(trans, parameter = list(supp=.001, conf=.01, minlen=5),
                    appearance = list(default="lhs", rhs="PINOTNOIR"),
                    control=list(verbose=FALSE))

PinotNoir<-sort(PinotNoir, decreasing=TRUE, by = "confidence")
arules::inspect(PinotNoir)

Chardonnay<- apriori(trans, parameter = list(supp=.001, conf=.01, minlen=5),
                     appearance = list(default="lhs", rhs="CHARDONNAY"),
                     control=list(verbose=FALSE))
Chardonnay<-sort(Chardonnay, decreasing=TRUE, by = "confidence")
arules::inspect(Chardonnay)

CabernetSauvignon<-apriori(trans, parameter = list(supp=.001, conf=.01, minlen=5),
                           appearance = list(default="lhs", rhs="CABERNETSAUVIGNON"),
                           control=list(verbose=FALSE))
arules::inspect(CabernetSauvignon)

RedBlend<-apriori(trans, parameter = list(supp=.001, conf=.01, minlen=5),
                           appearance = list(default="lhs", rhs="REDBLEND"),
                           control=list(verbose=FALSE))
arules::inspect(RedBlend)


WineRules<- function(varName){
  apriori(trans, parameter = list(supp=.001, conf=.01, minlen=5),
          appearance = list(default="lhs", rhs=varName),
          control=list(verbose=FALSE))
}

BordeauxStyleRedBlend<-WineRules("BORDEAUXSTYLEREDBLEND")
arules::inspect(BordeauxStyleRedBlend)

Riesling<- WineRules("RIESLING")
arules::inspect(Riesling)


SauvignonBlanc<-WineRules("SAUVIGNONBLANC")
arules::inspect(SauvignonBlanc)

install.packages("shinythemes")
library(shinythemes)
library(arulesViz)
library(shin)


arulesViz::ruleExplorer(rules)




##WOOOWIE this is taking forever. Gonna come back to this.




#####################################################
####################################################
#SVM
######################################################
#so I have the cleanedWineTasting frame. I want to pull the reviews and drop them into a DTM 
#and then see if svm can predict the variety



#Idea is to first create DTM and then drop in labels after the fact

#DTM of reviews

ReviewCorp<- Corpus(VectorSource(CleanedWineTasting$description))

TDM<-TermDocumentMatrix(ReviewCorp)
#that is a massive file and I dont have enough ram
#going to remove some very sparse terms and see if I can get it to go. 

TDM<-removeSparseTerms(TDM, .99)
#geez that cleared up a lot. 
#need to play with that threshold tho


WineMat<- (as.matrix(TDM))

WineMat<- t(WineMat)
WineMat<- data.frame(as.matrix(WineMat))

WineMat$Variety<-(gsub(" ", "", toupper(CleanedWineTasting$variety)))

#reorganize the cols because it is easier for setting up sampled data.
WineMat<-WineMat[,c(406, 1:405)]
#that's a little easier for subsetting it


##We need to make a normalized model. 
##dividing by lenghth of review is an option


library(stringr)
#count spaces therefore words?? not sure how this works with the remove sparse terms
#gonna also run the raw data through svm

#basis for loop:
#WineMat[2,2:406]/str_count(CleanedWineTasting$description[2], pattern = " ")
NormaizedWine<-WineMat
NormaizedWine[,2:406]<- MinMax(NormaizedWine[,2:406])

  MinMax <- function(x)
  {
    return((x- min(x)) /(max(x)-min(x)))
  }

NormaizedWine[1:5,]
UnnormalizedWine<-WineMat
#make train and test sets for both

#normalized

NormalTrainRows<-as.numeric(sample(rownames(NormaizedWine), dim(NormaizedWine)[1]*6/11))

NormalTrain<- NormaizedWine[NormalTrainRows,]
NormalTest<- NormaizedWine[-NormalTrainRows,]


##Remove labels from test
NormalTestLabels<- NormalTest[,1]
#drop labels from testset
NormalTest<- NormalTest[,-1]
str(NormalTestLabels)

###########  SVM  #############
library(e1071)
#install.packages("naivebayes")
library(naivebayes)

NormalModel<- svm(Variety~., NormalTrain, kernel = "linear", type = "C-classification")
ResultsSVM<- predict(NormalModel, NormalTest, type = "class")
caret::confusionMatrix(ResultsSVM, as.factor(NormalTestLabels))

View(cbind(ResultsSVM, as.factor(NormalTestLabels)))


BayesModel<- naive_bayes(Variety~., NormalTrain)
ResultsBayes<- predict(BayesModel, NormalTest, type = "class")
caret::confusionMatrix(ResultsBayes, as.factor(NormalTestLabels))



install.packages("class")
library(class)
knnTrainLabels<- NormalTrain[,1]
KNNTrain<- NormalTrain[,-1]
KNNModel<- knn(KNNTrain, NormalTest,cl=knnTrainLabels, k= 8, prob = FALSE)



















