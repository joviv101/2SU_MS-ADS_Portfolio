
################################################################
# Ian_Aliman_Wine_Data
## GOAL: stay away from the whaambulance!
## IST 707 Final Project
################################################################
## Aliman, 2019
## WINE EDA
################################################################


# env 

library(tm)
library(stringr)
library(rworldmap)
library(sqldf)
library(countrycode)
install.packages(rworldmap)
library(rworldmap)
setwd('D:/documents/888_school/06_IST_707/final_project/')

# load=====================================================


Wine <- read.csv('input/winemag-data-130k-v2.csv')
head(Wine)


# UDF to clean 1 column.

cleanColumn = function(col){
  holder = vector()
  for(i in 1:length(col)){
    this_text = col[i]
    clean_text = str_extract_all(this_text, "[a-zA-Z0-9]+")
    clean_text <- paste(clean_text, sep=" ")
    holder = c(holder, clean_text)
    
  }
  
    holder[is.na(holder)] <- ""
    
  return(holder)
}


# reset types===============================================
str(Wine) 
### country has no weird characters

for (i in names(Wine)) {
  Wine[,i]<- str_replace_all(Wine[,i], "[^a-zA-Z0-9 ]", "")
}


# clean data================================================

### description cleaning
d <- cleanColumn(Wine$description)
head(d)





# auxiliary wine data sets
# https://rdrr.io/cran/rattle.data/man/wine.html
# https://www.wineindustrydata.com/lightsearch


# Replace NA with ""
df[is.na(df)] <- ""

# assess data completion===============================================

# create name of column vector
names = colnames(Wine)

# assess data completion
for (i in 1:ncol(Wine)){
  this_col = Wine[,i]
  print(paste("this column =====>",names[i]))
  print("NUMBER OF BLANK VALUES..")
  print(sum(this_col==""))
  
}

# IDEA 1: based on data completion, we could guess the reviewer (using knn or clustering)

str(df2)

# i-eda=======================================================

unique(df$country)

d <- table(df$country)


t <- tapply(df$points, df$country, mean)
ct <- tapply(df$points, df$country, length)
t <- as.data.frame(t)
ggplot(data=t) + barplot()
t$country <- rownames(t)
t$ct <- ct


cor(df)

# variety by price by region

# how much do regions matter in a country?

sumry_df <- sqldf("SELECT country, count(country) as ct, min(price), avg(price), median(price), max(price), 
                  count(distinct region_1), count(distinct winery) from Wine GROUP BY 1")


# write out country statistics.


# write.csv(sumry_df, 'output/summary_of_country_stats.csv')

##############################

# price-score relationship
Wine_cor <- Wine
# kill NA
Wine_cor[is.na(Wine_cor)] <- "" 
# keep number columns
cdf <- Wine_cor[,5:6]
# convert to numbers
cdf$points <- as.numeric(cdf$points)
cdf$price <- as.numeric(cdf$price)
# remove NA 
cdf <- na.omit(cdf)

# correlation is 0.41================================

cor(cdf$price, cdf$points)


# vars are not same scale... normalize with z score
cdf$points_scale <- scale(cdf$points)
cdf$price_scale <- scale(cdf$price)
summary(cdf)

wine_u100 <- cdf[cdf$price < 100,]
hist(wine_u100$price,100, main="wine prices are more frequent near the top of each $5 range")
abline(v=c(20, 25, 30, 35, 40,45, 50, 55,60, 65, 70,75, 80), col="red", lty=2)


# COUNTRY PLOT==============================================================================


#
# for reference on using countrycode_panel, see here
# countrycode::cldr_examples
# and ?codelist

sumry_df <- sqldf("SELECT country, count(country) as ct, min(price), avg(price), median(price), max(price), 
                  count(distinct region_1) FROM Wine GROUP BY 1")
sumry_df$country[sumry_df$country == ""] = NA
head(sumry_df)
sumry_df <- na.omit((sumry_df))
# Step 1. Get country codes.=======================================================

# avg_price_by_co <- sqldf("SELECT country, count(country) as ct FROM Wine GROUP BY ")
co_iso3c <- countrycode(sumry_df$country, 'country.name','iso3c')



priceDF <- data.frame(country = co_iso3c,
                    price = sumry_df$`median(price)`)




# merge to the map data

priceMap <- joinCountryData2Map(priceDF, joinCode = "ISO3",
                              nameJoinColumn = "country")


mapCountryData(priceMap, nameColumnToPlot="price", catMethod = "categorical",
               missingCountryCol = gray(.8))




mapCountryData(priceMap, nameColumnToPlot="price", catMethod = "categorical",
               missingCountryCol = gray(.8))


# PRICE QUESTIONS===================================================
# Do map for score.
# are prices higher when the types of wine are fewer in a region?
# are prices higher when the wine type has less competition?


# REVIEWERS======================================================================
# do reviewers have one type of wine they prefer?

# are reviewers subject to their own mean scores?

heatmap(table(Wine$taster_name, Wine$variety))

table <- tapply(Wine$X, Wine$variety,length)

mat = as.data.frame(as.matrix(table))
mat$wine_name <- row.names(mat)
row.names(mat) <- 1:nrow(mat)
summary(mat)
mat200 <- mat[mat$V1 > 200,]
summary(mat200)
# filter the base df down to over 200 count wines
Wine_hac <- Wine[Wine$variety %in% mat200$wine_name,]
heatmap(table(Wine_hac$taster_name, Wine_hac$variety))

# OR, THE REVERSE (LOOK @)

# FEATURE GENERATION==================================

# create vintage
vintage_years = c(1996:2019)
vintage_years <- as.character(vintage_years)

title <- as.character(Wine$title)
alist = vector()
a = strsplit(title, split = " ")
rm(vintage)
vintage <- vector()
for (i in 1:length(a)){
  b = a[i][[1]]
  length(b)
  # print(length(b[[1]]))
  for (j in 1:length(b)){
    if(b[j] %in% vintage_years){
      this_year = b[j]
      vintage[i] <- this_year
    }
    # else{
    #   vintage[i] <- "no year found"
    # }
  }
}

length(vintage)

Wine$vintage_year <- vintage

write.csv(Wine, 'output/Wine_plus_vintage.csv')
