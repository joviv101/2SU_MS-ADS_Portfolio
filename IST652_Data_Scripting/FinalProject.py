# -*- coding: utf-8 -*-
"""
Created on Thu May  9 17:12:55 2019

@author: joviv
"""


#After cleaning the data, we compare how many installs free apps get compared to paid apps
# Load the Pandas libraries with alias 'pd' 
import pandas as pd 
import numpy as np
# Read data from file 'googleplaystore.csv' 
AppsDF = pd.read_csv("C:/Users/joviv/programsjov/googleplaystore.csv") 
#get shape and dimension
AppsDF.shape
AppsDF.ndim
#get data types
AppsDF.dtypes
#remove nonsensical row
AppsDF = AppsDF.drop([10472], axis=0)
#change Reviews to integer
AppsDF['Reviews']=AppsDF['Reviews'].astype(int)

#start cleaning data

#remove 'M' and 'K' and replace with appropriate number of zeros
#get rid of any plus signs in Size column
AppsDF['Size'] = AppsDF['Size'].str.replace('\+', '')
#then get rid of Varies with device
AppsDF['Size'] = AppsDF['Size'].str.replace('Varies with device', 'NaN')
#replace M & k with appropriate exponential
AppsDF['Size'] = AppsDF['Size'].str.replace('M', 'e+6')
AppsDF['Size'] = AppsDF['Size'].str.replace('k', 'e+3')
#convert Size column to float
AppsDF['Size'] = AppsDF['Size'].astype(float)
#remove NA from size column
AppsDF['Size'].fillna(0, inplace = True)
#get rid of the plus sign and comma in Installs
AppsDF['Installs'] = AppsDF['Installs'].str.replace('\+','')
AppsDF['Installs'] = AppsDF['Installs'].str.replace('\,','')
#convert installs to integer
AppsDF['Installs']=AppsDF['Installs'].astype(int)
#convert price to float for calculations 
AppsDF['Price'] = AppsDF['Price'].str.replace('\$','')
AppsDF['Price'] = AppsDF['Price'].str.replace(' ', '')
AppsDF['Price'] = AppsDF['Price'].astype(float)

#Now that the data is cleaned, let's do some analysis
#first import more libraries
import matplotlib.pyplot as plt
import statsmodels.api as sm
#then set a new dataframe to play with - in case you mump it up.
WorkDF = AppsDF


#run comparisons
#How are installs related to category? Do some categories perform better than others?
#Group by installs and category 
MinInstallbyCat = WorkDF.groupby('Category')['Installs'].min()
MaxInstallbyCat = WorkDF.groupby('Category')['Installs'].max()
AvgInstallbyCat = WorkDF.groupby('Category')['Installs'].mean()

#How many number of revews do you need to get at least 1,000,000 installs? 
#Group by number of reviews and number of installs
MinInstallbyRev = WorkDF.groupby('Reviews')['Installs'].min()
MaxInstallbyRev = WorkDF.groupby('Reviews')['Installs'].max()
AvgInstallbyRev = WorkDF.groupby('Reviews')['Installs'].mean()


#Begin with installs against category
InstallbyCat = WorkDF.groupby('Category')['Installs'].sum()

#print graph
InstallbyCat.plot.bar()
plt.show()

#Next is with installs against ratings
InstallbyRat = AppsDF.groupby('Rating')['Installs'].count()

#print graph
InstallbyRat.plot.bar()
plt.show()


#Next is with installs against number of reviews
InstallbyRev = WorkDF.groupby('Reviews')['Installs'].mean()

#print graph
InstallbyRev.plot.line()
plt.show()

#Content Rating compared to number of installs
InstallbyContent = WorkDF.groupby('Content Rating')['Installs'].sum()

#print graph
InstallbyContent.plot.bar()
plt.show()

#size of the app compared to number of installs
InstallbySize = WorkDF.groupby('Size')['Installs'].count()

#print graph
InstallbySize.plot()
plt.show

#cost of the app compared to number of installs
InstallbyCost = WorkDF.groupby('Type')['Installs'].count()

#print graph
InstallbyCost.plot.bar()
plt.show

#find min values

MinInstalls=min(WorkDF['Installs'])
MinReviews=min(WorkDF['Reviews'])
MinSize=min(WorkDF['Size'])
MinRating=min(WorkDF['Rating'])

#find max values

MaxInstalls=max(WorkDF['Installs'])
MaxReviews=max(WorkDF['Reviews'])
MaxSize=max(WorkDF['Size'])
MaxRating=max(WorkDF['Rating'])

#find and print averages
AvgInstalls= WorkDF['Installs'].mean()
AvgReviews=WorkDF['Reviews'].mean()
AvgSize= WorkDF['Size'].mean()
AvgRating= WorkDF['Rating'].mean()
AvgPrice = WorkDF['Price'].mean()
print('Average Installs: ', AvgInstalls)
print('Average Reviews: ',AvgReviews)
print('Average Size: ',AvgSize)
print('Average Rating: ',AvgRating)
print('Average Price: ',AvgPrice)

#determine why the rating chart has odd results by looking at the average Rating and how many apps are rated above 4.2
FiveRating = WorkDF[ WorkDF['Rating'] == 5]
PercentFiveRating = (len(FiveRating)/len(WorkDF['Rating']))*100
print('Percentage of Apps rated at 5.0', PercentFiveRating)


#Find out how many Apps are in each Content Rating Category
Teen = WorkDF['Content Rating'].str.count("Teen")
CountTeen = sum(Teen)
Everyone = WorkDF['Content Rating'].str.count("Everyone")
CountEveryone = sum(Everyone)
TenPlus = WorkDF['Content Rating'].str.count("Everyone 10+")
CountTenPlus = sum(TenPlus)
Adults = WorkDF['Content Rating'].str.count("Adults only 18+")
CountAdult = sum(Adults)
Mature = WorkDF['Content Rating'].str.count("Mature 17+")
CountMature = sum(Mature)
UnRated = WorkDF['Content Rating'].str.count("Unrated")
CountUnRated = sum(UnRated)
#print percentage of the main rating
PerEv = (CountEveryone/10842)*100
print('Percentage of Apps rated for Everyone', PerEv)

#find out how many apps are comms and games and print percentage
Comms = WorkDF['Category'].str.count('COMMUNICATION')
Games = WorkDF['Category'].str.count('GAME')
CountComms = sum(Comms)
CountGames = sum(Games)
PerComms = (CountComms/10842)*100
PerGames = (CountGames/10842)*100
print('Percentage of Game Apps: ', PerGames)
print('Percentage of Communication Apps: ', PerComms)


#run a correlation matrix
RevsCorr = WorkDF.corr(method='pearson', min_periods=1)
print(RevsCorr)

#do a linear regression to determine if the hypothesis that installs is impacted by the other variables is true
#need a temp Rating column that ignores NAs
tempDF = WorkDF
tempDF['Rating'].fillna(0, inplace = True)
X = tempDF[["Rating", "Size", "Price", "Reviews"]]
y = tempDF["Installs"]
model = sm.OLS(y, X).fit()
predictions = model.predict(X)
model.summary()
print(model.summary())

#rerun linear regression without Reviews
X = tempDF[["Rating", "Size", "Price"]]
y = tempDF["Installs"]
model = sm.OLS(y, X).fit()
predictions = model.predict(X)
model.summary()
print(model.summary())

#transform data to get a better linear regression
transDF = tempDF
transDF['Installs'] = np.sqrt(transDF['Installs'])
transDF['Size'] = np.sqrt(transDF['Size'])
transDF['Rating'] = np.sqrt(transDF['Rating'])
transDF['Price'] = np.sqrt(transDF['Price'])

#rerun linear model with transformed data
X = transDF[["Rating", "Size", "Price"]]
y = transDF["Installs"]
model = sm.OLS(y, X).fit()
predictions = model.predict(X)
model.summary()
print(model.summary())
