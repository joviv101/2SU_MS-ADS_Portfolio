#2019-1002 IST 718 Group 2
#Andrew Miller, Jo Vivian, Michael Morales, David Molenda
#Code to munge the Excel files together, generate plots, and perform regression

#Required libraries
import matplotlib.pyplot as plt
import seaborn as sns
import matplotlib as mpl
import pandas as pd
import statsmodels.formula.api as smf
import numpy as np
from scipy.stats import uniform

#Function to extract the Property Name
def extractPropertyName(seriesWithPropName):
	#Property Name is in First Row
    buildString = seriesWithPropName[0]
	#Get rid of Prefix
    buildString = buildString.replace(r'Property:','')
    return buildString

#Function to extract the sq ft as an integer
def extractSqFt(seriesWithSqFt):
    #Get rid of property area text
    buildString = seriesWithSqFt.str.replace(r'Property Area:','')
    #Get rid of SqFt text, use only property area row
    buildString = buildString[1].replace(r'Sq. Ft.','')
    #Get rid of the comma
    buildString = buildString.replace(r',','')
    #Get rid of blanks and return the int
    return int(buildString.strip())

#Function to extract month name
def extractMonths(dfWithMonths):
    #Get rid of the header
    test = dfWithMonths.drop(dfWithMonths.index[[0,1,2,3,4,5]])
    #Get rid of the YTD at the bottom
    test = test.drop(dfWithMonths.index[-1])
    #Get the series and dump the year
    test2 = test.iloc[:,0].str.replace(r'/.*','')
    #Change numbers to friendly Month text
    test2 = test2.str.replace(r'10', 'October')
    test2 = test2.str.replace(r'11', 'November')
    test2 = test2.str.replace(r'12', 'December')
    test2 = test2.str.replace(r'1', 'January')
    test2 = test2.str.replace(r'2', 'February')
    test2 = test2.str.replace(r'3', 'March')
    test2 = test2.str.replace(r'4', 'April')
    test2 = test2.str.replace(r'5', 'May')
    test2 = test2.str.replace(r'6', 'June')
    test2 = test2.str.replace(r'7', 'July')
    test2 = test2.str.replace(r'8', 'August')
    test2 = test2.str.replace(r'9', 'September')
    return test2

#Function to get any of the value columns
def extractValueColumn(dfWithValues, i):
    buildBilledCost = dfWithValues.drop(file.index[[0,1,2,3,4,5]])
    buildBilledCost = buildBilledCost.drop(file.index[-1])
    return buildBilledCost.iloc[:,i]
	

#Build the dataframe to do the regression on
OverallStats = pd.DataFrame()

#Get all the files in the data directory
from os import listdir
from os.path import isfile, join
#TODO: Use your path instead of this path: /home/dm/Documents/DocumentsDS5/IST 718/Project/Data
onlyfiles = [f for f in listdir('/home/dm/Documents/DocumentsDS5/IST 718/Project/Data') if isfile(join('/home/dm/Documents/DocumentsDS5/IST 718/Project/Data', f))]

for filename in onlyfiles:
	#TODO: Use your path instead of this path: /home/dm/Documents/DocumentsDS5/IST 718/Project/Data
    tempFileName = '/home/dm/Documents/DocumentsDS5/IST 718/Project/Data/' + filename
    file = pd.read_excel(tempFileName)
    #Set up the temp DataFrame
    OneFileStats = pd.DataFrame()
	#Populate from the lines of the file
    OneFileStats['Month'] = extractMonths(file)
    OneFileStats['Billed Cost'] =  extractValueColumn(file, 1)
    OneFileStats['Occupancy Pct'] =  extractValueColumn(file, 6)
    OneFileStats['Energy Demand'] = extractValueColumn(file, 10)
	OneFileStats['Billed Usage'] = extractValueColumn(file, 11)
    OneFileStats['Property Area'] = extractSqFt(file.iloc[:,0])
	OneFileStats['Property Name'] = extractPropertyName(file.iloc[:,0])
	#Add to the overall dataframe
    OverallStats = OverallStats.append(OneFileStats)
    
#Get rid of blanks    
OverallStats = OverallStats.dropna()

#Make these numbers
OverallStats['Billed Cost'] = OverallStats['Billed Cost'].astype(int)
OverallStats['Occupancy Pct'] = OverallStats['Occupancy Pct'].astype(float)
OverallStats['Property Area'] = OverallStats['Property Area'].astype(int)
OverallStats['Energy Demand'] = OverallStats['Energy Demand'].astype(int)
OverallStats['Billed Usage'] = OverallStats['Billed Usage'].astype(int)

#Elimante outliers
OverallStats = OverallStats[OverallStats['Energy Demand'] < 100000]
OverallStats = OverallStats[OverallStats['Billed Cost'] < 900000]

#Quick pairplot
ax = sns.pairplot(OverallStats)
plt.show()

#plot the Occ vs Billed Cost
sns.set_style("darkgrid")
ax = sns.regplot(x='Occupancy Pct', y='Billed Cost', data=OverallStats)
plt.title('Billed Cost vs Occupancy')
plt.xlabel('Occupancy Percentage')
plt.ylabel('Billed Cost')
plt.xticks(rotation=45)
ax.yaxis.set_major_formatter(mpl.ticker.StrMethodFormatter('{x:,.0f}'))
plt.show()

#plot the Property Area vs Billed Cost
sns.set_style("darkgrid")
ax = sns.regplot(x='Property Area', y='Billed Cost', data=OverallStats)
plt.title('Billed Cost vs Property Area')
plt.xlabel('Property Area Sq Ft')
plt.ylabel('Billed Cost USD')
plt.xticks(rotation=45)
ax.yaxis.set_major_formatter(mpl.ticker.StrMethodFormatter('{x:,.0f}'))
ax.xaxis.set_major_formatter(mpl.ticker.StrMethodFormatter('{x:,.0f}'))
plt.show()

#Export for others to use
#TODO: Use your path instead of this path: /home/dm/Documents/DocumentsDS5/IST 718/Project/
OverallStats.to_csv('/home/dm/Documents/DocumentsDS5/IST 718/Project/CombinedData1.csv')

#Rename cols for easy regression
OverallStats2 = OverallStats
OverallStats2 = OverallStats2.rename(columns={"Billed Cost": "BilledCost", "Occupancy Pct": "Occ", "Energy Demand": "Energy", "Property Area": "Area"})

#Perform a OLS Regression
model2 = 'BilledCost ~ Occ + Energy + Area'
billedCostModel2 = smf.ols(model2, data=OverallStats2).fit()
print(billedCostModel2.summary())

#Plot the residuals
ax = sns.regplot(x=billedCostModel2.fittedvalues, y=billedCostModel2.resid_pearson, fit_reg=False)
plt.title('Residuals over Billed Cost')
plt.xlabel('Billed Cost USD')
plt.ylabel('Residuals')
plt.xticks(rotation=45)
ax.yaxis.set_major_formatter(mpl.ticker.StrMethodFormatter('{x:,.0f}'))
ax.xaxis.set_major_formatter(mpl.ticker.StrMethodFormatter('{x:,.0f}'))
plt.show()

#Train set split
np.random.seed(25)
OverallStats2['runiform'] = uniform.rvs(loc = 0, scale = 1, size = len(OverallStats2))
OverallStats2_train = OverallStats2[OverallStats2['runiform'] >= 0.33]
OverallStats2_test = OverallStats2[OverallStats2['runiform'] < 0.33]

#Run regression test
billedCostModelTrain = smf.ols(model2, data=OverallStats2_train).fit()
print(billedCostModelTrain.summary())
OverallStats2_test['predict_Billed_Cost'] = billedCostModelTrain.predict(OverallStats2_test)
OverallStats2_train['predict_Billed_Cost'] = billedCostModelTrain.predict(OverallStats2_train)

#Calculate Test and Train statistics
train_result = \
    round(np.power(OverallStats2_train['BilledCost']\
        .corr(OverallStats2_train['predict_Billed_Cost']),2),3)
print('\nProportion of Training Set Variance Accounted for: ',\
    train_result)
    
test_result = \
    round(np.power(OverallStats2_test['BilledCost']\
        .corr(OverallStats2_test['predict_Billed_Cost']),2),3)
print('\nProportion of Test Set Variance Accounted for: ',\
    test_result)
	