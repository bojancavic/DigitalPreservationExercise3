###packages
library(plotly)
library(RMySQL)
library(formattable)
library(webshot)
webshot::install_phantomjs()

#Connection to database

USER_NAME = '';
PASSWORD = '';

mydb = dbConnect(MySQL(), user=USER_NAME, password=PASSWORD, dbname='dp3')


#income
incomeRes = dbSendQuery(mydb, "select DISTRICT_CODE, REF_DATE, SUM(AVERAGE_INCOME_TOTAL) from WirtschaftWien where REF_DATE >= '2002-01-01' group by DISTRICT_CODE, REF_DATE;")
incomeTotal = fetch(incomeRes, n=-1)

#population
populationRes = dbSendQuery(mydb, "select SUM(POP_TOTAL) from BevoelkerungWien where REF_DATE >= '2003-01-01' group by DISTRICT_CODE, REF_DATE;")
populationTotal = fetch(populationRes, n=-1)


#create matrix from data

income = unlist(incomeTotal, use.names = FALSE)
population = unlist(populationTotal, use.names = FALSE)

Result = matrix(c(income, population), nrow = length(population), ncol = 4)
colnames(Result) <- c("District", "Date", "Average_Income", "Population")
write.csv(Result, file = "output/ResultData.csv",row.names=FALSE)


###### statistics ######

#mean income per district over the years
meanIncomePerDistrictRes = dbSendQuery(mydb, "select AVG(AVERAGE_INCOME_TOTAL) from WirtschaftWien group by DISTRICT_CODE;")
meanIncomePerDistrict = fetch(meanIncomePerDistrictRes, n=-1)

meanIncome = unlist(meanIncomePerDistrict, use.names = FALSE)*1000

#mean population per district over the years
meanPopulationPerDistrictRes = dbSendQuery(mydb, "select AVG(POP_TOTAL) from BevoelkerungWien where REF_DATE >= '2003-01-01' group by DISTRICT_CODE;")
meanPopulationPerDistrict = fetch(meanPopulationPerDistrictRes, n=-1)

meanPopulation = unlist(meanPopulationPerDistrict, use.names = FALSE)


#highest and lowest incomes/district over the years
incomeOverYearFirstDistrict = unlist(as.numeric(Result[1:13,3]))
incomeOverYear15District = as.numeric(Result[183:195,3])

#highest and lowest populations/district over the years
populationOverYearFirstDistrict = as.numeric(Result[1:13,4])
populationOverYear15District = as.numeric(Result[183:195,4])

#how did the income change from 2002 compared to 2014(in %) in district 1
incomeChangeFirstDistrict = round((incomeOverYearFirstDistrict[13]-incomeOverYearFirstDistrict[1])/incomeOverYearFirstDistrict[1] * 100, digits = 2)

#how did the income change from 2002 compared to 2014(in %) in district 15
incomeChange15District = round((incomeOverYear15District[13]-incomeOverYear15District[1])/incomeOverYear15District[1] * 100, digits = 2)

#how did the population change from 2002 compared to 2014(in %) in district 1
populationChangeFirstDistrict = round((populationOverYearFirstDistrict[13]-populationOverYearFirstDistrict[1])/populationOverYearFirstDistrict[1] * 100, digits = 2)

#how did the population change from 2002 compared to 2014(in %) in district 15
populationChange15District =round((populationOverYear15District[13]-populationOverYear15District[1])/populationOverYear15District[1] * 100, digits = 2)


#######plotting#######



#Mean income and population throughout the districts
Districts = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
data = data.frame(Districts, meanIncome, meanPopulation)

p = plot_ly(data, x = ~Districts, y = ~meanPopulation, type = 'bar', name = 'Population') %>%
  add_trace(y = ~meanIncome, name = 'Income') %>%
  layout(yaxis = list(title = ''), barmode = 'group')

##output plot as html file
export(p, "output/meanIncomeMeanPopulation.png")

years = c(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)

dataMeanIncome = data.frame(years, incomeOverYearFirstDistrict, incomeOverYear15District)
dataMeanPopulation = data.frame(years, populationOverYearFirstDistrict, populationOverYear15District)

#Income comparison of first and 15 district
p1 = plot_ly(dataMeanIncome, x=~years, y=~incomeOverYearFirstDistrict, name=paste("Income change in first district:", incomeChangeFirstDistrict,"%", sep = " "), type = 'scatter', mode = 'lines')%>%
  add_trace(y = ~incomeOverYear15District, name = paste('Income change in 15 district:', incomeChange15District,"%", sep=" "), mode = 'lines+markers')%>%
  layout(title = 'Income comparison of first and 15 district',
         yaxis = list(title = 'Income in 1000'),
         xaxis = list(title = 'Years'))

##output plot as html file
export(p1, "output/incomeComparison.png")

#Population comparison of first and 15 district
p2 = plot_ly(dataMeanPopulation, x=~years, y=~populationOverYearFirstDistrict, name=paste("Population change in first district:", populationChangeFirstDistrict,"%", sep = " "), type = 'scatter', mode = 'lines')%>%
  add_trace(y = ~populationOverYear15District, name = paste("Population change in 15 district:", populationChange15District,"%", sep = " "), mode = 'lines+markers')%>%
  layout(title = 'Population comparison of first and 15 district',
         yaxis = list(title = 'Population'),
         xaxis = list(title = 'Years'))

##output plot as png file
export(p2, "output/populationComparison.png")

