library(forecast)

# Read all GDELT events for Israel/Palestine From Jan 1 1979
events = read.table("IsraelPalestine.csv", sep=",", header = TRUE)
nrow(events)

# Filter out events which are mentioned only once
select = (events$NumMentions > 1)
data = events[select, ]
nrow(data)

# Filter out events which are mentioned in one article only
select = (data$NumArticles > 1)
data = data[select, ]
nrow(data)

# Filter out actions that happened outside of Israel, WestBank and Gaza
select = data$ActionGeo_Lat <= 33 & data$ActionGeo_Lat >= 30 
data = data[select, ]
select = data$ActionGeo_Long <= 36 & data$ActionGeo_Long >= 34 
data = data[select, ]
nrow(data)

# Add Month Starting from Jan 1 1979
Month = data$MonthYear - 100*data$YEAR + 12 * (data$YEAR - 1979)
data = cbind(Month, data)

# Places in Israel, WestBank and Gaza where actions happened
actionPlace = unique(data$ActionGeo_FullName)
numPlaces   = length(actionPlace)

Summary.Year   = 0
Summary.Month  = 0
Summary.Simple = 0
Summary.Arfima = 0
Summary.Actual = 0

AvgError.Arfima = 0
AvgError.Simple = 0

# For prediction Start Month = Aug 2010
StartMonth = 380

# For prediction End Month = Aug 2014
EndMonth   = 428

for (i in StartMonth:EndMonth) {

  conflictStats = matrix(0, numPlaces, 3)

  for (j in 1:numPlaces) {

    # 1. Actual level of violence/cooperation
    Actual = subset(data, Month == i & ActionGeo_FullName == actionPlace[j])

    ActualVerbalConflict      = sum(Actual$QuadClass == 3)
    ActualMaterialConflict    = sum(Actual$QuadClass == 4)

    # Only worry about material conflict for now
    conflictStats[j,1] = ActualMaterialConflict;

    # 2. Naive prediction based on violence in previous month
    Simple = subset(data, Month == (i-1) & ActionGeo_FullName == actionPlace[j])

    SimpleVerbalConflict      = sum(Simple$QuadClass == 3)
    SimpleMaterialConflict    = sum(Simple$QuadClass == 4)

    # Only worry about material conflict for now
    conflictStats[j,2] = SimpleMaterialConflict;

    # 3.ARFIMA prediction
    ArfimaSet = subset(data, Month < i & ActionGeo_FullName == actionPlace[j])
    count = i - 1

    MonthWiseVerbalConflict   = rep(0, count)
    MonthWiseMaterialConflict = rep(0, count)

    for (k in 1:count) {
      MonthWiseVerbalConflict[k]   = sum(ArfimaSet$QuadClass == 3 & ArfimaSet$Month == k)
      MonthWiseMaterialConflict[k] = sum(ArfimaSet$QuadClass == 4 & ArfimaSet$Month == k)
    }

    # For convergence
    randoms = runif(length(MonthWiseMaterialConflict), min=0, max=.1)
    MonthWiseMaterialConflict = MonthWiseMaterialConflict + randoms

    ArfimaFit = arfima( MonthWiseMaterialConflict )
    ArfimaForecast = forecast(ArfimaFit, h=1)
    conflictStats[j,3] = as.vector( ArfimaForecast$mean )
  }

  conflictStats = round(conflictStats, digits = 0)
  index = i - (StartMonth-1)

  if (i %% 12 == 0) {
    Summary.Year[index]   = 1979 + i %/% 12 - 1
    Summary.Month[index]  = 12
  }
  else {
    Summary.Year[index]   = 1979 + i %/% 12
    Summary.Month[index]  = i %% 12
  }

  Summary.Simple[index] = sum(conflictStats[,2])
  Summary.Arfima[index] = sum(conflictStats[,3])
  Summary.Actual[index] = sum(conflictStats[,1])

  AvgError.Simple[index] = (sum(abs(conflictStats[,2] - conflictStats[,1]))) / numPlaces
  AvgError.Arfima[index] = (sum(abs(conflictStats[,3] - conflictStats[,1]))) / numPlaces
}


Results = cbind(Summary.Year, Summary.Month, Summary.Simple, Summary.Arfima, Summary.Actual, AvgError.Simple, AvgError.Arfima, AvgError.Simple - AvgError.Arfima)
colnames(Results) = c("Year", "Month", "Predicted Simple", "Predicted ARFIMA", "Actual", "Simple Error", "ARFIMA Error", "Error Difference")
Results


