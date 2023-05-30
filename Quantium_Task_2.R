library(data.table)
library(ggplot2)
library(tidyr)
library(ggplot2)
library(ggmosaic)
library(plyr)
library(stringr)
library(lubridate) 
library(dplyr)
library(stringr)

# Get working directory
getwd()


filepath <- "C:/Users/PRAISE/Documents/R_Forcast/Personal_Project"
data <- fread(paste0(filepath,"QVI_data.csv"))

# Set themes for plots
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))


str(data)
View(data)

# data[, YEARMONTH := format(year(data$DATE)* 100 + month(data$DATE))]

data$YEARMONTH <- year(data$DATE)* 100 + month(data$DATE)
measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID) / uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY) / uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES) / sum(PROD_QTY)),
                        by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]



storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% 
                                      storesWithFullObs, ]

storesWithFullObs
preTrialMeasures
measureOverTime

help("eval")
#correlation function
calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
  calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure =
                               numeric())
  storeNumbers <-unique(inputTable[, STORE_NBR])
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison,
                                   "Store2" = i,
                                   "corr_measure" = cor(inputTable[STORE_NBR == storeComparison, eval(metricCol)], inputTable[STORE_NBR==i, eval(metricCol)])
    )
    calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
  }
  return(calcCorrTable)
}

# Create a function to calculate a standardized magnitude distance 


calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
  calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH =
                               numeric(), measure = numeric())
  storeNumbers <- unique(inputTable[, STORE_NBR])
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison
                                   , "Store2" = i
                                   , "YEARMONTH" = inputTable[STORE_NBR ==
                                                                storeComparison, YEARMONTH]
                                   , "measure" = abs(inputTable[STORE_NBR ==
                                                                  storeComparison, eval(metricCol)]
                                                     - inputTable[STORE_NBR == i,
                                                                  eval(metricCol)])
    )
    calcDistTable <- rbind(calcDistTable, calculatedMeasure)
    
    
  }

  
  minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)), 
                              by = c("Store1", "YEARMONTH")]
  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
  
  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by = 
                                .(Store1, Store2)]
  return(finalDistTable)
}


#  calculate correlations against store 77 using total sales and number of customers.
trial_store <- 77
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)


# Then, use the functions for calculating magnitude.
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), 
                                               trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, 
                                                   quote(nCustomers), trial_store)



# Create a combined score composed of correlation and magnitude.
corr_weight <- 0.5
score_nSales <- merge(corr_nSales,magnitude_nSales, by = c("Store1", "Store2"))[, scoreNSales := corr_measure*corr_weight +mag_measure*(1-corr_weight)]
score_nCustomers <- merge(corr_nCustomers,magnitude_nCustomers , by = c("Store1", "Store2"))[, scoreNCust := corr_measure*corr_weight +mag_measure*(1-corr_weight)]

##Combine scores across the drivers by first merging our sales scores and customer scores into a single table
score_Control <- merge(score_nSales,score_nCustomers, by = c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]


## Select the most appropriate control store for trial store 77 by finding the store with the highest final score.
control_store <- score_Control[Store1==trial_store, ][order(-finalControlScore)][2,Store2]
control_store
# we have 233 control stores

# let's check visually if the drivers are indeed similar in the period before the trial.
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
                                                         "Trial",
                                                         ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH",
                                       "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/%
                                        100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


# Conduct visual checks on customer count trends by comparing the trial store to the control store and other stores.


measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR==control_store, "Control", "Others"))
][,num_Customers :=mean(nCustomers), by = c("YEARMONTH", "Store_type")
][,TransactionMonth := as.Date(paste(YEARMONTH%%100, YEARMONTH%%100,1, sep='-'), "%Y-%m-%d")
][YEARMONTH<201903, ]

ggplot(pastCustomers, aes(TransactionMonth,num_Customers , color = Store_type)) +
  geom_line() +
  labs(x = "Month of Operation", y = "Number of Customers", title = "Number of Customers per month")

## Assessment of trial

scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[
  STORE_NBR == control_store & YEARMONTH < 201902, sum(totSales)]

## Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][, controlSales := totSales * scalingFactorForControlSales]

str(scaledControlSales)

# Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR==trial_store, c("totSales", "YEARMONTH")], 
                        by = "YEARMONTH")[, percentageDiff := abs(controlSales-totSales)/controlSales]

#standard deviation based on the scaled percentage difference in the pre-trial period 
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7


# Calculate the t-values for the trial months

percentageDiff[, tValue := (percentageDiff/stdDev)
][, TransactionMonth := as.Date(paste(YEARMONTH%%100, YEARMONTH%%100,1, sep='-'), "%Y-%m-%d")
][YEARMONTH<201905 & YEARMONTH > 201901, .(TransactionMonth, tValue)]

qt(0.95, df = degreesOfFreedom)


measureOverTimeSales <- measureOverTime
#### Trial and control store total sales

# Create new variables Store_type, totSales and TransactionMonth in the data table.
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Others"))
][, totSales := mean(totSales)
][, TransactionMonth := as.Date(paste(YEARMONTH %/%
                                        100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"), ]


#### Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control", 
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence 
interval"]

#### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control", 
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence 
interval"]


trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)


# Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,], 
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax = 
                  Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

#  Compute a scaling factor to align control store customer counts to trial store.
# Apply the scaling factor to control store customer counts.
# Finally, calculate the percentage difference between scaled control store customers and trial customers. 

scalingFactorForControlCust <- preTrialMeasures[STORE_NBR==trial_store & YEARMONTH< 201902, sum(nCustomers)/ 
                                                  preTrialMeasures[STORE_NBR==control_store & YEARMONTH< 201902, sum(nCustomers)]]

measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR==control_store,
][, controlCustomers := nCustomers*scalingFactorForControlCust
][, Store_type :=ifelse(STORE_NBR==trial_store, "Trial", ifelse(STORE_NBR==control_store, "Control", "Other"))]

percentageDiff <- merge(scaledControlCustomers[,c("YEARMONTH", "controlCustomers")], 
                        measureOverTimeCusts[STORE_NBR==trial_store, c("nCustomers", "YEARMONTH")], by="YEARMONTH")[, percentageDiff:=
                                                                                                                      abs(controlCustomers-nCustomers)/controlCustomers]


stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff]) 
degreesOfFreedom <- 7 
#### Trial and control store number of customers
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = 
                                        c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]
#### Control store 95th percentile
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control", 
][, nCusts := nCusts * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]
#### Control store 5th percentile
pastCustomers_Controls5 <- pastCustomers[Store_type == "Control", 
][, nCusts := nCusts * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence 
interval"]
trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, 
                         pastCustomers_Controls5)


# Plot everything into one nice graph.
## geom_rect creates a rectangle in the plot. Use this to highlight the trial period in our graph.
ggplot(trialAssessment, aes(TransactionMonth, nCusts, color=Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,], 
            aes(xmin = min(TransactionMonth), xmax =max(TransactionMonth) , ymin = 0, ymax = Inf, color = NULL), 
            show.legend = FALSE) +
  geom_line() +
  labs(x ="Month of Operation", y="Number of Customers", title = "Total Customers per Month") 



### repeat finding the control store and assessing the impact of the trial for each of the other two trial stores.

## For trial store 86


measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID) / uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY) / uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES) / sum(PROD_QTY)),
                        by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]



measureOverTime

trial_store_ = 86

preTrialMeasures

#calculate correlations and magnitude for each potential control store

corr_nSales_ <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store_)
corr_nCustomers_ <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store_)

magnitude_nSales_ <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store_)
magnitude_nCustomers_ <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store_)

# lets see what our data looks like.
corr_nCustomers_
corr_nSales_
magnitude_nCustomers_
magnitude_nSales_
# let's create a combined score composed of correlation and magnitude




corr_weight_ <- 0.5
score_nSales_ <- merge(corr_nSales_,magnitude_nSales_, by = c("Store1", "Store2"))[, score_NSales := corr_measure*corr_weight_ +mag_measure*(1-corr_weight_)]
score_nCustomers_ <- merge(corr_nCustomers_,magnitude_nCustomers_ , by = c("Store1", "Store2"))[, scoreN_Cust := corr_measure*corr_weight_ +mag_measure*(1-corr_weight_)]

score_nSales_
score_nCustomers_

# Finally, combine scores across the drivers using a simple average.
score_Control_ <- merge(score_nSales_, score_nCustomers_,by=c("Store1", "Store2"))
score_Control_[, finalControlScore := score_NSales * 0.5 + scoreN_Cust * 0.5]

score_Control_

# Select control stores based on the highest matching store
# Select control store for trial store 86
control_store_ <- score_Control_[Store1 == trial_store_, 
][order(-finalControlScore)][2, Store2]
control_store_ #155.

# Visual Check on trends

measureOverTimeSales_ <- measureOverTime ;measureOverTimeSales_
pastSales_ <- measureOverTimeSales_[, Store_type_ := ifelse(STORE_NBR==trial_store_, "Trial Stores",
                                                            ifelse(STORE_NBR==control_store_, "Control Stores", "Others"))
][, totSales_ := mean(totSales), by = c("YEARMONTH", "Store_type_")
][, TransactionMonth_ := as.Date(paste(YEARMONTH %/%
                                         100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]
ggplot(pastSales_, aes(TransactionMonth_, totSales_,color= Store_type_)) +
  geom_line() +
  labs(x= "Month of Operation", y= "Total Sales", title= "Total Monthly Sales")


# Next, number of customers.

## let's Conduct visual checks on trends based on the drivers
measureOverTimeCusts_ <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type_ := ifelse(STORE_NBR==trial_store_,"Trial Store",
                                                              ifelse(STORE_NBR==control_store_, "Control Store", "Others"))
][, numberCustomers_ := mean(nCustomers), by = c("YEARMONTH", "Store_type_")
][, TransactionMonth := as.Date(paste(YEARMONTH%%100, YEARMONTH%%100,1, sep='-'), "%Y-%m-%d")
][YEARMONTH < 201903 , ]
ggplot(pastCustomers, aes(TransactionMonth, numberCustomers_, color =Store_type_,)) +
  geom_line() +
  labs(x= "Month of Operation", y= "Number of Customers", "Total Number of Customers Per Month")

## Scale pre-trial control sales to match pre-trial trial store sales 
scalingFactorForControlSales_ <- preTrialMeasures[STORE_NBR == trial_store_ & 
                                                    YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store_ & 
                                                                                                          YEARMONTH < 201902, sum(totSales)]
#### Apply the scaling factor
measureOverTimeSales_ <- measureOverTime
scaledControlSales_ <- measureOverTimeSales[STORE_NBR == control_store, ][ , 
                                                                           controlSales := totSales * scalingFactorForControlSales_]
measureOverTime
scaledControlSales_
### Calculate the percentage difference between scaled control sales and trial sales
#### Hint: When calculating percentage difference, remember to use absolute difference 

percentageDiff_ <- merge(scaledControlSales_[, c("YEARMONTH", "controlSales")],
                         measureOverTime[STORE_NBR==trial_store_, c("totSales", "YEARMONTH")],
                         by = "YEARMONTH")[, percentageDiff := abs(controlSales-totSales)]


# Calculate the standard deviation of percentage differences during the pre-trial period
stdDev <-sd(percentageDiff_[YEARMONTH < 201902, percentageDiff])
degreesOfFreedom <- 7


#### Trial and control store total sales
#### Create a table with sales by store type and month.
#### We only need data for the trial and control store.
measureOverTimeSales <- measureOverTime
pastSales_ <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR==trial_store_,"TRIAL",
                                                          ifelse(STORE_NBR==control_store_, "CONTROL", "OTHERS"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/%
                                        100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("TRIAL", "CONTROL"), ]


## Calculate the 5th and 95th percentile for control store sales.
#### Hint: The 5th and 95th percentiles can be approximated by using two standard deviations away from the mean.
#### Hint2: Recall that the variable stdDev earlier calculates standard deviation in percentages, and not dollar sales.

pastSales_Controls_95 <- pastSales_[Store_type == "CONTROL", 
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence 
interval"]


pastSales_Controls_5 <- pastSales_[Store_type == "CONTROL", 
][, totSales := totSales * (1- stdDev *2)
][, Store_type := "Control 5th % confidence 
interval"]


#### Then, create a combined table with columns from pastSales, pastSales_Controls_95 and pastSales_Controls_5
trialAssessment_ <- rbind(pastSales_, pastSales_Controls_95, pastSales_Controls_5) 


#### Plotting these in one nice graph
ggplot(trialAssessment_, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment_[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax = 
                  Inf, color = NULL), show.legend = FALSE) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

trial_store_88 <- 88

corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store_88)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store_88)

corr_nSales 
corr_nCustomers

#### Use the functions from earlier to calculate the magnitude distance of the sales and number of customers of each potential control store to the trial store

magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store_88)
magnitude_nCustomers <-calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store_88)

magnitude_nSales
magnitude_nCustomers

## Create a combined score composed of correlation and magnitude by merging the correlations table and the magnitudes table, for each driver.

corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by= c("Store1", "Store2"))[, scoreNsales := corr_measure*corr_weight +mag_measure *(1-corr_weight)]
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by= c("Store1", "Store2"))[, scoreNcustomers := corr_measure * corr_weight + mag_measure * (1-corr_weight)]

### Combine scores across the drivers by merging sales scores and customer scores,and compute a final combined score.
score_Control <- merge(score_nSales, score_nCustomers, by= c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNsales * 0.5 + scoreNcustomers *0.5]

score_Control
#### Select control stores based on the highest matching store 
#### (closest to 1 but not the store itself, i.e. the second ranked highest store)
#### Select control store for trial store 88
control_store <- score_Control[Store1 == trial_store_88, ][order(-finalControlScore)][2,Store2]
control_store #237

#### Visual checks on trends based on the drivers
#### For the period before the trial, create a graph with total sales of the trial store for each month, compared to the control store and other stores.
measureOverTimeSales <- measureOverTime
measureOverTimeSales

pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR==trial_store_88,"Trial",
                                                         ifelse(STORE_NBR==control_store, "Control","Others"))
][, totsales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, Transaction_month := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903, ]

ggplot(pastSales, aes(x= Transaction_month, y = totsales, color= Store_type)) +
  geom_line() +
  labs(x= "Month of Operation", y= "Total Sales", title = "Total Monthly Sales")

#Great, the trial and control stores have similar total sales.
#Next, number of customers.

#### Visual checks on trends based on the drivers
#### For the period before the trial, create a graph with customer counts of the trial store for each month, compared to the control store and other stores.
measureOverTimeCusts <- measureOverTime
measureOverTimeCusts
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR==trial_store_88, "Trial",
                                                             ifelse(STORE_NBR==control_store, "Control","Others"))
][, num_cust := mean(nCustomers), by= c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")][
  YEARMONTH <201903, ]
ggplot(pastCustomers, aes(TransactionMonth, num_cust, color= Store_type)) +
  geom_line() +
  labs(x= "Month of Operation", y= "Number of Customers", "Monthly Number of Customers")


#### Scale pre-trial control store sales to match pre-trial trial store sales 
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR==trial_store_88 & YEARMONTH < 201902, sum(totSales)]/
  preTrialMeasures[STORE_NBR==control_store & YEARMONTH < 201902, sum(totSales)]
## Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR==control_store, ][, controlStoreSales:= totSales*scalingFactorForControlSales] 

scaledControlSales
# Calculate the absolute percentage difference between scaled control sales and trial sales

percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlStoreSales")],
                        measureOverTime[STORE_NBR==trial_store_88, c("totSales", "YEARMONTH")],
                        by = "YEARMONTH")[, PercentageDiff := abs(controlStoreSales-totSales)/controlStoreSales]

#### As our null hypothesis is that the trial period is the same as the pre-trial 
#period, let's take the standard deviation based on the scaled percentage difference in the pre-trial period 
stdDev <- percentageDiff[YEARMONTH <201902, PercentageDiff]

degreesOfFreedom <- 7
#### Trial and control store total sales

measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type:= ifelse(STORE_NBR == trial_store_88, "Trial",
                                                        ifelse(STORE_NBR==control_store, "Control_Store", "Others"))
][, Totsales := mean(totSales), by=c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control")]


pastSales
#### Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type=="Control_Store"
][, totSales_ := totSales* (1 + stdDev *2)
][, Store_type := "95% Control interval"]


#### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control_Store"
][, totSales_ := totSales *(1-stdDev*2)
][, Store_type := "5 Control interval"]
#### Combine the tables pastSales, pastSales_Controls95, pastSales_Controls5
trialAssessment <- rbind(trialAssessment, pastCustomers_Controls95, pastCustomers_Controls5)

trialAssessment
#### Plot these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, totSales, color= Store_type)) +
  geom_rect(data=trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901, ],
            aes(xmin=min(TransactionMonth),xmax=max(TransactionMonth), ymin=0, ymax=Inf,
                color = NULL), show.legend=FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month") 


#### This would be a repeat of the steps before for total sales
#### Scale pre-trial control store customers to match pre-trial trial store customers 
scalingFactorForControlCust <- preTrialMeasures[STORE_NBR==trial_store_88 & YEARMONTH < 201902, sum(nCustomers)]/
  preTrialMeasures[STORE_NBR==control_store & YEARMONTH < 201902, sum(nCustomers)]

#### Apply the scaling factor
measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR==control_store][,
                                                                         Control_Customers := nCustomers*scalingFactorForControlCust][,
                                                                                                                                      Store_type := ifelse(STORE_NBR==trial_store_88, "Trial",
                                                                                                                                                           ifelse(STORE_NBR == control_store, "Control", "Others"))]
#### Calculate the absolute percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "Control_Customers")],
                        measureOverTimeCusts[STORE_NBR == "Trial", c("nCustomers", "YEARMONTH")], by="YEARMONTH")[, Percentdiff:= abs(Control_Customers-nCustomers)]

#### As our null hypothesis is that the trial period is the same as the pre-trial 
stdDev <- sd(percentageDiff[YEARMONTH < 201902, Percentdiff])
degreesOfFreedom <- 7 

# note that there are 8 months in the pre-trial period hence 
# 8 - 1 = 7 degrees of freedom
#### Trial and control store number of customers
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = 
                                        c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]
#### Control store 95th percentile
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control", 
][, nCusts := nCusts * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]
#### Control store 5th percentile
pastCustomers_Controls5 <- pastCustomers[Store_type == "Control", 
][, nCusts := nCusts * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence 
interval"]
#### Combine the tables pastSales, pastSales_Controls95, pastSales_Controls5
trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95, 
                         pastCustomers_Controls5)
trialAssessment
#### Plotting these in one nice graph
ggplot(trialAssessment, aes(Transaction_month, nCusts, color=Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,], 
            aes(xmin = min(TransactionMonth), xmax =max(TransactionMonth) , ymin = 0, ymax = Inf, color = NULL), 
            show.legend = FALSE) +
  geom_line() +
  labs(x ="Month of Operation", y="Number of Customers", title = "Total Customers per Month") 






