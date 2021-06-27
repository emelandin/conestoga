##################################################
### PROG8430                                    ##
### Time Series Assignment 2                    ## 
##################################################
#                                               ##
##################################################
# Written by Emerson Landin
# Student ID: 8710791
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("C:/CONESTOGA/Data Analysis Math/Assignment 2 - Time Series")

options(scipen=9)

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(tseries)){install.packages("tseries")}
library("tseries")

if(!require(TTR)){install.packages("TTR")}
library("TTR")

if(!require(smooth)){install.packages("smooth")}
library("smooth")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

##################################################
### Load  R DATA                        ##
##################################################

load("Woodstock.Rdata")
DF_WD_EL <- Woodstock
str(DF_WD_EL)
head(DF_WD_EL,20)

####################################################
## TIME SERIES  - Woodstock - monthly  Data       ##
####################################################

#Convert to a Time Series datatype
Temp_WD_EL <- ts(DF_WD_EL, frequency = 12, start=c(1988,1))
head(Temp_WD_EL)

#Summarize the temperature information
summary(Temp_WD_EL)
Temp_WD_EL

###PLOT THE TIME SERIES ####

plot.ts(Temp_WD_EL, main="Average Temperature - Woodstock",  ylim = c(-15, 25)) 

### Decompose and check for Autocorrelation

decomp_EL <- decompose(Temp_WD_EL, type="additive")  
decomp_EL 
plot(decomp_EL)

adf.test(Temp_WD_EL) # p-value < 0.05  indicated series is stationary: note - can set the lags, k=n

#Deseasonalize

Temp_DS_EL <- Temp_WD_EL - decomp_EL$seasonal

plot.ts(Temp_DS_EL, main="Deseasonalized - Average Temperature - Woodstock", ylim = c(-15, 25))



#####################################################
## TIME SERIES  - Annual                           ##
#####################################################

##################################################
### Load  R DATA - Ayr Precipitation           ##
##################################################

load("Ayr.Rdata")
DF_Ayr_EL <- Ayr
str(DF_Ayr_EL)
head(DF_Ayr_EL,20)

####################################################
## TIME SERIES  -  - Annual  Data##
####################################################

Temp_Ary_EL <- ts(DF_Ayr_EL, frequency = 1, start=c(1968))  #Converts to Time Series
head(Temp_Ayr_EL)  
stat.desc(Temp_Ary_EL)


#Summarize the temperature information
summary(Temp_Ary_EL )



###PLOT THE TIME SERIES ####

plot.ts(Temp_Ary_EL, main="Average Temperature - Ayr")

#Spot trends by smoothing

Temp_Ary_SMA_EL <- SMA(Temp_Ary_EL,n=5)
plot.ts(Temp_Ary_SMA_EL)

adf.test(Temp_Ary_EL) # p-value < 0.05  indicated series is stationary

acf(Temp_Ary_EL)   #Autocorrelations

#Moving Average Forecast- Let's build a forecast

Move_avg_EL <- sma(Temp_Ary_EL)
Move_avg_EL
Move_avg_EL <- forecast(Move_avg_EL, h=5,level=0.75)   #h - periods to forecast; level=Prediction Level
Move_avg_EL
plot(Move_avg_EL)

#Exponential Smoothing Forecast

ES_avg_EL <- es(Temp_Ary_EL)
ES_avg_EL
ES_avg_EL <- forecast(ES_avg_EL, h=5,level=0.75)
ES_avg_EL
plot(ES_avg_EL)