##########################################################
# Dissertation: Aseess the impact of current trade dispute on the economies of China and the U.S. 
# 180025784
# August 2019
##########################################################
# setwd("/Users/apple/Desktop/MSc Dissertation/Datasets")

# Clear all variables in workspace
rm(list = ls())

# Load the packages 
# install.packages("forecast")
# install.packages("fpp2")
# install.packages("Rmisc")
library(readxl)
library(forecast)
library(fpp2)
library(ggplot2)
library(Rmisc)
library(psych)

##########################################################
##########################################################
# 1 Comparison of predicted value & actual value
##########################################################
##########################################################

# Load the data from Excel
trade <- read_xlsx("BalanceofTrade.xlsx")

# Store the import/export as time-series objects
import <- ts(trade$import[1:398], frequency = 12, start = c(1985, 1))
import_trade <- ts(trade$import, frequency = 12, start = c(1985, 1))
export <- ts(trade$export[1:398], frequency = 12, start = c(1985, 1))
export_trade <- ts(trade$export, frequency = 12, start = c(1985, 1))
balance <- ts(trade$`balance of payment`, frequency = 12, start = c(1985,1))

##########################################################
# Data Manipulation
##########################################################
# Time plot of original data 
autoplot(balance, col = "orange") +
  autolayer(import_trade) +
  autolayer(export_trade) +
  autolayer(balance) +
  ggtitle("Time Plot: U.S. Trade in Goods with China") +
  ylab("Millions of U.S. Dollars")
  

autoplot(import_trade) + 
  ggtitle("Time Plot: U.S. Trade in Goods with China - Imports") +
  ylab("Millions of U.S. Dollars")
autoplot(export_trade) + 
  ggtitle("Time Plot: U.S. Trade in Goods with China - Exports") +
  ylab("Millions of U.S. Dollars")
#*Sharp drop in 2008-2009, should be due to Great Recession
#*Also another sharp drop in 2018-2019, may be due to Trade War

# Time plot of original data (before trade war)
p1 <- autoplot(import) + 
  ggtitle("Time Plot: U.S. Trade in Goods with China - Imports from Jan 1985 to Feb 2018") +
  ylab("Millions of U.S. Dollars")

p2 <- autoplot(export) + 
  ggtitle("Time Plot: U.S. Trade in Goods with China - Exports from Jan 1985 to Feb 2018") +
  ylab("Millions of U.S. Dollars")

multiplot(p1, p2, cols = 1)
#*As time series goes on, the fluctuations increase

# Transform time series using natural log of the original data
log_import <- log(import)
log_import_trade <- log(import_trade)
log_export <- log(export)
log_export_trade <- log(export_trade)
#*Now the size of the fluctuations could to be roughly constant over time

##########################################################
# Preliminary analysis
##########################################################
# Basic statistics
describe(trade$import)
describe(trade$export)
describe(trade$`balance of payment`)

# Time plot
p3 <- autoplot(log_import) + 
  ggtitle("Log-transformed Time Plot: U.S. Trade in Goods with China - Imports from Jan 1985 to Feb 2018") +
  ylab("Millions of U.S. Dollars 
       (Placed on Log Scale)")

p4 <- autoplot(log_export) + 
  ggtitle("Log-transformed Time Plot: U.S. Trade in Goods with China - Exports from Jan 1985 to Feb 2018") +
  ylab("Millions of U.S. Dollars
       (Placed on Log Scale)")

multiplot(p3, p4, cols = 1)

#*Positive trend in both import and export
#*Clear seasonal patterns in import, but hard to tell if export has one

# Investigate transformations to get rid of trend
# Take the 1st difference of the data to remove the trend
Dlog_import <- diff(log_import)
Dlog_export <- diff(log_export)

# Time plot of differenced data
autoplot(Dlog_import) + 
  ggtitle("Log-transformed Time Plot: 
          Change in U.S. Trade in Goods with China - Imports from Jan 1985 to Feb 2018") +
  ylab("Millions of U.S. Dollars
       (Placed on Log Scale)")
autoplot(Dlog_export) + 
  ggtitle("Log-transformed Time Plot: 
          Change in U.S. Trade in Goods with China - Exports from Jan 1985 to Feb 2018") +
  ylab("Millions of U.S. Dollars
       (Placed on Log Scale)")

# Series appears trend-stationary, now to investigate seasonality
p5 <- ggseasonplot(Dlog_import, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Seasonal Plot: 
          Change in U.S. Trade in Goods with China - Imports from Jan 1985 to Feb 2018") +
  ylab("Millions of U.S. Dollars
       (Placed on Log Scale)")
p6 <- ggseasonplot(Dlog_export, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Seasonal Plot: 
          Change in U.S. Trade in Goods with China - Exports from Jan 1985 to Feb 2018") +
  ylab("Millions of U.S. Dollars
       (Placed on Log Scale)")
#*Seasonal pattern exists except for Mar, 1990s seem negative, but 2000s seem positive
#*No seasonal pattern in export

# Another seasonal plot - subseries plot
p7 <- ggsubseriesplot(Dlog_import) +
  ggtitle("Seasonal Plot: 
          Change in U.S. Trade in Goods with China - Imports from Jan 1985 to Feb 2018") +
  ylab("Millions of U.S. Dollars
       (Placed on Log Scale)")
p8 <- ggsubseriesplot(Dlog_export) +
  ggtitle("Seasonal Plot: 
          Change in U.S. Trade in Goods with China - Exports from Jan 1985 to Feb 2018") +
  ylab("Millions of U.S. Dollars
       (Placed on Log Scale)")
#*Basically no seasonal pattern in export, basically flat in change in different months
#*For import, strong seasonal pattern except for Feb and Mar, basically flat in change in different months

multiplot(p5, p7)
multiplot(p6, p8)
##########################################################
# Import has trend and seasonality
# Export only has trend
# To remove the trend, we take the first difference
# The 1st differenced series still has seasonality
#
# Forecast with various methods
##########################################################
# 1 Use a benchmark to forecast (seasonal naive method as the benchmark)
# y_t = y_{t-s} + e_t
snm_import <- snaive(Dlog_import) #Residual SD = 0.1029
print(summary(snm_import))
#*Residual SD shows how well the data is fitting, the smaller the better 
#*Residual SD: missing on average by roughly 0.1029 million$
checkresiduals(snm_import)
#*Residual plot shows the data looks totally random
#*ACF shows the residuals (left-over error terms), only a few autocorrelation over time, may be ideal

snm_export <- snaive(Dlog_export) #Residual SD = 0.2171
print(summary(snm_export))
#*Residual SD shows how well the data is fitting, the smaller the better 
#*Residual SD: missing on average by roughly 0.2171 million$
checkresiduals(snm_export) 
#*Residual plot shows the data looks totally random
#*ACF shows the residuals (left-over error terms), only a few autocorrelation over time, may be ideal

# 2 ETS method (exponential smoothing model)
ets_import <- ets(log_import) #Residual SD = sigma = 0.0748
print(summary(ets_import))
#*1st letter: error type
#*2nd letter: trend type
#*3rd letter: seasonal type
#*"N"=none, "A"=additive, "M"=multiplicative and "Z"=automatically
#AAA: additive exponential smoothing model with additive errors
checkresiduals(ets_import)
#*More autocorrelation over time, information in the data that model is not using efficiently
#*But Residual SD is smaller than snm, is the better model

ets_export <- ets(log_export) #Residual SD = sigma = 0.1443
print(summary(ets_export))
#*1st letter: error type
#*2nd letter: trend type
#*3rd letter: seasonal type
#*"N"=none, "A"=additive, "M"=multiplicative and "Z"=automatically
#AAA: additive exponential smoothing model with additive errors
checkresiduals(ets_export)
#*Alomost no autocorrelation over time
#*Residual SD is smaller than snm, is the better model


# 3 ARIMA
#*Needs to be stationary
#*Remove trend by using differenced data
#*Remove seasonality by telling the model there's seasonality
arima_import <- auto.arima(log_import, d = 1, D = 1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*d = 1: tell the function you're free to fit ARIMA take the 1st difference of the data
#*D = 1: get rid of the seasonality
#*stepwise = F: make our model try every combination of the model
#*approximation = F: make our model more accurate using the exact AIC instead of approximate AIC
#*trace = T: print out all the models
print(summary(arima_import))
#*ARIMA(2,1,0)(2,1,0)[12]
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(0.005118) = 0.0715402
checkresiduals(arima_import)
#*ACF looks better, only a few lags are outside the 95% CI

arima_export <- auto.arima(log_export, d = 1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*d = 1: tell the function you're free to fit ARIMA take the 1st difference of the data
#*stepwise = F: make our model try every combination of the model
#*approximation = F: make our model more accurate using the exact AIC instead of approximate AIC
#*trace = T: print out all the models
print(summary(arima_export))
#*ARIMA(2,1,1)(2,0,0)[12] with drift
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(0.02193) = 0.1480878
#*Greater than Residual SD of ets, ets should be the better model
checkresiduals(arima_export)
#*ACF also looks worse than ETS


# 4 Neural Network 
set.seed(25784)
nnar_import <- nnetar(log_import)

nnar_import
#*NNAR(1,1,2)[12]
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(0.007278) = 0.0853112
#*p = 1: last 1 observation is used as predictors
#*P = 1: Seasonality
#*k = 2: the number of hidden nodes (2 neurons in the hidden layer)
#*m = 12: monthly data

nnar_export <- nnetar(log_export)

nnar_export
#*NNAR(2,1,2)[12]
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(0.02143) = 0.1463899
#*p = 2: last 2 observation are used as predictors
#*P = 1: Seasonality
#*k = 2: the number of hidden nodes (2 neurons in the hidden layer)
#*m = 12: monthly data

##########################################################
# Forecast
##########################################################
# 1 Import
fcst_import_snm <- forecast(snm_import, h = 16)
fcst_import_snm$mean <- exp(diffinv(fcst_import_snm$mean)[2:17])*trade$import[398]
accuracy(fcst_import_snm$mean, import_trade[399:414])
#*SNM:
#*ME       RMSE     MAE       MPE       MAPE
#*-10119.14 11757.7 10119.14 -25.89406 25.89406

fcst_import_ets <- forecast(ets_import, h = 16)
fcst_import_ets$mean <- exp(fcst_import_ets$mean)
accuracy(fcst_import_ets$mean, import_trade[399:414])
#*ETS
#*ME       RMSE     MAE     MPE       MAPE
#*-5508.637 6643.835 5508.637 -14.2435 14.2435

fcst_import_arima <- forecast(arima_import, h = 16)
fcst_import_arima$mean <- exp(fcst_import_arima$mean)
accuracy(fcst_import_arima$mean, import_trade[399:414])
#*ARIMA
#*ME        RMSE      MAE       MPE       MAPE
#*-4631.9 5680.56  4631.9    -12.11285     12.11285

fcst_import_nnar <- forecast(nnar_import, PI = TRUE, h = 16)
fcst_import_nnar$mean <- exp(fcst_import_nnar$mean)
accuracy(fcst_import_nnar$mean, import_trade[399:414])
#*NNAR
#*ME       RMSE      MAE        MPE        MAPE
#*138.4959 4800.392 3871.927 -1.291097 9.681288

#*NNAR is the best model among these four models
fcst_import_nnar$upper <- exp(fcst_import_nnar$upper)
fcst_import_nnar$lower <- exp(fcst_import_nnar$lower)
fcst_import_nnar$x <- exp(fcst_import_nnar$x)
# Plot the actual and predicted value
autoplot(fcst_import_nnar) + 
  ggtitle("U.S. Trade in Goods with China - Imports (Actual v.s. Forecast)") +
  ylab("Millions of U.S. Dollars") +
  autolayer(import_trade)

# 2 Export
fcst_export_snm <- forecast(snm_export, h = 16)
fcst_export_snm$mean <- exp(diffinv(fcst_export_snm$mean)[2:17])*trade$export[398]
accuracy(fcst_export_snm$mean, export_trade[399:414])
#*SNM:
#*ME        RMSE     MAE       MPE       MAPE
#*-1109.181 2275.069 1830.028 -13.83847 20.15965

fcst_export_ets <- forecast(ets_export, h = 16)
fcst_export_ets$mean <- exp(fcst_export_ets$mean)
accuracy(fcst_export_ets$mean, export_trade[399:414])
#*ETS
#*ME        RMSE     MAE      MPE       MAPE
#*-2423.277 2916.995 2534.352 -27.71163 28.60869

fcst_export_arima <- forecast(arima_export, h = 16)
fcst_export_arima$mean <- exp(fcst_export_arima$mean)
accuracy(fcst_export_arima$mean, export_trade[399:414])
#*ARIMA
#*ME        RMSE      MAE       MPE       MAPE
#*-2317.748 2992.87 2581.074 -27.11882 29.25935

fcst_export_nnar <- forecast(nnar_export, PI = TRUE, h = 16)
fcst_export_nnar$mean <- exp(fcst_export_nnar$mean)
accuracy(fcst_export_nnar$mean, export_trade[399:414])
#*NNAR
#*ME        RMSE      MAE        MPE        MAPE
#*-1066.127 1958.823 1613.898 -13.51546 18.224

#*NNAR is the best model among these four models
fcst_export_nnar$upper <- exp(fcst_export_nnar$upper)
fcst_export_nnar$lower <- exp(fcst_export_nnar$lower)
fcst_export_nnar$x <- exp(fcst_export_nnar$x)
# Plot the actual and predicted value
autoplot(fcst_export_nnar) + 
  ggtitle("U.S. Trade in Goods with China - Exports (Actual v.s. Forecast)") +
  ylab("Millions of U.S. Dollars") +
  autolayer(export_trade)

##########################################################
##########################################################
# 2 Forecast of monthly import/export in the following 3 years [2019-21]
##########################################################
##########################################################
# Building NNAR
set.seed(25784)
nnar_import_trade <- nnetar(log_import_trade)

nnar_import_trade
#*NNAR(1,1,2)[12]
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(0.0072) = 0.08485281
#*p = 1: last 1 observation is used as predictors
#*P = 1: Seasonality
#*k = 2: the number of hidden nodes (2 neurons in the hidden layer)
#*m = 12: monthly data

nnar_export_trade <- nnetar(log_export_trade)

nnar_export_trade
#*NNAR(2,1,2)[12]
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(0.02151) = 0.1466629
#*p = 2: last 2 observation is used as predictors
#*P = 1: Seasonality
#*k = 2: the number of hidden nodes (2 neurons in the hidden layer)
#*m = 12: monthly data

# Forecasting
fcst_import_trade <- forecast(nnar_import_trade, PI = TRUE, h = 30)
fcst_import_trade$mean <- exp(fcst_import_trade$mean)
fcst_import_trade$upper <- exp(fcst_import_trade$upper)
fcst_import_trade$lower <- exp(fcst_import_trade$lower)
fcst_import_trade$x <- exp(fcst_import_trade$x)
autoplot(fcst_import_trade) + 
  ggtitle("U.S. Trade in Goods with China - Imports (Forecasted from Jul 2019 to Dec 2021))") +
  ylab("Millions of U.S. Dollars")

fcst_export_trade <- forecast(nnar_export_trade, PI = TRUE, h = 30)
fcst_export_trade$mean <- exp(fcst_export_trade$mean)
fcst_export_trade$upper <- exp(fcst_export_trade$upper)
fcst_export_trade$lower <- exp(fcst_export_trade$lower)
fcst_export_trade$x <- exp(fcst_export_trade$x)
autoplot(fcst_export_trade) + 
  ggtitle("U.S. Trade in Goods with China - Exports (Forecasted from Jul 2019 to Dec 2021)") +
  ylab("Millions of U.S. Dollars")

##########################################################
##########################################################
# 3 Forecast of GDP, CPI, unemployment rate in the following 3 years
##########################################################
##########################################################

# Load the data from Excel (3 datasets except for trade)
gdp_dataset <- read_xlsx("GDP.xlsx")
cpi_dataset <- read_xlsx("CPI.xlsx")
ur_dataset <- read_xlsx("Unemployment Rate.xlsx")

# Basic statistics
describe(gdp_dataset$USA)
describe(gdp_dataset$China)
describe(cpi_dataset$USA)
describe(cpi_dataset$China)
describe(ur_dataset$USA)
describe(ur_dataset$China)

# Combine the actual data [Jan 1985 - Jun 2019] and forecasted data [Jul 2019 - Dec 2021] 
fc_import <- ts(fcst_import_trade$mean, frequency = 12, start = c(2019,7))
fc_import_all <- ts(c(import_trade, fc_import), frequency = frequency(import_trade), start = start(import_trade))
fc_export <- ts(fcst_export_trade$mean, frequency = 12, start = c(2019,7))
fc_export_all <- ts(c(export_trade, fc_export), frequency = frequency(export_trade), start = start(export_trade))

# Change monthly time series import&export to yearly
import_yearly <- aggregate(fc_import_all, nfrequency = 1)
export_yearly <- aggregate(fc_export_all, nfrequency = 1)
trade_yearly <- cbind(import_yearly, export_yearly)

# Store the other variables as time-series objects
gdp_usa <- ts(gdp_dataset$USA, frequency = 1, start = 1985)
gdp_china <- ts(gdp_dataset$China, frequency = 1, start = 1985)
cpi_usa <- ts(cpi_dataset$USA, frequency = 1, start = 1985)
cpi_china <- ts(cpi_dataset$China, frequency = 1, start = 1985)
ur_usa <- ts(ur_dataset$USA, frequency = 1, start = 1985)
ur_china <- ts(ur_dataset$China, frequency = 1, start = 1985)

##########################################################
# Preliminary analysis
##########################################################
# Time plot of original data 
ggplot(gdp_dataset) +
  geom_line(aes(x = Year,y = USA, colour = "USA"), size = 1) +
  geom_line(aes(x = Year,y = China,colour ="China"),size=1) + 
  scale_colour_manual("", values = c("China" = "red","USA" = "blue")) +
  xlab("Year") + ylab("Millions of U.S. Dollars") + 
  ggtitle("Time Plot: GDP of China v.s. USA")
#*A drop of USA GDP in 2008-2009, should be due to Great Recession
#*Monotonic increasing of China GDP (the degree of increase constantly increases)

ggplot(cpi_dataset) +
  geom_line(aes(x = Year,y = USA, colour = "USA"), size = 1) +
  geom_line(aes(x = Year,y = China,colour ="China"),size=1) + 
  scale_colour_manual("", values = c("China" = "red","USA" = "blue")) +
  xlab("Year") + ylab("Annual Growth Rate %") + 
  ggtitle("Time Plot: CPI of China v.s. USA")
#*Analysis should be written in the dissertation
#*As time series goes on, large fluctuations appear

ggplot(ur_dataset) +
  geom_line(aes(x = Year,y = USA, colour = "USA"), size = 1) +
  geom_line(aes(x = Year,y = China,colour ="China"),size=1) + 
  scale_colour_manual("", values = c("China" = "red","USA" = "blue")) +
  xlab("Year") + ylab("% of Labour Force") + 
  ggtitle("Time Plot: Unemployment Rate of China v.s. USA")
#*Fluctuations appears in the U.S.
#*General pattern shows an increasing trend in China

##########################################################
# Both GDP have trend and no seasonality
# Both CPI have no trend and no seasonality
# USA unemployment rate has no trend and no seasonality
# China unemployment rate has trend and no seasonality
##########################################################
# Forecast using dynamic regression models
##########################################################
# 1 GDP
##ARIMA
arima_gdp_usa <- auto.arima(gdp_usa, d = 1, xreg = trade_yearly[1:34, ], stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*d = 1: take 1st difference
#*xreg: a numerical matrix of external regressors - import&export
#*stepwise = F: make our model try every combination of the model
#*approximation = F: make our model more accurate using the exact AIC instead of approximate AIC
#*trace = T: print out all the models
print(summary(arima_gdp_usa))
#*ARIMA(2,1,0) errors
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(9.58e+09) = 97877.47
checkresiduals(arima_gdp_usa)
#*ACF shows all lags are inside the 95% CI
accuracy(arima_gdp_usa)
#*ME       RMSE      MAE       MPE       MAPE      MASE        ACF1
#*1757.571 88821.65 71350.91 -0.1166889 0.7232579 0.1411369 0.01121269

arima_gdp_china <- auto.arima(gdp_china, d = 1, xreg = trade_yearly[1:34, ], stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*xreg: a numerical matrix of external regressors - import&export
#*stepwise = F: make our model try every combination of the model
#*approximation = F: make our model more accurate using the exact AIC instead of approximate AIC
#*trace = T: print out all the models
print(summary(arima_gdp_china))
#*ARIMA(1,1,1) errors
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(1.636e+10) = 127906.2
checkresiduals(arima_gdp_china)
#*ACF shows all lags are inside the 95% CI
accuracy(arima_gdp_china)
#*ME       RMSE      MAE       MPE       MAPE      MASE        ACF1
#*37948.5 116075.6 82223.14 0.07348095 1.70852 0.1102773 -0.08223658

##NNAR
set.seed(25784)
nnar_gdp_usa <- nnetar(gdp_usa, xreg = trade_yearly[1:34, ])

nnar_gdp_usa
#*NNAR(1,2)
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(3.762e+09) = 61335.14
#*p = 1: last 1 observation is used as predictors
#*k = 2: the number of hidden nodes (2 neurons in the hidden layer)
accuracy(nnar_gdp_usa)
#*ME       RMSE      MAE       MPE          MAPE      MASE        ACF1
#*388.5886 62321.53 45410.89 -0.002499813 0.490881 0.08982582 0.0004596059

nnar_gdp_china <- nnetar(gdp_china,  xreg = trade_yearly[1:34, ])

nnar_gdp_china
#*NNAR(1,2)
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(2.792e+09) = 52839.38
#*p = 1: last 1 observation are used as predictors
#*k = 2: the number of hidden nodes (2 neurons in the hidden layer)
accuracy(nnar_gdp_china)
#*ME       RMSE      MAE       MPE       MAPE      MASE        ACF1
#*2076.398 90525.71 57578.96 -0.01405055 1.059446 0.07722465 0.05336912

#*Both GDP in US and China using NNAR

# 2 CPI
##ARIMA
arima_cpi_usa <- auto.arima(cpi_usa, xreg = trade_yearly[1:34, ], stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*xreg: a numerical matrix of external regressors - import&export
#*stepwise = F: make our model try every combination of the model
#*approximation = F: make our model more accurate using the exact AIC instead of approximate AIC
#*trace = T: print out all the models
print(summary(arima_cpi_usa))
#*ARIMA(0,0,1) errors
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(1.031) = 1.015382
checkresiduals(arima_cpi_usa)
#*ACF shows all lags are inside the 95% CI
accuracy(arima_cpi_usa)
#*ME         RMSE       MAE       MPE       MAPE      MASE        ACF1
#*-0.00676133 0.9539593 0.6913773 -14.91935 88.32274 0.7611922 -0.02273644

arima_cpi_china <- auto.arima(cpi_china, xreg = trade_yearly[1:34, ], stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*xreg: a numerical matrix of external regressors - import&export
#*stepwise = F: make our model try every combination of the model
#*approximation = F: make our model more accurate using the exact AIC instead of approximate AIC
#*trace = T: print out all the models
print(summary(arima_cpi_china))
#*ARIMA(0,0,1) errors
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(16.24) = 4.029888
checkresiduals(arima_cpi_china)
#*ACF shows all lags are inside the 95% CI
accuracy(arima_cpi_china)
#*ME       RMSE      MAE       MPE       MAPE      MASE        ACF1
#*-0.01429449 3.785105 2.801339 25.57312 140.3367 0.8358424 0.2022386

##NNAR
set.seed(25784)
nnar_cpi_usa <- nnetar(cpi_usa, xreg = trade_yearly[1:34, ])
nnar_cpi_usa
#*NNAR(1,2)
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(0.4665) = 0.6830081
#*p = 1: last 1 observation is used as predictors
#*k = 2: the number of hidden nodes (2 neurons in the hidden layer)
accuracy(nnar_cpi_usa)
#*ME        RMSE      MAE       MPE          MAPE      MASE        ACF1
#*0.001236831 0.6829751 0.5172169 -19.75173 66.38162 0.5694453 -0.05088162

nnar_cpi_china <- nnetar(cpi_china, xreg = trade_yearly[1:34, ])
nnar_cpi_china
#*NNAR(5,4)
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(0.06626) = 0.2574102
#*p = 5: last 5 observation are used as predictors
#*k = 4: the number of hidden nodes (4 neurons in the hidden layer)
accuracy(nnar_cpi_china)
#*ME            RMSE      MAE       MPE       MAPE      MASE        ACF1
#*-0.0007777047 0.2574062 0.1751069 5.382673 10.78762 0.05224709    -0.2453657

#*Both CPI in US and China using NNAR

# 3 Unemployment Rate
##ARIMA
arima_ur_usa <- auto.arima(ur_usa, xreg = trade_yearly[1:34, ], stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*xreg: a numerical matrix of external regressors - import&export
#*stepwise = F: make our model try every combination of the model
#*approximation = F: make our model more accurate using the exact AIC instead of approximate AIC
#*trace = T: print out all the models
print(summary(arima_ur_usa))
#*ARIMA(2,0,1) errors
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(0.2862) = 0.5349766
checkresiduals(arima_ur_usa)
#*ACF shows all lags are inside the 95% CI
accuracy(arima_ur_usa)
#*ME         RMSE       MAE       MPE       MAPE      MASE        ACF1
#*0.04029842 0.4854454 0.3490715 0.8105623 5.892881 0.5290177 0.05104611

arima_ur_china <- auto.arima(ur_china, d = 1, xreg = trade_yearly[1:34, ], stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*xreg: a numerical matrix of external regressors - import&export
#*stepwise = F: make our model try every combination of the model
#*approximation = F: make our model more accurate using the exact AIC instead of approximate AIC
#*trace = T: print out all the models
print(summary(arima_ur_china))
#*ARIMA(0,1,0) errors
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(0.03418) = 0.1848783
checkresiduals(arima_ur_china)
#*ACF shows all lags are inside the 95% CI
accuracy(arima_ur_china)
#*ME         RMSE      MAE       MPE       MAPE      MASE        ACF1
#*5.018642e-05 0.1736537 0.1271638 -0.0693511 4.074366 0.999144 0.1830708

##NNAR
set.seed(25784)
nnar_ur_usa <- nnetar(ur_usa, xreg = trade_yearly[1:34, ])

nnar_ur_usa
#*NNAR(2,2)
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(0.08036) =  0.2834784
#*p = 2: last 1 observation is used as predictors
#*k = 2: the number of hidden nodes (2 neurons in the hidden layer)
accuracy(nnar_ur_usa)
#*ME         RMSE      MAE       MPE          MAPE      MASE        ACF1
#*0.01227054 0.2865812 0.2224113 -0.1700383 4.050789 0.3370643 0.2956436

nnar_ur_china <- nnetar(ur_china, xreg = trade_yearly[1:34, ])

nnar_ur_china
#*NNAR(1,2)
#*Residual SD = sqrt(sigma^2 estimated) = sqrt(0.01163) = 0.1078425
#*p = 1: last 1 observation are used as predictors
#*k = 2: the number of hidden nodes (1 neurons in the hidden layer)
accuracy(nnar_ur_china)
#*ME       RMSE      MAE       MPE       MAPE      MASE        ACF1
#*-0.0001851183 0.1078269 0.08266885 -0.1901174 2.83701 0.649541 -0.0491004

#*Both Unemployment Rate in US and China using NNAR
##########################################################
# Forecast using time series models (without impacts of trade war)
##########################################################
# 1 GDP
arima_gdp_usa_n <- auto.arima(gdp_usa, d = 1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*ARIMA(1,1,0) with drift
print(summary(arima_gdp_usa_n))
#*Residual SD = sqrt(4.238e+10)
arima_gdp_china_n <- auto.arima(gdp_china, d = 1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*ARIMA(2,1,2)
print(summary(arima_gdp_china_n))
#*Residual SD = sqrt(1.665e+10)
accuracy(arima_gdp_usa_n)
#*ME       RMSE       MAE       MPE    MAPE      MASE       ACF1
#*5511.667 196565.1 137178.5 -0.2429258 1.23884 0.2713484 0.08572064
accuracy(arima_gdp_china_n)
#*ME         RMSE      MAE      MPE     MAPE     MASE        ACF1
#*33951.02 117082.9 81287.5 -0.09074421 1.730513 0.1090224 -0.07803587

set.seed(25784)
nnar_gdp_usa_n <- nnetar(gdp_usa)
nnar_gdp_usa_n
#*NNAR(1,1)
#*Residual SD = sqrt(4.815e+10)
nnar_gdp_china_n <- nnetar(gdp_china)
nnar_gdp_china_n
#*NNAR(1,1)
#*Residual SD = sqrt(3.028e+10)
accuracy(nnar_gdp_usa_n)
#*ME         RMSE       MAE        MPE     MAPE      MASE      ACF1
#*-599.8503 219426.4 144159 -0.1119949 1.297575 0.2851563 0.4187752
accuracy(nnar_gdp_china_n)
#*ME          RMSE     MAE        MPE     MAPE      MASE      ACF1
#*-1257.184 174000.6 119349.3 -1.371319 2.700539 0.1600708 0.5401614

#*Best model should be ARIMA when forecasting GDP in both countries.

fcst_gdp_usa_n <- forecast(arima_gdp_usa_n, h = 3)
fcst_gdp_china_n <- forecast(arima_gdp_china_n, h = 3)

# 2 CPI
arima_cpi_usa_n <- auto.arima(cpi_usa, stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*ARIMA(0,1,2)
print(summary(arima_cpi_usa_n))
#*Residual SD = sqrt(1.123)
arima_cpi_china_n <- auto.arima(cpi_china, stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*ARIMA(0,1,2)
print(summary(arima_cpi_china_n))
#*Residual SD = sqrt(16.37)
accuracy(arima_cpi_usa_n)
#*ME           RMSE       MAE       MPE    MAPE      MASE       ACF1
#*-0.193704 1.012062 0.7433498 -21.56326 100.4545 0.818413 -0.07482249
accuracy(arima_cpi_china_n)
#*ME          RMSE       MAE      MPE     MAPE     MASE        ACF1
#*-0.5114925 3.863039 2.861922 25.83405 135.8875 0.8539189 0.1444656

set.seed(25784)
nnar_cpi_usa_n <- nnetar(cpi_usa)
nnar_cpi_usa_n
#*NNAR(1,1)
#*Residual SD = sqrt(1.001)
nnar_cpi_china_n <- nnetar(cpi_china)
nnar_cpi_china_n
#*NNAR(5,3)
#*Residual SD = sqrt(0.7689)
accuracy(nnar_cpi_usa_n)
#*ME            RMSE       MAE        MPE     MAPE      MASE      ACF1
#*0.001083309 1.000271 0.7233561 -22.51689 101.6376 0.7964003 0.05681852
accuracy(nnar_cpi_china_n)
#*ME           RMSE       MAE        MPE     MAPE      MASE      ACF1
#*0.003091029 0.876869 0.64863 13.75952 41.13341 0.1935334 -0.06113646

#*Best model should be NNAR when forecasting CPI in both countries.

fcst_cpi_usa_n <- forecast(nnar_cpi_usa_n, PI = TRUE, h = 3)
fcst_cpi_china_n <- forecast(nnar_cpi_china_n, PI = TRUE, h = 3)

# 3 Unemployment Rate
arima_ur_usa_n <- auto.arima(ur_usa, stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*ARIMA(2,0,0)
print(summary(arima_ur_usa_n))
#*Residual SD = sqrt(0.4382)
arima_ur_china_n <- auto.arima(ur_china, d = 1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
#*ARIMA(0,1,1)
print(summary(arima_ur_china_n))
#*Residual SD = sqrt(0.03522)
accuracy(arima_ur_usa_n)
#*ME           RMSE       MAE       MPE    MAPE      MASE       ACF1
#*-0.006916699 0.63207 0.4333709 -1.284165 7.11417 0.6567733 0.09630501
accuracy(arima_ur_china_n)
#*ME          RMSE       MAE      MPE     MAPE     MASE        ACF1
#*0.04292576 0.182061 0.1208995 1.469917 3.877175 0.949925 -0.07350245

set.seed(25784)
nnar_ur_usa_n <- nnetar(ur_usa)
nnar_ur_usa_n
#*NNAR(2,2)
#*Residual SD = sqrt(0.273)
nnar_ur_china_n <- nnetar(ur_china)
nnar_ur_china_n
#*NNAR(1,1)
#*Residual SD = sqrt(0.02568)
accuracy(nnar_ur_usa_n)
#*ME            RMSE       MAE        MPE     MAPE      MASE      ACF1
#*0.0008549603 0.5224743 0.3699153 -0.6922858 6.279768 0.5606064 0.3377347
accuracy(nnar_ur_china_n)
#*ME           RMSE       MAE        MPE     MAPE      MASE      ACF1
#*4.920407e-07 0.1602561 0.125162 -0.3232209 4.148301 0.983416 0.201808

#*Best model should be NNAR when forecasting Unemployment Rate in USA, ARIMA in China.

fcst_ur_usa_n <- forecast(nnar_ur_usa_n, PI = TRUE, h = 3)
fcst_ur_china_n <- forecast(arima_ur_china_n, h = 3)

##########################################################
# Forecast 2019 - 2021
##########################################################
fcst_gdp_usa <- forecast(nnar_gdp_usa, PI = TRUE, xreg = trade_yearly[35:37, ], h = 3)
autoplot(fcst_gdp_usa) + 
  ggtitle("GDP - USA (Forecasted from 2019 to 2021)") +
  ylab("Millions of U.S. Dollars")+
  autolayer(fcst_gdp_usa_n$mean)

fcst_gdp_china <- forecast(nnar_gdp_china, PI = TRUE, xreg = trade_yearly[35:37, ], h = 3)
autoplot(fcst_gdp_china) + 
  ggtitle("GDP - China (Forecasted from 2019 to 2021)") +
  ylab("Millions of U.S. Dollars")+
  autolayer(fcst_gdp_china_n$mean)

fcst_cpi_usa <- forecast(nnar_cpi_usa, PI = TRUE, xreg = trade_yearly[35:37, ], h = 3)
autoplot(fcst_cpi_usa) + 
  ggtitle("CPI - USA (Forecasted from 2019 to 2021)") +
  ylab("Annual Growth Rate %")+
  autolayer(fcst_cpi_usa_n$mean)

fcst_cpi_china <- forecast(nnar_cpi_china, PI = TRUE, xreg = trade_yearly[35:37, ], h = 3)
autoplot(fcst_cpi_china) + 
  ggtitle("CPI - China (Forecasted from 2019 to 2021)") +
  ylab("Annual Growth Rate %")+
  autolayer(fcst_cpi_china_n$mean)

fcst_ur_usa <- forecast(nnar_ur_usa, PI = TRUE, xreg = trade_yearly[35:37, ], h = 3)
autoplot(fcst_ur_usa) + 
  ggtitle("Unemployment Rate - USA (Forecasted from 2019 to 2021)") +
  ylab("% of labour force")+
  autolayer(fcst_ur_usa_n$mean)

fcst_ur_china <- forecast(nnar_ur_china, PI = TRUE, xreg = trade_yearly[35:37, ], h = 3)
autoplot(fcst_ur_china) + 
  ggtitle("Unemployment Rate - China (Forecasted from 2019 to 2021)") +
  ylab("% of labour force") +
  autolayer(fcst_ur_china_n$mean)
