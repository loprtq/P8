### Preliminaries -------------------------------------------------------------
library(httr)
library(rvest)
library(dplyr)
library(lubridate)
library(magrittr)
library(patchwork)
library(tidyr)
library(quantmod)
library(stringr)
library(tidyquant)
library(RQuantLib)

## APIkeys for getting data
APIKEYS = c("RvavzpwdcKvlctKSK4NlQq_hDE1Jt62n", "m41jRNmoQTr_FwKWIc4C3Npjk8y3ml_A",
            "1BE6HtpyXAO7VBklwdyyxERyM2dyHrX5", "ph8GVvz0Rqrb3m9j_LkYEz6yTd0sEtVJ", 
            "KNxmUaH5plSGYAE7B75VonAyWNenU2ir", "FyK7kEW8AhRaajsgQG0RzzVYKUXYXU4Q", 
            "wbo1PJx1EwSgLPA2CF1MG_Z3HRT6MYDh", "AeCLOk8i1H8xiQFQ29vPbGiecMcG0HU7", 
            "Qtw3bn_ZAR6FRVWxEzL5id5rQTYyJlMD", "RMnOTMQYLQpxQpCJTCeO2l1cZ8KtySHj", 
            "DKT0T63PrinHG_UBuD_heSo9sSLCd2xg", "qg2kqgyANLrQ0MHqvkSIyItZWm6fNRPZ", 
            "ywkgolNpXmCCpGkmLPS79F5er7qcHV7F", "Vh6ZIpLnMhjtIiaWHLn76d_VzE_CxUgc", 
            "dmcp303MLS_0e7NKFrzhToH5xDK4M6hI", "KzgZNiZH3lptITrp7kaWqs7REKlMDzid",
            "wDo9nnpy9BxYI7xibursrIvFkbaXCHE9", "jTY7PiMMbjRYd9O7AQ2kKcmHNVLg8x9M",
            "TMYAy708kDMOUy7hXLDp4nMNyWFAVBwb", "WJ9gg966EpXjYhJCLUpxVV7LIQc0IYig",
            "cxceRuPmf05A7KX6ThWbpXspRYqbOmcv", "Zucc3R7UPGo1YSBTbDGAhz1EFSTZxaeG",
            "O32mJWZ7OyD8LmmGPaSqDCpP2zJUWzbA", "7U8rFdomTWxkxiE__wmTGS9_zBJPuw73",
            "2STbpujQL1nQbvSFMj3bO9kJ5GIF70Cn", "MwfIwGUZJ2KeWC_BdALwwN9FYUgmp5Be",
            "wXoLMBPd4lfNXOVVXn24pLkFMtyBqb7A", "3wk3OL_3naR4ihs8RwNTJlIKNIPpd3kU",
            "iVFf9fCiGOSbKKRB9KFtszWGSca0jLRd")


#Parameters for API
# Change ticker for other stocks than GOOG
ticker <- "GOOG" 
multiplier <- "1"
interval <- "minute"
from_date <- "2022-10-17"
to_date <- "2025-10-21"   

## Getting possible maturity times (only dates after todays date is possible)
# GOOG 2023-03-17
dates <-
  c(
    "2023-02-17",
    "2023-02-24",
    "2023-03-03",
    "2023-03-10",
    "2023-03-17",
    "2023-03-24",
    "2023-04-21",
    "2023-06-16",
    "2023-07-21",
    "2023-09-15",
    "2024-01-19",
    "2024-06-21",
    "2025-01-17",
    "2025-06-20"
  )

# Other stocks 2023-05-03
dates <-
  c(
    "2023-05-05",
    "2023-05-12",
    "2023-05-19",
    "2023-05-26", 
    "2023-06-02",
    "2023-06-09"
  )

## Getting possible strikeprices
strikes = data.frame()
for (date in dates) {
  strikes2 = data.frame(date, format(round(getOptionChain(ticker, Exp=date)$calls$Strike, 2), nsmall=2))
  strikes = rbind.data.frame(strikes, strikes2)
}

## Function for constructing contract names 
contract <- function(navn, dam){
  numbers = str_pad(gsub(".","", gsub(" ", "", dam[[2]]), fixed = TRUE), 7,side = "left", pad = "0")
  contract_name = paste0(navn, format(as.Date(dam[[1]]), "%y%m%d") , "C", numbers, "0")
  return(contract_name)
}

contract(ticker, strikes)

### Get data ----------- takes a long time -------------------------------------
for (KEY in APIKEYS) {
  option_api <- paste0(
    "https://api.polygon.io/v2/aggs/ticker/",
    "O:",contract(ticker, strikes), "/range/", multiplier,
    "/", interval, "/", from_date, "/", to_date,
    "?adjusted=true&sort=asc&limit=50000&",
    "apiKey=", KEY
  )
}

ddf1 = list()
for (opt in option_api[1:length(option_api)]) {
  ddf = jsonlite::fromJSON(opt)
  ddf1 = rbind(ddf1, ddf)
  Sys.sleep(12)
}

dud = ddf1
dud1 = dud[-which(dud[,3] == 0),]
hej3 = strikes[1:length(hej),]
hej4 = hej3[-which(dud[,3] == 0),]

dam = list()
damdam = list()
for (i in 1:length(dud1[,1])) {
  option <- dud1[i,]$results %>%  
    dplyr::mutate(
      Start = lubridate::as_datetime(t/1000)
    ) %>% 
    dplyr::select(
      Start,
      "OptionPrice" = c
    )
  dam = rbind(dam, option)
  damdam = rbind(damdam, length(option[,1]))
}

# Reduce hej3 to strikes[1:length(damdam),] if hej3 is longer than damdam
# and run hej4 again too

samlet = cbind(rep(hej4[,1], damdam[,1]), rep(hej4[,2], damdam[,1]), dam)
samlet = cbind(samlet, difftime(samlet[,1],samlet[,3], units="days")/365.25)
colnames(samlet) = c("Maturity Time","Strike Price","Time","Option Price","Time to Maturity") 


# Get stock prices - Remember to change ticker in fromJSON from GOOG if getting
# stock prices for other stocks
ddf = jsonlite::fromJSON("https://api.polygon.io/v2/aggs/ticker/GOOG/range/1/minute/2023-03-02/2023-05-03?adjusted=true&limit=50000&apiKey=wjsioHaxBhv8bN2UQ3sbspeS6fbFJHfs")
stock <- ddf$results %>% 
  dplyr::mutate(
    Start = lubridate::as_datetime(t/1000)
  ) %>%
  dplyr::select(
    Start,
    "StockPrice" = c
  )
ddf = jsonlite::fromJSON("https://api.polygon.io/v2/aggs/ticker/GOOG/range/1/minute/2022-12-15/2023-03-02?adjusted=true&limit=50000&apiKey=wjsioHaxBhv8bN2UQ3sbspeS6fbFJHfs")
stock2 <- ddf$results %>% 
  dplyr::mutate(
    Start = lubridate::as_datetime(t/1000)
  ) %>%
  dplyr::select(
    Start,
    "StockPrice" = c
  )
ddf = jsonlite::fromJSON("https://api.polygon.io/v2/aggs/ticker/GOOG/range/1/minute/2022-10-15/2022-12-15?adjusted=true&limit=50000&apiKey=wjsioHaxBhv8bN2UQ3sbspeS6fbFJHfs")
stock4 <- ddf$results %>% 
  dplyr::mutate(
    Start = lubridate::as_datetime(t/1000)
  ) %>%
  dplyr::select(
    Start,
    "StockPrice" = c
  )
stock3 = rbind(stock, stock2, stock4)
stocks = list() 

for (i in 1:length(samlet[,1])) {
  stocks = rbind(stocks, stock3[which(stock3$Start == samlet$Time[i]),2])  
}

helt_samlet = cbind(samlet, stocks)
colnames(helt_samlet) = c("Maturity Time","Strike Price","Time","Option Price","Time to Maturity", "Stock Price") 

data <- helt_samlet

# Black Scholes formula for call options
BSMOption<- function(S, T, K, r, sigma){
  d1 <- (log(S / K) + (r + sigma ^ 2 / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  return(S * pnorm(d1, mean = 0, sd = 1) - K * exp(-r * T) * pnorm(d2, mean =
                                                                     0, sd = 1))
}

# Minimize difference between real and BS price by finding implied volatility
implied_vol <- function(i,s){
  data[i,4] - BSMOption(S = as.numeric(data[i,6]), T = as.numeric(data[i,5]),
                        K = as.numeric(data[i,2]),r = 0.0386, sigma = s)
}
imp = list()
for (j in 1:length(data[,1])) {
  print(j)
  xmin = uniroot(implied_vol, interval = c(-1000, 1000), tol = .Machine$double.eps^0.25, i = j)$root
  imp = rbind(imp, xmin)
}
mmm = cbind(data, simplify2array(imp))
colnames(mmm) = c("Maturity Time","Strike Price","Time","Option Price","Time to Maturity", "Stock Price", "Implied Volatility") 

# Doing the same thing just using function from R and checking difference in results
# This does not work for all stocks 
nydata = mmm
implied2 = list()
for (j in 1:length(nydata[,1])) {
  print(j)
  imp2 = EuropeanOptionImpliedVolatility(value = as.numeric(nydata[j,4]), underlying = as.numeric(nydata[j,6]), 
                                         strike = as.numeric(nydata[j,2]), maturity = as.numeric(nydata[j,5]),
                                         riskFreeRate = 0.0386, type = "call", volatility = 0.3, dividendYield = 0)
  implied2 = rbind(implied2, imp2)
}

nydata_mimp2 = cbind(nydata, implied2, as.numeric(nydata$`Implied Volatility`) - as.numeric(implied2))
max(abs(as.numeric(nydata$`Implied Volatility`) - as.numeric(implied2)))
colnames(nydata_mimp2) = c("Maturity Time","Strike Price","Time","Option Price","Time to Maturity", "Stock Price", "Implied Volatility BS", "Implied Volatility fkt", "Diff in Implied Volatility") 

# save nydata_mimp2 as what you read in in NN document




