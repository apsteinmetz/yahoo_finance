# download stock data from yahoo finance
library(quantmod)
getSymbols("AAPL", src = "yahoo", from = "2010-01-01", to = "2019-12-31")
# calculate daily returns
daily_return = diff(log(Cl(AAPL)))
# calculate annualized volatility
volatility = sd(daily_return,na.rm = TRUE) * sqrt(252)
print(volatility)
 
 The code above downloads the stock data of Apple Inc. (AAPL) from Yahoo Finance from 2010 to 2019 and calculates the annualized volatility of the stock. 
 The  getSymbols()  function from the  quantmod  package is used to download the stock data. The  src  argument specifies the source of the data (in this case, Yahoo Finance), and the  from  and  to  arguments specify the time period for which the data should be downloaded. 
 The daily returns are calculated by taking the difference of the log of the closing prices of the stock. The annualized volatility is then calculated as the standard deviation of the daily returns multiplied by the square root of 252 (the number of trading days in a year). 
 The result is printed to the console. 
 