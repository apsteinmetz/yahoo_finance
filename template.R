# download stock data from yahoo finance
library(tidyverse)
library(quantmod)
getSymbols("", src = "yahoo", from = "2010-01-01", to = "2019-12-31")
# calculate daily returns
daily_return = diff(log(Cl(AAPL)))
# calculate annualized volatility
volatility = sd(daily_return,na.rm = TRUE) * sqrt(252)
print(volatility)
 

rgti <- read_csv("rgti.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y"),
                                                Time = col_time(format = "%I:%M %p"))) |> 
        drop_na() |> 
        select(Date,Time,Close) |> 
        rename(RGTI = Close)

rgtiw <- read_csv("rgtiw.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y"),
                                                Time = col_time(format = "%I:%M %p"))) |> 
        drop_na() |>
        select(Date,Time,Close) |>
        rename(RGTIW = Close)

rgti270115c12 <- read_csv("rgti270115c12.csv", 
                          col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                           Time = col_time(format = "%I:%M %p"))) |> 
        drop_na() |>
        select(Date,Time,Close) |>
        rename(RGTI_Call = Close)


RGTI_all <- left_join(rgti270115c12,rgtiw,by = c("Date","Time")) |> 
        left_join(rgti,by = c("Date","Time")) |> 
        mutate(Spread = RGTI_Call - RGTIW)

# scatter plot RGTI vs Spread
ggplot(RGTI_all, aes(x = RGTI, y = Spread)) + geom_point() + geom_smooth(method = "lm") +
        # show the correlation coefficient
        geom_text(aes(x = 10, y = 0.5, label = paste("Correlation = ", round(cor(RGTI, Spread), 2))), 
                  color = "red", size = 5) +
        # show regression equation
        geom_text(aes(x = 10, y = 1, label = paste("Spread = ", round(coef(lm(Spread ~ RGTI))[1], 2), " + ", 
                                                      round(coef(lm(Spread ~ RGTI))[2], 2), "RGTI")), 
                  color = "blue", size = 5) +
        labs(title = "RGTI vs Spread 15-minute", x = "RGTI", y = "Spread")

# scatter plot RGTIW vs RGTI_Call and RGTIW
RGTI_all |> ggplot(aes(x = RGTI)) + 
        geom_point(aes(y = RGTI_Call),color = "red") + 
        geom_point(aes(y = RGTIW),color = "blue") +
        annotate("text", x = 10, y = 10, label = "RGTI_Call", color = "red") +
        annotate("text", x = 10, y = 1, label = "RGTIW", color = "blue")
