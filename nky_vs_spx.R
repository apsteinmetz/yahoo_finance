library(tidyverse)
# download stock data from yahoo finance
library(quantmod)
getSymbols(c("EWJ","HEWJ","SPY"),src = "yahoo")

# turn closing prices into a single tibble
prices = cbind(Cl(EWJ),Cl(HEWJ),Cl(SPY)) %>% 
   as_tibble(rownames = "date") |> 
   mutate(date = as.Date(date)) |> 
   pivot_longer(cols = -1, names_to = "ticker", values_to = "price") |> 
   mutate(ticker = str_remove(ticker,"\\.Close")) |> 
   group_by(ticker) |>
   mutate(daily_return = price/lag(price)-1)

prices


# create index of value
start_date = as.Date("2024-01-01")
#start_date = Sys.Date()-365
index = prices %>% 
   # replace NA in daily return with 0
   mutate(daily_return = ifelse(is.na(daily_return),0,daily_return)) |>
   arrange(ticker,date) |>
   filter(date >= start_date) |> 
   group_by(ticker) |> 
   mutate(value = 100*cumprod(1+daily_return))

index |> 
   ggplot(aes(x = date, y = value, color = ticker)) + 
   # add line for each ticker with thicker line
   geom_line(size = 1) +
   # annotate last point with value
   geom_text(data = filter(index,date == max(date)), 
             aes(label = round(value,2)), hjust = 1.2, vjust = 0) +
   labs(title = "YTD Performance of SPY,\nEWJ (Japan ETF) and HEWJ (Hedged Japan ETF)",
        x = "Date",
        y = "Index Value") +
   theme_minimal()

