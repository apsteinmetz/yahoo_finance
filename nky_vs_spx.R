# analyze spac warrants
# load library or install package if library not present
if (!require("tidyverse")) install.packages("tidyverse")
# stock retrieval
if (!require("quantmod")) install.packages("quantmod")
if (!require("googlesheets4")) install.packages("googlesheets4")

# load spreadsheets from google sheets -----------------------------------------
spac_tickers <- "https://docs.google.com/spreadsheets/d/1gXte0S4lFURNwDtz6FGUxIeIz2bUP3RZmYKh33-N7N8/edit?usp=sharing"
# set to deauth since we don't need a password to get this
gs4_deauth()
ticker_sheet <- read_sheet(spac_tickers)

# get stock/warrant prices for all available dates
# Current src methods available are: yahoo, MySQL, FRED, csv, RData, oanda, and av.
tickers <- ticker_sheet$ticker
tickers


fix_column_names <- 

prices <- ticker_sheet$ticker |> 
   map(~getSymbols(.x, src = "yahoo", auto.assign = FALSE))

prices |> 
   map(as_tibble, rownames = "date") |> 
   map(select, date, contains("Adjusted")) |> 
   map(~rename_with(., ~str_remove(., "\\.[A-Za-z]+\\.Adjusted"))) |> 
   map2(tickers, ~mutate(.x, ticker = .y))


# Here's the corrected and simplified version that should work:

#```r
prices <- ticker_sheet$ticker |> 
  map(~getSymbols(.x, src = "yahoo", auto.assign = FALSE)) |> 
  map(as_tibble, rownames = "date") |> 
  map(select, date, contains("Adjusted")) |> 
  map(~rename_with(., ~str_remove(., "\\.[A-Za-z]+\\.Adjusted"))) |> 
  map2(tickers, ~mutate(.x, ticker = .y)) |> 
  reduce(bind_rows) |> 
  mutate(date = as.Date(date))

ggplot(prices, aes(x = date, y = Adjusted, color = ticker)) +
  geom_line() +
  labs(title = "SPAC Warrant Prices",
       x = "Date",
       y = "Price") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  facet_wrap(~ticker, scales = "free_y")
#```








prices <- ticker_sheet$ticker |> 
  map(~getSymbols(.x, src = "yahoo", auto.assign = FALSE))



temp <- prices |> 
   map(as_tibble,rownames = "date") |> 
   map(select, date, contains("Adjusted"))
   map(select, date, contains("Adjusted")) |> 
   map(mutate,date = as.Date(date)) |> 
   map(\(x) rename_with(x,str_remove(x, "\\.[A-Za-z]+\\.Adjusted"), contains("Adjusted")))


  map(~rename(., close = close)) |> 
  map(~mutate(ticker = .x)) |> 
  reduce(bind_rows)


|> 
  ggplot(aes(x = date, y = close, color = ticker)) +
  geom_line() +
  labs(title = "SPAC Warrant Prices",
       x = "Date",
       y = "Price") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  facet_wrap(~ticker, scales = "free_y")


