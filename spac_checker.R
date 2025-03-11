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
ticker_sheet <- read_sheet(spac_tickers) |> 
   mutate(warrant_ex_date = as.Date(warrant_ex_date)) |> 
   # factors where there are a few categories
   # add levels for possible missing categories we might use in the future
   mutate(type = factor(type,levels = c("common","warrant"))) |> 
   mutate(status = factor(status,levels = c("announced","merger_approved","completed","searching")))
             

# get stock/warrant prices for all available dates
# Current src methods available are: yahoo, MySQL, FRED, csv, RData, oanda, and av.
tickers <- ticker_sheet$ticker
tickers


# download prices and combine into a list
prices_raw <- ticker_sheet$ticker |> 
   map(~getSymbols(.x, src = "yahoo", auto.assign = FALSE)) |> 
   map(as_tibble, rownames = "date") |> 
   map(select, date, contains("Adjusted"))

# convert the list of ticker prices to a single tidy data frame
prices <- prices_raw |> 
   map(~rename_with(.x, ~str_remove(.x, ".Adjusted"))) |> 
   map2(tickers, ~mutate(.x, ticker = .y)) |>
   # convert date to date
   map(~mutate(.x, date = as.Date(date))) |> 
   reduce(full_join, by = c("date", "ticker")) |> 
   pivot_longer(cols = -c(date, ticker), names_to = "type", values_to = "price") |> 
   select(date, ticker,price) |> 
   drop_na() |> 
   # now add the metadata from our spreadsheet
   left_join(ticker_sheet) |> 
   # get daycount around ex-dates
   mutate(.by=ticker,daycount = as.numeric(date - as.Date(warrant_ex_date)))
   



# quick explore with a plot
prices |> 
ggplot(aes(x = date, y = price, color = ticker)) +
  geom_line() +
  labs(title = "SPAC Warrant Prices",
       x = "Date",
       y = "Price") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  facet_wrap(~ticker, scales = "free_y")

# now look at daycounts
prices |> 
   filter(type == "warrant") |>
   ggplot(aes(x = daycount, y = price, color = ticker)) +
   geom_line() +
   labs(title = "SPAC Warrant Prices",
        x = "Days from Ex-Date",
        y = "Price") +
   theme_minimal() +
   theme(legend.position = "none") +
   scale_y_continuous(labels = scales::dollar) +
   facet_wrap(~ticker, scales = "free_y")
