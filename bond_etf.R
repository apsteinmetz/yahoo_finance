# fixed income etf hedging
library(tidyverse)
library(quantmod)
library(tidymodels)


# download prices and combine into a list. Keep only the adjusted price.
RELOAD = TRUE
if (RELOAD) {
   tickers = c('SHY', 'IEF', 'TLT')
   prices_raw <- tickers |> 
   map(~getSymbols(.x, src = "yahoo", auto.assign = FALSE)) |> 
   map(as_tibble, rownames = "date") |> 
   map(select, date, contains("Adjusted"))

   prices_shy <- prices_raw |> 
      map(~rename_with(.x, ~str_remove(.x, ".Adjusted"))) |> 
      # use column names to make a ticker column
      map2(tickers, ~mutate(.x, ticker = .y)) |>
      # convert date string to date type
      map(~mutate(.x, date = as.Date(date))) |> 
      # combine the list of data frames into one
      reduce(full_join, by = c("date", "ticker")) |> 
      pivot_longer(cols = -c(date, ticker), names_to = "type", values_to = "price") |> 
      select(date, ticker,price) |> 
      arrange(date) |> 
      drop_na()
   
   save(prices_shy, file = "data/prices_shy.RData")
fred_codes <- "T10Y2Y"
fred_raw <- fred_codes |> 
   map(~getSymbols(.x, src = "FRED", auto.assign = FALSE)) |> 
   map(as_tibble, rownames = "date")

# convert the list of ticker prices to a single tidy data frame
# the map functions lets us step through the list of data frames
# and apply the same operations to each one
values_fred <- fred_raw |> 
#   map(~rename_with(.x, ~str_remove(.x, ".Adjusted"))) |> 
#   # use column names to make a ticker column
   map2(fred_codes, ~mutate(.x, ticker = .y)) |>
   # convert date string to date type
   map(~mutate(.x, date = as.Date(date))) |> 
   # combine the list of data frames into one
   reduce(full_join, by = c("date", "ticker")) |> 
   pivot_longer(cols = -c(date, ticker), names_to = "type", values_to = "price") |> 
   select(date, ticker,price) |> 
   arrange(date) |> 
   drop_na()

save(prices_shy, file = "data/prices_shy.RData")
} else {
   # load the data from file
   load("data/prices_shy.RData")
}
# create daily returns and volatility
window <- 60 # rolling 3 month window
returns <- prices_shy |> 
   arrange(date) |> 
   # add daily returns
   group_by(ticker) |>
   mutate(daily_return = price/lag(price)-1) |> 
   mutate(log_daily_return = log(daily_return+1)) |> 
   drop_na() |> 
   # add sd of trailing 30-day daily returns
   mutate(vol = rollapply(log_daily_return, width = window, FUN = sd, fill = NA, align = "right")) |>
   mutate(vol = vol*250^.5) |>
   mutate(value = cumprod(1+daily_return)) |>
   # remove rows with NA in any column
   drop_na()

values_fred <- values_fred |> 
   # trim to match earliest date in prices
   filter(date >= min(prices_shy$date)) |> 
   # pivot wider
   pivot_wider(names_from = ticker, values_from = price)
   

#  make a monthly frequency return data frame
window <- 12 # rolling 12 month window
returns_monthly <- prices_shy |> 
   #find date in each month closest to the 1st day of that month
   mutate(month = floor_date(date, "month")) |>
   # find the first date in each month
   filter(.by = month,date == min(date)) |> 
   select(-month) |> 
   arrange(date) |> 
   # add monthly returns
   group_by(ticker) |>
   mutate(monthly_return = price/lag(price)-1) |> 
   mutate(log_monthly_return = log(monthly_return+1)) |> 
   drop_na() |> 
   # add sd of trailing 30-day monthly returns
   mutate(vol = rollapply(log_monthly_return, width = window, FUN = sd, fill = NA, align = "right")) |>
   mutate(vol = vol*250^.5) |>
   mutate(value = cumprod(1+monthly_return)) |>
   # remove rows with NA in any column
   drop_na()

# make mountain chart
returns_monthly |> 
   ggplot(aes(x = date, y = value, color = ticker)) +
   geom_line() +
   labs(title = "Cumulative Returns",
        x = "Date",
        y = "Value") +
   theme_minimal() +
   scale_color_manual(values = c("SHY" = "green", "IEF" = "blue", "TLT" = "red"))

# plot rolling volatility
returns_monthly |> 
   ggplot(aes(x = date, y = vol, color = ticker)) +
   geom_line() +
   scale_y_continuous(labels = scales::percent) +
   scale_color_manual(values = c("blue","green","red")) +
   labs(title = str_glue("{window}-Month Rolling Volatility"),
        x = "Date",
        y = "Annualized Volatility") +
   theme_minimal()

# from Blackrock web site
eff_dur_SHY <- 1.85
eff_dur_IEF <- 7.04
eff_dur_TLT <- 15.81
convexity_SHY <- 0.11
convexity_IEF <- 0.59
convexity_TLT <- 3.44





# compute hedge ratios based on vol
# pivot longer
returns_long <- returns_monthly |> 
   pivot_longer(cols = -c(date,ticker), names_to = "item", values_to = "value")
returns_wide <- returns_long |> 
   pivot_wider(names_from = ticker, values_from = value) 


correlations <- returns_long |> 
   filter(item == "monthly_return") |>
   pivot_wider(names_from = ticker, values_from = value) |> 
  # add rolling correlation
   mutate(cor_SHY_IEF = runCor(SHY, IEF, n = window, sample = FALSE),
          cor_SHY_TLT = runCor(SHY, TLT, n = window, sample = FALSE),
          cor_IEF_TLT = runCor(IEF, TLT, n = window, sample = FALSE)) |>
   # select correlation columns
   select(date, cor_SHY_IEF, cor_SHY_TLT, cor_IEF_TLT) |>
   # rename columns to remove 'cor_' prefix
   rename_with(~str_remove(.x, "cor_")) |>
   # remove rows with NA in any column
   drop_na() |> 
   # pivot longer
   pivot_longer(cols = -date, names_to = "fund_pair", values_to = "correlation")

#plot correlations
correlations |> 
   ggplot(aes(x = date, y = correlation, color = fund_pair)) +
   geom_line() +
   labs(title = str_glue("{window}-Day Rolling Correlation"),
        x = "Date",
        y = "Correlation") +
   theme_minimal() +
   scale_y_continuous(labels = scales::percent)


window = 24
regress





summary(regression_SHY_IEF)
regression_SHY_TLT <- lm(daily_return_SHY ~ daily_return_TLT, data = values_wide)
summary(regression_SHY_TLT)
regression_IEF_TLT <- lm(daily_return_IEF ~ daily_return_TLT, data = values_wide)
summary(regression_IEF_TLT)
values_wide |> 
   ggplot(aes(x = daily_return_SHY, y = daily_return_IEF)) +
   geom_point() +
   geom_smooth(method = "lm", color = "red") +
   labs(title = "SHY vs IEF",
        x = "SHY Daily Return",
        y = "IEF Daily Return",
        subtitle = sprintf("Beta: %.2f, R²: %.2f", 
                           coef(regression_SHY_IEF)[2],
                           summary(regression_SHY_IEF)$r.squared)) +
   theme_minimal() +
   scale_y_continuous(labels = scales::percent) +
   scale_x_continuous(labels = scales::percent)

values_wide |> 
   ggplot(aes(x = daily_return_SHY, y = daily_return_TLT)) +
   geom_point() +
   geom_smooth(method = "lm", color = "red") +
   labs(title = "SHY vs TLT",
        x = "SHY Daily Return",
        y = "TLT Daily Return",
        subtitle = sprintf("Beta: %.2f, R²: %.2f", 
                           coef(regression_SHY_TLT)[2],
                           summary(regression_SHY_TLT)$r.squared)) +
   theme_minimal() +
   scale_y_continuous(labels = scales::percent) +
   scale_x_continuous(labels = scales::percent)

values_wide |> 
   ggplot(aes(x = daily_return_IEF, y = daily_return_TLT)) +
   geom_point() +
   geom_smooth(method = "lm", color = "red") +
   labs(title = "IEF vs TLT",
        x = "IEF Daily Return",
        y = "TLT Daily Return",
        subtitle = sprintf("Beta: %.2f, R²: %.2f", 
                           coef(regression_IEF_TLT)[2],
                           summary(regression_IEF_TLT)$r.squared)) +
   theme_minimal() +
   scale_y_continuous(labels = scales::percent) +
   scale_x_continuous(labels = scales::percent)
# Calculate hedge ratios (beta * ratio of prices)
values_hedged <- values_wide |> 
   mutate(
      hedge_ratio_IEF = coef(regression_SHY_IEF)[2] * SHY/IEF,
      # hedge ratios baed on duration
      duration_ratio_IEF = eff_dur_SHY / eff_dur_IEF,
      # Calculate hedged returns
      equal_hedged_IEF = 1+(daily_return_SHY),
      #duration_hedged_IEF = 1+(daily_return_SHY - duration_ratio_IEF * daily_return_IEF),
      # Calculate cumulative returns
      cum_equal_IEF = cumprod(equal_hedged_IEF),
      #cum_duration_IEF = cumprod(duration_hedged_IEF)
   )

# Plot cumulative returns
values_hedged |>
   select(date, equal_hedged_IEF) |>
   pivot_longer(-date) |>
   ggplot(aes(x = date, y = value, color = name)) +
   geom_line() +
   labs(title = "Cumulative Returns of Beta-Hedged SHY Positions",
        x = "Date",
        y = "Cumulative Return") +
   theme_minimal() +
   scale_y_continuous(labels = scales::percent)

# plot rolling hedge ratios
values_hedged |> 
   select(date, hedge_ratio_IEF, duration_ratio_IEF) |>
   pivot_longer(-date) |>
   ggplot(aes(x = date, y = value, color = name)) +
   geom_line() +
   labs(title = "Rolling Hedge Ratios",
        x = "Date",
        y = "Hedge Ratio") +
   theme_minimal() +
   scale_y_continuous(labels = scales::percent)

hr <- eff_dur_SHY / eff_dur_IEF
hr <- .1
values_hedged <- values_hedged |> 
   mutate(px_ratio = SHY/IEF) |>
   mutate(dur_ratio = SHY/(IEF * duration_ratio_IEF)) |>
   mutate(vol_ratio = SHY/(IEF*vol_SHY/hedge_ratio_IEF)) |> 
   # index all ratios to start at zero
   mutate(px_ratio = px_ratio/first(px_ratio)-1,
          dur_ratio = dur_ratio/first(dur_ratio)-1,
          vol_ratio = vol_ratio/first(vol_ratio)-1
          )

values_hedged|>
   ggplot(aes(x = date)) +
   geom_line(aes(y = px_ratio, color = "Hedge Ratio = 1:1")) +
   geom_line(aes(y = dur_ratio, color = "Duration Hedge")) +
   geom_line(aes(y = vol_ratio, color =  "Vol Hedge")) +
   
   labs(title = "Long SHY vs Short IEF",
        x = "Date",
        y = "Ratio",
        color = "Legend") +
   theme_minimal()

