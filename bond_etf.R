# fixed income etf hedging
library(tidyverse)
# library(tidyfit)
library(quantmod)
library(tsgarch)


# download prices and combine into a list. Keep only the adjusted price.
RELOAD = FALSE
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
save(values_fred, file = "data/values_fred.RData")
} else {
   # load the data from file
   load("data/values_fred.RData")
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
   pivot_wider(names_from = ticker, values_from = price) |> 
   # spread in basis points
   mutate(T10Y2Y = T10Y2Y*100)
   

#  make a monthly frequency return data frame
window <- 36 # rolling month window
period_scale <- 12^.5
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
   mutate(vol = vol*period_scale) |>
   mutate(value = cumprod(1+monthly_return)) |>
   # remove rows with NA in any column
   drop_na() |> 
   ungroup()

# pivot longer
returns_long <- returns_monthly |> 
   pivot_longer(cols = -c(date,ticker), names_to = "item", values_to = "value")
returns_wide <- returns_long |> 
   pivot_wider(names_from = ticker, values_from = value) 

# function to pull a single item from a named list by name
pull_item <- function(x, name) {
   x[[name]]
}

garch <- function(.ticker) {
   # return stop with error if .ticker is not in ticker
   if (!.ticker %in% unique(returns_monthly$ticker)) {
      stop("Ticker not found in returns_monthly")
   }
   returns_monthly|> 
   filter(ticker == .ticker) |>
   select(date,log_monthly_return) |>
   as.xts() |>
   garch_modelspec(model = 'egarch', constant = TRUE, 
                               init = 'unconditional', distribution = 'jsu') |> 
   estimate() |>
   pull_item("sigma") |> 
   as_tibble() |>
   rename(garch_vol = value) |>
   mutate(garch_vol = garch_vol * period_scale) |>
   mutate(.before = "garch_vol", date = pull(filter(returns_monthly,ticker == .ticker),date)) |> 
   mutate(.after= "date", ticker = .ticker) |> 
   # right_join(returns_monthly, by = c("date",ticker)) |> 
   identity()
}

returns_monthly_g <- c("SHY","TLT","IEF") |> 
   map(garch) |> 
   bind_rows() |>
   # add garch volatility to returns_monthly
   right_join(returns_monthly, by = c("date", "ticker")) |>
   relocate(garch_vol, .after = vol)

# compute average vol for the last 2 years
avg_vol <- returns_monthly_g |> 
   filter(date >= max(date)-years(2)) |> 
   group_by(ticker) |> 
   summarise(avg_vol = mean(vol, na.rm = TRUE),avg_garch_vol = mean(garch_vol, na.rm = TRUE)) |> 
   ungroup()

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
returns_monthly_g |> 
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


correlations <- returns_long |> 
   filter(item == "monthly_return") |>
   pivot_wider(names_from = ticker, values_from = value) |> 
  # add rolling correlation
   mutate(cor_IEF_SHY = runCor(SHY, IEF, n = window, sample = FALSE),
          cor_TLT_SHY = runCor(SHY, TLT, n = window, sample = FALSE),
          cor_IEF_TLT = runCor(IEF, TLT, n = window, sample = FALSE)) |>
   # select correlation columns
   select(date, cor_IEF_SHY, cor_TLT_SHY, cor_IEF_TLT) |>
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
   labs(title = str_glue("{window}-Month Rolling Correlation"),
        x = "Date",
        y = "Correlation") +
   theme_minimal() +
   scale_y_continuous(labels = scales::percent)


window = 24 # months
beta_ratios <- returns_wide |> 
   filter(item == "monthly_return") |> 
   select(-item) |> 
   mutate(beta_ratio_TLT_SHY = slider::slide_period_dbl(
      .x = tibble(TLT, SHY),
      .i = date,
      .period = "month",
      .f = ~coef(lm(TLT~SHY, data = as.data.frame(.x)))[2],
      .before = window-1)) |> 
   mutate(beta_ratio_IEF_SHY = slider::slide_period_dbl(
      .x = tibble(IEF, SHY),
      .i = date,
      .period = "month",
      .f = ~coef(lm(IEF~SHY, data = .x))[2],
      .before = window-1)) |> 
   mutate(beta_ratio_TLT_IEF = slider::slide_period_dbl(
     .x = tibble(IEF, TLT),
     .i = date,
     .period = "month",
     .f = ~coef(lm(IEF~TLT, data = .x))[2],
    .before = window-1)) |> 
   # add moving averages
   mutate(beta_ratio_TLT_SHY_ma = slider::slide_dbl(
      .x = beta_ratio_TLT_SHY,
      .i = date,
      .f = ~mean(.x, na.rm = TRUE),
      .before = window-1)) |>
   mutate(beta_ratio_IEF_SHY_ma = slider::slide_dbl(
      .x = beta_ratio_IEF_SHY,
      .i = date,
      .f = ~mean(.x, na.rm = TRUE),
      .before = window-1)) |>
   mutate(beta_ratio_TLT_IEF_ma = slider::slide_dbl(
      .x = beta_ratio_TLT_IEF,
      .i = date,
      .f = ~mean(.x, na.rm = TRUE),
      .before = window-1))
   
vol_ratios <- returns_wide |> 
   filter(item == "vol") |>
   select(-item) |>
   mutate(vol_ratio_TLT_SHY = TLT/SHY,
          vol_ratio_IEF_SHY = IEF/SHY,
          vol_ratio_TLT_IEF = TLT/IEF) |>
   # add moving averages
   mutate(vol_ratio_TLT_SHY_ma = slider::slide_dbl(
      .x = vol_ratio_TLT_SHY,
      .i = date,
      .f = ~mean(.x, na.rm = TRUE),
      .before = window-1)) |>
   mutate(vol_ratio_IEF_SHY_ma = slider::slide_dbl(
      .x = vol_ratio_IEF_SHY,
      .i = date,
      .f = ~mean(.x, na.rm = TRUE),
      .before = window-1)) |>
   mutate(vol_ratio_TLT_IEF_ma = slider::slide_dbl(
      .x = vol_ratio_TLT_IEF,
      .i = date,
      .f = ~mean(.x, na.rm = TRUE),
      .before = window-1))

# plot betas
beta_ratios |> 
   select(date, beta_ratio_TLT_SHY, beta_ratio_TLT_SHY_ma) |> 
   pivot_longer(-date, names_to = "fund_pair", values_to = "beta") |> 
   ggplot(aes(x = date, y = beta, color = fund_pair)) +
   geom_line() +
   #geom_smooth()
   labs(title = str_glue("{window}-Month Rolling Beta"),
        x = "Date",
        y = "Beta") +
   theme_minimal()

# focus on IEF/SHY
hedged_port <- returns_wide |> 
   filter(item == "monthly_return") |> 
   select(-item,-TLT) |> 
   left_join(select(beta_ratios,date,beta_ratio_IEF_SHY), by = "date") |>
   left_join(select(vol_ratios,date,vol_ratio_IEF_SHY), by = "date") |> 
   mutate(dur_ratio_IEF_SHY = eff_dur_IEF/eff_dur_SHY) |> 
   drop_na()


# regression scatter plot 
hedged_port|> 
   ggplot(aes(x = SHY, y = IEF)) +
   geom_point() +
   geom_smooth(method = "lm", color = "red") +
   labs(title = "SHY vs IEF",
        x = "SHY Monthly Return",
        y = "IEF Monthly Return",
        subtitle = sprintf("Beta: %.2f, R²: %.2f", 
                           coef(lm(IEF~SHY, data = returns_wide))[2],
                           summary(lm(IEF~SHY, data = returns_wide))$r.squared)) +
   theme_minimal() +
   scale_y_continuous(labels = scales::percent) +
   scale_x_continuous(labels = scales::percent)

# multiply SHY by each of the hedge ratios
# daily return of long hedge ratio SHY  and short 1 IEF
# use just last 3 years
# lag the hedge ratios by 1 month because next month's return is based on 
# this month's hedge ratio.  We rebalance monthly.
hedge_port_4y <- hedged_port |> 
   filter(date >= max(date)-years(4)) |>
   mutate(
      equal_hedged = SHY-IEF,
      duration_hedged = SHY*lag(dur_ratio_IEF_SHY) - IEF,
      vol_hedged = SHY * lag(vol_ratio_IEF_SHY) - IEF,
      beta_hedged = SHY * lag(beta_ratio_IEF_SHY) - IEF,
   ) |> 
   drop_na() |>
   # add the cumulative returns
   mutate(
      cum_equal_hedged = cumprod(1+equal_hedged),
      cum_duration_hedged = cumprod(1+duration_hedged),
      cum_vol_hedged = cumprod(1+vol_hedged),
      cum_beta_hedged = cumprod(1+beta_hedged)
   )
   


# plot hedged daily returns
hedge_port_4y |> 
   select(date, equal_hedged, duration_hedged, vol_hedged, beta_hedged) |>
   pivot_longer(-date) |>
   ggplot(aes(x = date, y = value, color = name,fill = name)) +
   geom_col(position = "dodge") +
   labs(title = "Hedged Returns",
        x = "Date",
        y = "Return") +
   theme_minimal() +
   scale_y_continuous(labels = scales::percent)

# plot the cumulative returns
hedge_port_4y |> 
   select(date, cum_equal_hedged, cum_duration_hedged, cum_vol_hedged, cum_beta_hedged) |>
   pivot_longer(-date) |>
   ggplot(aes(x = date, y = value, color = name)) +
   geom_line() +
   geom_line(aes(x = date, y = T10Y2Y/500+1), data = filter(values_fred,date >= max(date)-years(4)), 
             color = "black", linetype = "dashed", inherit.aes = FALSE) +
   scale_y_continuous(sec.axis = sec_axis(~.*50-1, name = "10Y-2Y Spread")) +
   
   labs(title = "Cumulative Returns Long SHY Short IEF",
        subtitle = "With Curve Shape",
        x = "Date",
        y = "Cumulative Return") +
   theme_minimal()

# plot rolling hedge ratios

x_text_pos <- max(hedged_port$date) - months(36)

hedged_port |> 
   select(date, beta_ratio_IEF_SHY, vol_ratio_IEF_SHY) |>
   pivot_longer(cols = c(beta_ratio_IEF_SHY, vol_ratio_IEF_SHY), 
                names_to = "hedge_ratio", values_to = "value") |> 
   ggplot() +
   geom_line(aes(x = date, y = value, color = hedge_ratio)) +
   scale_color_manual(values = palette()[3:5]) +
   annotate("point", x = max(hedged_port$date), y = eff_dur_IEF/eff_dur_SHY, 
            color = palette()[6], size = 4) +
   annotate("text", x = x_text_pos, y = 12,
            label = str_glue("Duration Ratio: {round(eff_dur_IEF/eff_dur_SHY,2)}"),
            color = palette()[6], hjust = -.1, size = 5) +
   annotate("text", x = x_text_pos, y = 10,
            label = str_glue("Beta Ratio: {round(tail(hedged_port$beta_ratio_IEF_SHY,1),2)}"),
            color = palette()[3], hjust = -.1, size = 5) +
   annotate("text", x = x_text_pos, y = 8, 
            label = str_glue("Vol Ratio: {round(tail(hedged_port$vol_ratio_IEF_SHY,1),2)}"),
            color = palette()[4], hjust = -.1, size = 5) +
   geom_line(aes(x = date, y = T10Y2Y/50), data = values_fred, 
             color = "black", linetype = "dashed", inherit.aes = FALSE) +
   scale_y_continuous(breaks = -1:20,sec.axis = sec_axis(~.*50, name = "10Y-2Y Spread")) +
   # increase the  y-axis major tick mark size
   labs(title = str_glue("{window}-Month Rolling Hedge Ratios SHY/IEF"),
        x = "Date", 
        y = "Hedge Ratio",
        subtitle = "Duration ratio uses only most recent published number") +
   theme_minimal()


