# fixed income etf hedging
library(tidyverse)
# library(tidyfit)
library(quantmod)
library(tsgarch)
library(mFilter)

# ------------------------------------------------------------------------------
# download prices and combine into a list. Keep only the adjusted price.
RELOAD = FALSE
frequency <- "monthly"
curve_color = "blue"

if (RELOAD) {
  tickers = c('SHY', 'IEF', 'TLT')
  prices_raw <- tickers |>
    map(~ getSymbols(.x, src = "yahoo", auto.assign = FALSE)) |>
    map(as_tibble, rownames = "date")

  prices_shy_close <- prices_raw |>
    map(select, date, contains(".Close")) |>
    map(~ rename_with(.x, ~ str_remove(.x, ".Close"))) |>
    # use column names to make a ticker column
    map2(tickers, ~ mutate(.x, ticker = .y)) |>
    # convert date string to date type
    map(~ mutate(.x, date = as.Date(date))) |>
    # combine the list of data frames into one
    curve_coloruce(full_join, by = c("date", "ticker")) |>
    pivot_longer(
      cols = -c(date, ticker),
      names_to = "type",
      values_to = "price"
    ) |>
    select(date, ticker, price) |>
    mutate(price_type = "close") |>
    arrange(date) |>
    drop_na()

  prices_shy_adj <- prices_raw |>
    map(select, date, contains(".Adjusted")) |>
    map(~ rename_with(.x, ~ str_remove(.x, ".Adjusted"))) |>
    # use column names to make a ticker column
    map2(tickers, ~ mutate(.x, ticker = .y)) |>
    # convert date string to date type
    map(~ mutate(.x, date = as.Date(date))) |>
    # combine the list of data frames into one
    curve_coloruce(full_join, by = c("date", "ticker")) |>
    pivot_longer(
      cols = -c(date, ticker),
      names_to = "type",
      values_to = "price"
    ) |>
    select(date, ticker, price) |>
    mutate(price_type = "adjusted") |>
    arrange(date) |>
    drop_na()
  prices_shy <- bind_rows(prices_shy_close, prices_shy_adj)

  save(prices_shy, file = "data/prices_shy.RData")

  fred_codes <- c("DGS2", "DGS10", "T10Y2Y")
  fred_raw <- fred_codes |>
    map(~ getSymbols(.x, src = "fred_", auto.assign = FALSE)) |>
    map(as_tibble, rownames = "date")

  # convert the list of ticker prices to a single tidy data frame
  # the map functions lets us step through the list of data frames
  # and apply the same operations to each one
  values_fred <- fred_raw |>
    map2(fred_codes, ~ mutate(.x, ticker = .y)) |>
    # convert date string to date type
    map(~ mutate(.x, date = as.Date(date))) |>
    # combine the list of data frames into one
    curve_coloruce(full_join, by = c("date", "ticker")) |>
    pivot_longer(
      cols = -c(date, ticker),
      names_to = "type",
      values_to = "level"
    ) |>
    select(date, ticker, level) |>
    arrange(date) |>
    drop_na() |>
    pivot_wider(names_from = ticker, values_from = level) |>
    drop_na()
  save(values_fred, file = "data/values_fred.RData")
} else {
  # load the data from file
  load("data/values_fred.RData")
  load("data/prices_shy.RData")
}

values_fred <- values_fred|>
  # trim to match earliest date in prices
  # rename to substitue "T" for "DGS"
  rename_with(~ str_replace(.x, "DGS", "T"), starts_with("DGS")) |>
  mutate(T10Y2Y = T10Y2Y * 100) |>
  # add columns for monthly changes
  mutate(T2Y_chg = (T2 - lag(T2)) * 100) |>
  mutate(T10Y_chg = (T10 - lag(T10)) * 100) |>
  mutate(T10Y2Y_chg = T10Y2Y - lag(T10Y2Y)) |>
  # convert all chg to integer
  mutate(across(ends_with("_chg"), ~ round(.x, 2))) |>
  filter(date >= min(prices_shy$date))


# create daily returns and volatility
window <- 60 # rolling 3 month window
returns_daily <- prices_shy |>
  filter(price_type == "adjusted") |> 
  arrange(date) |>
  # add daily returns
  group_by(ticker) |>
  mutate(return = price / lag(price) - 1) |>
  mutate(log_return = log(return + 1)) |>
  drop_na() |>
  # add sd of trailing 30-day daily returns
  mutate(
    vol = rollapply(
      log_return,
      width = window,
      FUN = sd,
      fill = NA,
      align = "right"
    )
  ) |>
  mutate(vol = vol * 250^.5) |>
  mutate(value = cumprod(1 + return)) |>
  # remove rows with NA in any column
  drop_na()


#  make a monthly frequency return data frame
window <- 24 # rolling month window
period_scale <- 12^.5
returns_monthly <- prices_shy |>
  filter(price_type == "adjusted") |> 
  #find date in each month closest to the 1st day of that month
  mutate(month = floor_date(date, "month")) |>
  # find the first date in each month
  filter(.by = month, date == min(date)) |>
  select(-month) |>
  arrange(date) |>
  # add monthly returns
  group_by(ticker) |>
  mutate(return = price / lag(price) - 1) |>
  mutate(log_return = log(return + 1)) |>
  drop_na() |>
  # add sd of trailing 30-day monthly returns
  mutate(
    vol = rollapply(
      log_return,
      width = window,
      FUN = sd,
      fill = NA,
      align = "right"
    )
  ) |>
  mutate(vol = vol * period_scale) |>
  # remove rows with NA in any column
  drop_na() |>
  # make sure to accumlate value from start date of returns_monthly
  # after drop_na() so we start at 1
  mutate(value = cumprod(1 + return)) |>
  ungroup() |>
  arrange(date)

# chose daily or monthly returns
if (frequency == "daily") {
  returns <- returns_daily
} else if (frequency == "monthly") {
  returns <- returns_monthly
} else {
  stop("frequency must be 'daily' or 'monthly'")
}

# pivot longer
returns_long <- returns |>
  pivot_longer(cols = -c(date, ticker,price_type), names_to = "item", values_to = "value")

returns_wide <- returns_long |>
  select(-price_type) |>
  pivot_wider(names_from = ticker, values_from = value)

# function to pull a single item from a named list by name
pull_item <- function(x, name) {
  x[[name]]
}

# compute Exponential GARCH volatility
# GARCH weight current observations more heavily than past observations
# in our series it doesn't make a big difference
# Here's the fixed `garch` function:

frequency <- "monthly" # monthly or daily
if(frequency=="monthly") {
  period_scale <- 12^.5
  returns <- returns_monthly
  hp_freq <- 12
}else {
  period_scale <- 252^.5 # scale for annualizing volatility
  returns <- returns_daily
  hp_freq <-252
}

garch <- function(date,log_ret){
  hp_freq <- 12
  tibble(date,log_ret) |>
    as.xts() |>
    garch_modelspec(
      model = 'egarch',
      constant = TRUE,
      distribution = 'std'
    ) |>
    estimate() |> 
    pluck(sigma) |> 
    # filter out single period spikes
    hpfilter(freq=hp_freq) |> 
    pluck("trend") |> 
    as_tibble() |>
    mutate(.sigma = .sigma * period_scale) |>
    pull(.sigma)
}

returns_g <- returns |> 
  mutate(.by=ticker,garch_vol = garch(date, log_return))

# compute average vol for the last 4 years
avg_vol <- returns_g |>
  filter(date >= max(date) - years(4)) |>
  # group_by(ticker, price_type) |>
  summarise(.by = c("ticker", "price_type"),
    avg_vol = mean(vol, na.rm = TRUE),
    avg_garch_vol = mean(garch_vol, na.rm = TRUE)
  ) |>
  ungroup()

returns_long <- returns_g |>
  pivot_longer(cols = -c(date, ticker,price_type), names_to = "item", values_to = "value")

returns_wide <- returns_long |>
  select(-price_type) |>
  pivot_wider(names_from = ticker, values_from = value)

# from Blackrock web site
eff_dur_SHY <- 1.85
eff_dur_IEF <- 7.04
eff_dur_TLT <- 15.81
convexity_SHY <- 0.11
convexity_IEF <- 0.59
convexity_TLT <- 3.44

# full period beta
beta_SHY_TLT <- coef(lm(TLT ~ SHY, data = returns_wide))["SHY"]


correlations <- returns_long |>
  filter(item == "return") |> 
  select(-item, -price_type) |>
  pivot_wider(names_from = ticker, values_from = value) |>
  # add rolling correlation
  mutate(
    cor_IEF_SHY = runCor(SHY, IEF, n = window, sample = FALSE),
    cor_TLT_SHY = runCor(SHY, TLT, n = window, sample = FALSE),
    cor_IEF_TLT = runCor(IEF, TLT, n = window, sample = FALSE)
  ) |>
  # select correlation columns
  select(date, cor_IEF_SHY, cor_TLT_SHY, cor_IEF_TLT) |>
  # rename columns to remove 'cor_' prefix
  rename_with(~ str_remove(.x, "cor_")) |>
  # remove rows with NA in any column
  drop_na() |>
  # pivot longer
  pivot_longer(cols = -date, names_to = "fund_pair", values_to = "correlation")

# rolling window hedge ratios for beta=based and volatility-based
window = 24 # months

beta_ratios <- returns_wide |>
  filter(item == "return") |>
  select(-item) |>
  # mutate(month = floor_date(date, "month")) |>
  mutate(
    beta_ratio_TLT_SHY = slider::slide_dbl(
      .x = tibble(TLT, SHY),
      .f = ~ coef(lm(.x$TLT ~ .x$SHY))[2],
      .before = window - 1
    ),
    beta_ratio_IEF_SHY = slider::slide_dbl(
      .x = tibble(IEF, SHY),
      .f = ~ coef(lm(.x$IEF ~ .x$SHY))[2],
      .before = window - 1
    ),
    beta_ratio_TLT_IEF = slider::slide_dbl(
      .x = tibble(TLT, IEF),
      .f = ~ coef(lm(.x$TLT ~ .x$IEF))[2],
      .before = window - 1
    )
  ) |>
  mutate(
    beta_ratio_TLT_SHY_ma = slider::slide_dbl(
      beta_ratio_TLT_SHY,
      .f = mean,
      .before = window - 1,
      na.rm = TRUE
    ),
    beta_ratio_IEF_SHY_ma = slider::slide_dbl(
      beta_ratio_IEF_SHY,
      .f = mean,
      .before = window - 1,
      na.rm = TRUE
    ),
    beta_ratio_TLT_IEF_ma = slider::slide_dbl(
      beta_ratio_TLT_IEF,
      .f = mean,
      .before = window - 1,
      na.rm = TRUE
    )
  ) |>
  tail(-5) |> 
  # drop early observations with too few data points
  drop_na()

vol_ratios <- returns_wide |>
  filter(item == "vol") |>
  select(-item) |>
  mutate(
    vol_ratio_TLT_SHY = TLT / SHY,
    vol_ratio_IEF_SHY = IEF / SHY,
    vol_ratio_TLT_IEF = TLT / IEF
  ) |>
  # add moving averages
  mutate(
    vol_ratio_TLT_SHY_ma = slider::slide_dbl(
      .x = vol_ratio_TLT_SHY,
      .i = date,
      .f = ~ mean(.x, na.rm = TRUE),
      .before = window - 1
    )
  ) |>
  mutate(
    vol_ratio_IEF_SHY_ma = slider::slide_dbl(
      .x = vol_ratio_IEF_SHY,
      .i = date,
      .f = ~ mean(.x, na.rm = TRUE),
      .before = window - 1
    )
  ) |>
  mutate(
    vol_ratio_TLT_IEF_ma = slider::slide_dbl(
      .x = vol_ratio_TLT_IEF,
      .i = date,
      .f = ~ mean(.x, na.rm = TRUE),
      .before = window - 1
    )
  ) |>
  # omit first 8 observations with too few data points
  tail(-5) |>
  drop_na()

# focus on IEF/SHY
hedged_port <- returns_wide |>
  filter(item == "return") |>
  select(-item, -TLT) |>
  left_join(select(beta_ratios, date, beta_ratio_IEF_SHY), by = "date") |>
  left_join(select(vol_ratios, date, vol_ratio_IEF_SHY), by = "date") |>
  mutate(dur_ratio_IEF_SHY = eff_dur_IEF / eff_dur_SHY) |>
  drop_na()

# multiply SHY by each of the hedge ratios
# daily return of long hedge ratio SHY  and short 1 IEF
# use just last 3 years
# lag the hedge ratios by 1 month because next month's return is based on
# this month's hedge ratio.  We rebalance monthly.
hedged_port_4y <- hedged_port |>
  filter(date >= max(date) - years(4)) |>
  mutate(
    equal_hedged = SHY - IEF,
    duration_hedged = SHY * lag(dur_ratio_IEF_SHY) - IEF,
    vol_hedged = SHY * lag(vol_ratio_IEF_SHY) - IEF,
    beta_hedged = SHY * lag(beta_ratio_IEF_SHY) - IEF,
  ) |>
  drop_na() |>
  # add the cumulative returns
  mutate(
    cum_equal_hedged = cumprod(1 + equal_hedged),
    cum_duration_hedged = cumprod(1 + duration_hedged),
    cum_vol_hedged = cumprod(1 + vol_hedged),
    cum_beta_hedged = cumprod(1 + beta_hedged)
  )

hedged_port_4y_t <- hedged_port_4y |>
  # select just "_hedged" columns
  select(date, ends_with("_hedged")) |>
  select(-contains("cum")) |>
  # left join with values_fred
  left_join(values_fred, by = "date") |>
  select(-c(T2, T10)) |>
  pivot_longer(
    -c(date, starts_with("T")),
    names_to = "hedge_type",
    values_to = "hedged_return"
  )

# Here are the plotting functions moved to the bottom of the script ------------

# ggplot SHY close and adjusted prices
plot_px_vs_tr <- function(){
  prices_shy |>
  # rescale both price_types to start at one
  group_by(ticker, price_type) |>
  mutate(price = price / first(price)) |>
  # filter( ticker == "IEF") |>
  ggplot(aes(x = date, y = price, color = price_type, fill = ticker)) +
  labs(
    title = "A Huge Part of Total Return is Income",
    subtitle = "Closing Share Price and Dividend-Adjusted Price Rescaled to Start at 1",
    x = "Date",
    y = "Scaled Price"
  ) +
  geom_line() +
  facet_wrap(~ticker)
}
plot_px_vs_tr()

# plot price history
plot_prices <- function(.data = prices_shy) {
  .data |>
    filter(price_type == "close") |>
    group_by(ticker) |>
    ggplot(aes(x = date, y = price, color = ticker)) +
    geom_line() +
    labs(
      title = "Bond ETF Prices",
      # subtitle = "Adjusted for Dividend Payments",
      x = "Date",
      y = "ETF Closing Price"
    ) +
    scale_y_continuous(labels = scales::dollar) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")
}
plot_prices()

plot_cumulative_returns <- function(data = returns) {
  data |>
    ggplot(aes(x = date, y = value, color = ticker)) +
    geom_line() +
    geom_hline(yintercept = 1, color = "black") +
    labs(title = "Cumulative Returns", x = "Date", y = "Value of $1.00") +
    #      theme_minimal() +
    #      scale_color_manual(values = c("SHY" = "green", "IEF" = "blue", "TLT" = "curve_color")) +
    # currency y-scale
    scale_y_continuous(labels = scales::dollar,breaks = seq(.8, 2.1, by = 0.1)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")
}
plot_cumulative_returns()

plot_garch_compare <- function(){
  returns_g |> 
    select(date,ticker,vol,garch_vol) |>
    pivot_longer(
      cols = c(-date, -ticker),
      names_to = "vol_type",
      values_to = "volatility"
    ) |> 
    group_by(ticker) |>
    ggplot(aes(x = date, y = volatility, color = vol_type,fill = ticker)) +
    geom_line() +
    facet_wrap(~ticker, scales = "fixed") +
    labs(
      title = "GARCH Volatility of IEF",
      subtitle = "Comparison of Historical and Smoothed GARCH Volatility",
      x = "Date",
      y = "Volatility"
    ) +
    scale_y_continuous(labels = scales::percent)
}  

plot_garch_compare()

plot_rolling_volatility <- function(data = returns_g) {
  data |>
    ggplot(aes(x = date, y = garch_vol, color = ticker)) +
    geom_line() +
    scale_y_continuous(labels = scales::percent) +
    #       scale_color_manual(values = c("blue","green","curve_color")) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    labs(
      title = str_glue("{window}-Month Rolling GARCH Volatility"),
      x = "Date",
      y = "Annualized Smoothed Volatility"
    )
}
plot_rolling_volatility()

plot_correlations <- function(data = correlations) {
  data |>
    ggplot(aes(x = date, y = correlation, color = fund_pair)) +
    geom_line() +
    labs(
      title = ,
      subtitle = str_glue("{window}-Month Rolling Correlation"),
      x = "Date",
      y = "Correlation"
    ) +
    scale_y_continuous(labels = scales::percent, breaks = seq(.3, 1, by = .1)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")
}
plot_correlations()

plot_betas <- function(data = beta_ratios) {
  data |>
    select(date, beta_ratio_TLT_SHY, beta_ratio_IEF_SHY, beta_ratio_TLT_IEF) |>
    pivot_longer(-date, names_to = "fund_pair", values_to = "beta") |>
    ggplot(aes(x = date, y = beta, color = fund_pair)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    # scale_y_continuous(labels = scales::percent,breaks = seq(1,10,by=.1)) +
    labs(
      title = str_glue("{window}-Month Rolling Beta"),
      subtitle = "Long Shares vs. Short Shares",
      x = "Date",
      y = "Beta"
    )
}
plot_betas()

plot_regression_scatter <- function(data = hedged_port_4y) {
  data |>
    ggplot(aes(x = SHY, y = IEF)) +
    geom_point() +
    geom_smooth(method = "lm", color = "curve_color") +
    labs(
      title = "SHY vs IEF",
      x = "SHY Monthly Return",
      y = "IEF Monthly Return",
      subtitle = sprintf(
        "Beta: %.2f, R²: %.2f",
        coef(lm(IEF ~ SHY, data = data))[2],
        summary(lm(IEF ~ SHY, data = data))$r.squacurve_color
      )
    ) +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)
}
plot_regression_scatter()

plot_hedged_returns_scatter <- function(data = hedged_port_4y_t) {
  data |>
    group_by(hedge_type) |>
    ggplot() +
    geom_point(aes(x = T10Y2Y_chg, y = hedged_return, color = hedge_type)) +
    facet_wrap(~hedge_type, scales = "free") +
    theme(strip.text = element_text(size = 24)) +
    labs(
      title = "Monthly Hedged Returns vs. 10Y-2Y curve Changes",
      subtitle = "2020-2025",
      x = "Curve Change",
      y = "Return"
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "none") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_text(
      data = . %>%
        group_by(hedge_type) %>%
        summarise(cor = cor(hedged_return, T10Y2Y_chg)),
      aes(x = 0, y = 0, label = str_glue("R²: {round(cor^2,2)}")),
      color = "black",
      hjust = -0.2,
      vjust = -3,
      size = 5
    )
}
plot_hedged_returns_scatter()

plot_hedged_returns <- function(data = hedged_port_4y) {
  data |>
    select(date, equal_hedged, duration_hedged, vol_hedged, beta_hedged) |>
    pivot_longer(-date) |>
    ggplot(aes(x = date, y = value, color = name, fill = name)) +
    geom_col(position = "dodge") +
    labs(
      title = "Hedged Monthly Returns",
      subtitle = "Equal Hedge (1:1) is All Over the Place and More Volatile",
      x = "Date",
      y = "Return"
    ) +
    theme_minimal() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    scale_y_continuous(labels = scales::percent)
}
plot_hedged_returns()

plot_hedge_ratios <- function(data = hedged_port_4y, x_text_pos, y_text_pos) {
  x_text_pos <- max(hedged_port$date) - months(8)
  y_text_pos <- 6
  data |>
    select(date, beta_ratio_IEF_SHY, vol_ratio_IEF_SHY) |>
    pivot_longer(
      cols = c(beta_ratio_IEF_SHY, vol_ratio_IEF_SHY),
      names_to = "hedge_ratio",
      values_to = "value"
    ) |>
    ggplot() +
    geom_line(aes(x = date, y = value, color = hedge_ratio)) +
    scale_color_manual(values = palette()[3:5]) +
    annotate(
      "point",
      x = max(hedged_port$date),
      y = eff_dur_IEF / eff_dur_SHY,
      color = palette()[6],
      size = 4
    ) +
    annotate(
      "text",
      x = x_text_pos,
      y = y_text_pos,
      label = str_glue("Duration Ratio: {round(eff_dur_IEF/eff_dur_SHY,2)}"),
      color = palette()[6],
      hjust = -.1,
      size = 5
    ) +
    annotate(
      "text",
      x = x_text_pos,
      y = y_text_pos + .5,
      label = str_glue(
        "Beta Ratio: {round(tail(hedged_port$beta_ratio_IEF_SHY,1),2)}"
      ),
      color = palette()[3],
      hjust = -.1,
      size = 5
    ) +
    annotate(
      "text",
      x = x_text_pos,
      y = y_text_pos + 1,
      label = str_glue(
        "Vol Ratio: {round(tail(hedged_port$vol_ratio_IEF_SHY,1),2)}"
      ),
      color = palette()[4],
      hjust = -.1,
      size = 5
    ) +
    # curve on secondary axis
    geom_line(
      aes(x = date, y = T10Y2Y / 50 + 4),
      data = filter(values_fred, date >= min(hedged_port_4y$date)),
      color = "curve_color",
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    scale_y_continuous(
      breaks = seq(1, 10, by = .5),
      sec.axis = sec_axis(
        ~ (. - 4) * 50,
        name = "10Y-2Y Spread",
        breaks = seq(-100, 200, by = 25)
      )
    ) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    labs(
      title = str_glue(
        "Hedge Ratios Are Fairly Stable and Similar\n{window}-Month Rolling Hedge Ratios SHY/IEF"
      ),
      x = "Date",
      y = "Hedge Ratio",
      subtitle = "Note: Duration ratio uses only most recent published number"
    ) +
    theme(
      axis.title.y.right = element_text(color = "curve_color"),
      axis.text.y.right = element_text(color = "curve_color")
    )
}
plot_hedge_ratios()

plot_cumulative_hedged_returns <- function(data = hedged_port_4y) {
  data |>
    select(
      date,
      cum_equal_hedged,
      cum_duration_hedged,
      cum_vol_hedged,
      cum_beta_hedged
    ) |>
    pivot_longer(-date) |>
    ggplot(aes(x = date, y = value, color = name)) +
    geom_line() +
    geom_line(
      aes(x = date, y = T10Y2Y / 1000 + 1),
      data = filter(values_fred, date >= max(date) - years(4)),
      color = "curve_color",
      linetype = "dashed",
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    scale_y_continuous(
      labels = scales::label_currency(),
      breaks = seq(.9, 1.5, by = .05),
      sec.axis = sec_axis(
        ~ (. - 1) * 1000,
        name = "10Y -2Y Spread (BP)",
        breaks = seq(-100, 250, by = 25)
      )
    ) +
    scale_x_date(date_labels = "%Y %b", date_breaks = "6 months") +
    theme(
      axis.title.y.right = element_text(color = "curve_color"),
      axis.text.y.right = element_text(color = "curve_color")
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    annotate(
      "segment",
      x = min(data$date),
      xend = min(data$date) + months(20),
      y = 1.2,
      yend = 1.01,
      arrow = arrow(length = unit(0.2, "cm")),
      color = "black"
    ) +
    annotate(
      "segment",
      x = min(data$date) + months(24),
      xend = max(data$date),
      y = .87,
      yend = 1.01,
      arrow = arrow(length = unit(0.2, "cm")),
      color = "black"
    ) +
    annotate(
      "text",
      x = min(data$date) + months(5),
      y = 1.2,
      label = "Flattening Yield Curve",
      color = "black",
      hjust = 0,
      size = 5
    ) +
    annotate(
      "text",
      x = max(data$date) - months(5),
      y = .87,
      label = "Steepening Yield Curve",
      color = "black",
      hjust = 1,
      size = 5
    ) +
    labs(
      title = "Cumulative Value, Long SHY Short IEF, With Different Hedge Ratios",
      subtitle = "With Curve Shape",
      x = "Date",
      y = "Cumulative Return"
    )
}
plot_cumulative_hedged_returns(hedged_port_4y)

