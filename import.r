# load data from cloud
# charts for dashboard

# load library or install package if library not present ----------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("quantmod")) install.packages("quantmod")
if (!require("googlesheets4")) install.packages("googlesheets4")


RELOAD <- TRUE

if (RELOAD) {
   # load cloud data --------------------------------------------------------------   
   holdings_tickers <- "https://docs.google.com/spreadsheets/d/16XDBWC4jvlSy0qkx9hFLgJ-RJ4FAwOLpVHtHSyC-lAQ/edit?usp=sharing"
   cat("Reading holdings from google docs\n")
   # gs4_auth_configure(path ="../googlesheets.json")
   # gs4_scopes()
   gs4_auth(email = "apsteinmetz@gmail.com", cache = TRUE)
   
   # ticker_sheet <- readxl::read_xlsx(path =  "Investment Inventory.xlsx",
   #                                  sheet = "Fund Detail") |>
   #   mutate(across(c("asset","asset_type","where"), as.factor))
   
   ticker_sheet <- read_sheet(holdings_tickers, sheet = "Fund Detail")
   
   ticker_sheet <- ticker_sheet |>
      mutate(across(c("asset", "asset_type", "where"), as.factor))
   
   tickers <- ticker_sheet |> pull(ticker)
   tickers_unique <- tickers |> unique()
   # load("data/prices_raw.RData")
   
   # download prices and combine into a list. Keep only the adjusted price. -------
   # By default getSymbols gets all available dates
   cat("Downloading prices from yahoo finance\n")
   prices_raw <- tickers_unique |>
      map(~ getSymbols(.x, src = "yahoo", auto.assign = FALSE)) |>
      map(as_tibble, rownames = "date") |>
      map(select, date, contains("Adjusted"))
   save(prices_raw, file = "data/prices_raw.RData")
   
   prices <- prices_raw |>
      map(~ rename_with(.x, ~ str_remove(.x, ".Adjusted"))) |>
      # convert date string to date type
      map(~ mutate(.x, date = as.Date(date))) |>
      # combine the list of data frames into one
      reduce(full_join, by = c("date")) |>
      pivot_longer(cols = -c(date), names_to = "ticker", values_to = "adj_price") |>
      # change any ticker with a period to just the stem
      mutate(ticker = str_remove(ticker, "\\..*")) |>
      select(date, ticker, adj_price) |>
      drop_na() |>
      distinct() |>
      # now add the metadata from our spreadsheet
      left_join(ticker_sheet, by = "ticker", relationship = "many-to-many") |>
      mutate(ticker = as_factor(ticker)) |>
      mutate(value = value * 1000)
   
   # impute daily return and values of each asset ----------------------------------
   # using prices$value as the terminal value, calculate the daily values based on
   # the adj_price so this is sort of running the usual calculations in reverse.
   # adjust return for beta
   values <- prices |>
      arrange(date) |>
      mutate(
         .by = asset,
         daily_return = ((adj_price) / lag(adj_price) - 1) * beta
      ) |>
      mutate(daily_return = ifelse(is.na(daily_return), 0, daily_return)) |>
      arrange(asset, desc(date)) |>
      mutate(.by = asset, asset_value = cumprod(1 - daily_return) * first(value))
   
   window = 60
   
   #   calculate the fraction of the portfolio that each asset represents
   asset_weight <-
      values |>
      mutate(
         .by = c(date),
         weight = asset_value / sum(asset_value, na.rm = TRUE)
      ) |>
      summarize(
         .by = c(date, asset_type),
         asset_value = sum(asset_value, na.rm = TRUE),
         weight = sum(weight, na.rm = TRUE)
      ) |>
      arrange(date) |>
      group_by(asset_type) |>
      mutate(daily_return = asset_value / lag(asset_value) - 1) |>
      mutate(
         rolling_volatility = zoo::rollapply(
            daily_return,
            window,
            sd,
            fill = NA,
            align = "right"
         )
      ) |>
      mutate(rolling_volatility = rolling_volatility * sqrt(252))
   
   port_weight <- values |>
      mutate(wgt_value = asset_value * daily_return) |>
      mutate(
         .by = c(date),
         weight = asset_value / sum(asset_value, na.rm = TRUE)
      ) |>
      summarize(
         .by = c(date),
         asset_value = sum(asset_value, na.rm = TRUE),
         weight = sum(weight, na.rm = TRUE)
      ) |>
      arrange(date) |>
      mutate(daily_return = asset_value / lag(asset_value) - 1) |>
      mutate(
         rolling_volatility = zoo::rollapply(
            daily_return,
            window,
            sd,
            fill = NA,
            align = "right"
         )
      ) |>
      mutate(rolling_volatility = rolling_volatility * sqrt(252)) |>
      mutate(.after = "date", asset_type = "portfolio")
   
   filter_threshold = 3 # 300%
   agg_by_day <- rbind(asset_weight, port_weight) |>
      arrange(date) |>
      # remove artifical spikes in the data do to asset inflows or outflows
      #filter(rolling_volatility/lag(rolling_volatility,window +1) < filter_threshold) |>
      filter(year(date) > 2019) |> 
      # remove NA daily returns
      filter(!is.na(daily_return))
   
   agg_by_day <- agg_by_day %>%
      ungroup() %>%
      # make sure all asset types are present for each date
      complete(
         date, asset_type,
         fill = list(asset_value = 0, daily_return = 0, weight = 0, rolling_volatility = 0)
      )
   
   save(agg_by_day, file = "data/agg_by_day.RData")
   
   # Calculate risk decomposition -------------------------------------------------
   risk_decomp <- function(date_1) {
      WINDOW <- 60
      # Calculate covariance matrix using 60-day returns
      cov_matrix <- agg_by_day |>
         filter(date <= date_1) |>
         filter(date >= date_1 - WINDOW) |>
         select(date, asset_type, daily_return) |>
         pivot_wider(names_from = asset_type, values_from = daily_return) |>
         select(-portfolio,-date,) |>
         cov(use = "complete.obs")
      
      # Get latest weights
      weights <- agg_by_day |>
         filter(asset_type != "portfolio") |>
         filter(date == date_1)  |>
         # summarize(.groups = c("asset_type"),
         #          weight = sum(weight, na.rm = TRUE))
         pull(weight)
      
      # Portfolio volatility (60-day)
      port_vol1 <- sqrt(
         # t(latest_weights$weight) %*% cov_matrix %*% latest_weights$weight) |> 
         t(weights) %*% cov_matrix %*% weights) |> 
         as.numeric()
      
      
      marg_contrib1 <- (cov_matrix %*% weights) / port_vol1
      risk_contrib1 <- (marg_contrib1 *
                           as.matrix(weights, ncol = 1)) /
         port_vol1
      risk_absolute <- (risk_contrib1 * port_vol1) * sqrt(256)
      
      combo <- risk_decomp <- tibble(
         date = date_1,
         asset_type = rownames(cov_matrix),
         weight = weights,
         marg_contrib = as.vector(marg_contrib1),
         risk_contrib = as.vector(risk_contrib1),
         risk_absolute = as.vector(risk_absolute)
      )
      return(combo)
   }
   
   
   # rolling risk computation------------------------------------------------------
   rolling_risk <- agg_by_day |> 
      filter(year(date) >= 2019) |>
      pull(date) |>
      map(risk_decomp,.progress = TRUE) |> 
      bind_rows() |> 
      arrange(desc(weight)) |> 
      # factor levels in order of weight
      mutate(asset_type  =as_factor(asset_type)) |> 
      arrange(date)  |> 
      distinct() |> 
      remove_missing()
   
   save(rolling_risk, file = "data/rolling_risk.RData")
   # animate the bar plot ---------------------------------------------------------
   rolling_risk_monthly <- rolling_risk %>%
      mutate(month = floor_date(date, "month")) %>%
      mutate(.before = date, month = format(date, "%b %Y")) |>
      group_by(month, asset_type) %>%
      summarize(
         date = min(date),
         weight = mean(weight),
         risk_contrib = mean(risk_contrib),
         marg_contrib = mean(marg_contrib),
         .groups = "drop"
      )
   rr_mon <- rolling_risk_monthly |> 
      arrange(date) |>
      # convert date to month-year format
      pivot_longer(cols = -c(month,date,asset_type), names_to = "risk_type", values_to = "metric") |>
      mutate(risk_type = factor(risk_type, levels = c("weight","risk_contrib","marg_contrib"))) |>
      group_by(month) |>
      ggplot(aes(x = asset_type, metric, fill = asset_type)) +
      geom_col(position = "dodge") +
      facet_wrap(~risk_type, scales = "free_y",
                 labeller = labeller(risk_type = facet_names)) +
      scale_fill_manual(values = pal,
                        aesthetics = c("color","fill")) +
      scale_y_continuous(labels = scales::label_percent(),
                         # different limits for each facet
                         limits = function(x) c(0, x[2] * 1.1)) +
      labs(title = "Risk Decomposition",
           x = "Asset Type",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 15),
            strip.text = element_text(size = 15, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
   
   
   anim <- rr_mon +
      transition_states(date,
                        transition_length = 3,
                        state_length = 1) +
      ggtitle("Risk Decomposition: {closest_state}") +
      enter_fade() +
      enter_grow() +
      exit_fade() +
      exit_shrink() +
      ease_aes('quadratic-in')
   # To make the transition smoother in your animation, you can modify the animation parameters in both the transition setup and the animate function. Here's the modified code for smoother transitions:
   
   # For the risk decomposition animation
   prog_expand = 50 # shrinks progress bar, 100 would be full width
   anim <- rr_mon +
      transition_states(date,
                        transition_length = 12,  # increased from 3
                        state_length = 4) +      # increased from 1
      ggtitle("Risk Decomposition: {closest_state}",
              subtitle = "{paste0(paste0(rep('=',round(progress * prog_expand)),
           collapse = ''),'>')}") +
      enter_fade() +
      enter_grow() +
      exit_fade() +
      exit_shrink() +
      ease_aes('cubic-in-out')  # changed from quadratic-in
   # anim
   
   anim_rendered <- animate(
      anim,
      width = 800,
      height = 600,
      end_pause = 10,
      renderer = gifski_renderer(loop = TRUE)
   )
   anim_save(filename = "data/rolling_risk.gif")
   
}
