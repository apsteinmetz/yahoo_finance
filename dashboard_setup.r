# charts for dashboard

# load library or install package if library not present ----------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("showtext")) install.packages("showtext")
if (!require("quantmod")) install.packages("quantmod")
if (!require("googlesheets4")) install.packages("googlesheets4")
if (!require("gganimate")) install.packages("gganimate")
if (!require("ggfittext")) install.packages("ggfittext")


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
      
   # Use the font_add_google() function to load fonts from the web
   # Name of the fonts we need
   font <- "Josefin Sans"
   font2 <- "Open Sans"
   font3 <- "Lora"
   font_add_google(family = font, font, db_cache = FALSE)
   font_add_google(family = font2, font2, db_cache = FALSE)
   font_add_google(family = font3, font3, db_cache = FALSE)
   fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
   font_add(family = "fa-brands", regular = fa_path)
   
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

load("data/agg_by_day.RData")
load("data/rolling_risk.RData")

theme_set(theme_minimal(base_family = font2, base_size = 15))
bg <- "white"
txt_col <- "black"
showtext_auto(enable = TRUE)


rolling_risk_weekly <- rolling_risk %>%
   mutate(week = floor_date(date, "week")) %>%
   group_by(week, asset_type) %>%
   summarize(
      weight = mean(weight),
      risk_contrib = mean(risk_contrib),
      marg_contrib = mean(marg_contrib),
      .groups = "drop"
   ) %>%
   rename(date = week)

# ------------------------------------------------------------------------------

# factor levels in order of terminal size
type_levels <- as.character(terminal_values$asset_type)
terminal_values$asset_type <- fct_relevel(
   terminal_values$asset_type,
   type_levels
)
agg_by_day$asset_type <- fct_relevel(agg_by_day$asset_type, type_levels)

# factor levels in order of volatility for use in stack area
volatility_levels <- terminal_values |>
   arrange(volatility) |>
   pull(asset_type) |>
   as.character()

# plotting ---------------------------------------------------------------------
# plot the volatility by asset type over time
rolling_vol_g <-agg_by_day |>
   filter(rolling_volatility > 0) |>
   ggplot(aes(x = date, y = rolling_volatility, color = asset_type)) +
   geom_line(linewidth = .5) +
   geom_line(data = filter(agg_by_day, asset_type == "portfolio"),
             aes(x = date, y = rolling_volatility, color = asset_type),
             linewidth = 2) +
   labs(x = "", y = "Volatility", title = "Rolling Volatility by Asset Type") +
   theme_minimal() +
   #   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = seq(0, max(agg_by_day$rolling_volatility, na.rm = TRUE), by = 0.1)
   ) +
# break by 1 year
scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
   scale_color_manual(values = RColorBrewer::brewer.pal(6,"Dark2")) +
   theme_minimal()+
   theme(
      text = element_text(size = 20),
      #legend.position = "none",
      plot.title.position = "plot",
      plot.caption.position = "plot")

rolling_vol_spark <- ggplot(filter(agg_by_day,asset_type == "portfolio"),aes(date,rolling_volatility)) + 
   geom_line()


# plot the weight by asset type over time
weight_over_time <-agg_by_day |>
   filter(asset_type != "portfolio") |>
   ggplot(aes(x = date, y = weight, color = asset_type)) +
   geom_line(linewidth = 2) +
   theme_minimal() +
   geom_line() +
   labs(x = "", y = "weight", title = "Allocation by Asset Type") +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   scale_color_brewer(palette = "Set2") +
   theme_minimal() +
   # increase size of all labels
   theme(text = element_text(size = 20))


# function to insert an element into a list at an arbitrary position after 'index'
insert_item <- function(item_list, index, value) {
   if (index == 0) {
      position <- "first"
   }
   if (index >= length(item_list)) {
      position <- "last"
   }
   if (index > 0 & index < length(item_list)) {
      position <- "middle"
   }
   
   newlist <- switch(
      position,
      first = c(value, item_list),
      middle = c(
         item_list[1:index],
         value,
         item_list[index + 1:(length(item_list))]
      ),
      last = c(item_list, value)
   )
   # stop NAs from being appended. I don't understand why
   if (position == "middle") {
      newlist <- newlist[1:(length(item_list) + 1)]
   }
   return(newlist)
}

pal_raw <- rev(brewer.pal(5, "Blues"))
# #setup some custom colors
# # we need five
pal <- pal_raw[1:3] |>
  insert_item(4, "#FFC300") |>
   insert_item(5, "#FF5733")
 pal_short <- pal


# display palette colors
display_palette <- function(pal) {
   n <- length(pal)
   image(
      1:n,
      1,
      as.matrix(1:n),
      col = pal,
      xlab = "Custom",
      ylab = "",
      xaxt = "n",
      yaxt = "n",
      bty = "n"
   )
}
# display_palette(pal_raw)
# display_palette(pal_short)
terminal_values <- agg_by_day |>
   ungroup() |>
   mutate(
      .by = "asset_type",
      volatility = sd(daily_return, na.rm = TRUE) * sqrt(252)
   ) |>
   select(-c(rolling_volatility, daily_return)) |>
   filter(date == max(date)) |>
   mutate(year = year(date)) |>
   arrange(desc(asset_value)) |>
   ungroup() |>
   mutate(cum_value = cumsum(asset_value) - asset_value[1])

total_value <- agg_by_day |>
   filter(date == max(date)) |>
   filter(asset_type == "portfolio") |>
   summarize(total = sum(asset_value)/1000000) |>
   pull(total)

latest_vol <- agg_by_day |>
   filter(date == max(date)) |>
   filter(asset_type == "portfolio") |>
   pull(rolling_volatility)

historical_vol <-  mean(agg_by_day$rolling_volatility)

# set up the plot annotations --------------------------------------------------
setup_annotations <- function() {
   # add colors
   terminal_values <<- terminal_values |>
      filter(asset_type != "portfolio") |>
      mutate(color = pal_short)
   
   max_date <<- agg_by_day |>
      filter(date == max(date))
   
   annotate_title <<- function(start_date = as.Date("2021-03-15")){
      scale_fact <- .000001
      title_pos <- list(x = 0, y = 0)
      title_pos$x <- start_date + (max(agg_by_day$date) - start_date) / 5
      start_y <- 0
      title_pos$y <- start_y +
         (max(agg_by_day$asset_value) - start_y) / 3 * scale_fact
      
      ann <- annotate(
      "text",
      x = title_pos$x,
      y = title_pos$y,
      label = "Aggregated\nInvestment\nAssets",
      hjust = 0,
      size = 15,
      lineheight = .9,
      fontface = "bold",
      family = font,
      color = "black"
   )
      return(ann)
   }
   
   annotate_label <<- function(data, index = 1) {
      # prevent the stock label from overlapping the alts label
      if (data$asset_type[index] == "stock") {
         y_pos <- data$cum_value[index] * .9 * scale_fact
      } else {
         y_pos <- data$cum_value[index] * scale_fact
      }
      ann <- annotate(
         "text",
         x = data$date[index],
         y = y_pos,
         label = paste0(
            str_to_title(data$asset_type[index]),
            " $",
            round(data$asset_value[index] * scale_fact, 1),
            " MM"
         ),
         hjust = 0,
         size = 5,
         lineheight = .8,
         fontface = "bold",
         family = font2,
         color = label_pal[index]
      )
      return(ann)
   }
   
   annotate_labels <<- function(data, index = 1) {
      ann <- index |>
         map(\(x) annotate_label(data, x)) |>
         unlist(recursive = FALSE)
      return(ann)
   }
   
   verticals <<- function(year = 2020) {
      earliest <- agg_by_day |>
         filter(year(date) == year) |>
         summarize(date = min(date)) |>
         pull(date) |>
         pluck(1)
      latest <- agg_by_day |>
         filter(year(date) == year) |>
         summarize(date = max(date)) |>
         pull(date) |>
         pluck(1)
      year_date <- latest
      y_pos <- agg_by_day |>
         filter(date == year_date, asset_type == "portfolio") |>
         summarize(values = sum(asset_value)) |>
         pull(values)
      nudge <- 1
      ann <- list(
         geom_segment(
            aes(
               x = year_date,
               xend = year_date,
               y = 0,
               yend = y_pos * scale_fact * nudge
            ),
            color = "black"
         ),
         geom_point(
            aes(
               x = year_date,
               y = y_pos * scale_fact * nudge
            ),
            color = "black"
         ),
         annotate(
            "text",
            x = year_date,
            y = y_pos * scale_fact * nudge * 1.1,
            label = paste0("$", as.character(round(y_pos * scale_fact, 1)), " MM"),
            hjust = 0.5,
            size = 5,
            lineheight = .8,
            fontface = "bold",
            family = font2,
            color = "black"
         )
      )
      
      return(ann)
   }
   
   annotate_verticals <<- function(years = 2022) {
      ann <- years |> map(\(x) verticals(x)) |> unlist(recursive = FALSE)
      return(ann)
   }
}

# plot the full chart ----------------------------------------------------------
pal <- RColorBrewer::brewer.pal(5,"Dark2")
# "alts"      "stock"     "private"   "portfolio" "bond"      "cash"   
label_pal <- rev(c(pal[2],pal[5],pal[1],pal[3],pal[4]))
volatility_levels_sm <- volatility_levels[
      -which(volatility_levels == "portfolio")
   ]
terminal_values_sm <- terminal_values |>
      filter(asset_type != "portfolio") |> 
      mutate(color = label_pal) |> 
      mutate(asset_type = fct_relevel(asset_type, volatility_levels_sm)) |>
      arrange(asset_type) |>
      mutate(cum_value = cumsum(asset_value))
   
vol_sort <- agg_by_day |>
      filter(date > as.Date("2020-01-02")) |>
      filter(asset_type != "portfolio") |> 
      mutate(asset_type = fct_relevel(asset_type, volatility_levels)) |>
      mutate(asset_type = fct_rev(asset_type))

setup_annotations()

mountain <-  vol_sort |>
      ggplot(aes(
         x = date,
         y = asset_value * scale_fact,
         fill = asset_type
      )) +
      geom_area() +
      annotate_title(start_date = Sys.Date()-365*5) +
      annotate_verticals(c(2020, 2021, 2022, 2023, 2024, 2025)) +
      annotate_labels(data = terminal_values_sm, index = 1:5) +
      labs(x = "", y = "") +
      scale_y_continuous(labels = scales::dollar) +
      # scale_color_manual(values = pal) +
      # scale_fill_manual(values = rev(terminal_values_sm$color)) +
      # scale_color_manual(values = rev(terminal_values_sm$color)) +
      coord_cartesian(clip = "off") +
      theme(
         axis.line.x = element_line(linewidth = .75),
         axis.line = element_line(color = "black"),
         axis.ticks = element_line(color = "black"),
         plot.caption = element_text(size = 10, color = txt_col),
         plot.title = element_text(size = 10, color = txt_col),
         plot.margin = margin(20, 120, 20, 20),
         plot.background = element_rect(fill = "lightyellow"),
         legend.position = "none",
         text = element_text(size = 20),
         plot.title.position = "plot",
         plot.caption.position = "plot"
      )

mountain_ltm <-  vol_sort |>
   filter(date > Sys.Date()-365) |>
   ggplot(aes(
      x = date,
      y = asset_value * scale_fact,
      fill = asset_type
   )) +
   geom_area() +
   annotate_title(start_date = Sys.Date()-365) +
   # annotate_verticals(c(2020, 2021, 2022, 2023, 2024, 2025)) +
   annotate_labels(data = terminal_values_sm, index = 1:5) +
   labs(x = "", y = "") +
   scale_y_continuous(labels = scales::dollar) +
   # scale_color_manual(values = pal) +
   # scale_fill_manual(values = rev(terminal_values_sm$color)) +
   # scale_color_manual(values = rev(terminal_values_sm$color)) +
   coord_cartesian(clip = "off") +
   theme(
      axis.line.x = element_line(linewidth = .75),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      plot.caption = element_text(size = 10, color = txt_col),
      plot.title = element_text(size = 10, color = txt_col),
      plot.margin = margin(20, 120, 20, 20),
      plot.background = element_rect(fill = "lightyellow"),
      legend.position = "none",
      text = element_text(size = 20),
      plot.title.position = "plot",
      plot.caption.position = "plot"
   )
# plot the risk decomposition -------------------------------------------------

risk_decomp <- rolling_risk |> 
   distinct() |>
   ggplot(aes(x = date, y = risk_contrib, color = asset_type)) +
   # geom_line() +
   geom_smooth() +
   geom_line(aes(date,weight),linetype = "dashed",linewidth = 1) +
   labs(title = "Risk Decomposition",
        x = "Date",
        y = "Risk Contribution (Solid Line)") +
   theme_minimal() +
   # theme(legend.position = "none") +
   scale_fill_manual(values = pal,
                     aesthetics = c("color","fill")) +
   
   # add another y-axis
   scale_y_continuous(labels = scales::percent, sec.axis = sec_axis(~ . * 100, name = "Weight (Dashed Line)")) +
   
   
   scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
   # increase text size of all labels
   
   theme(text = element_text(size = 20),
         axis.text.x = element_text(angle = 45, hjust = 1))

facet_names = c(
   "weight" = "Weight",
   "risk_contrib" = "Risk\nContribution",
   "marg_contrib" = "Marginal\nContribution"
)

# plot the cumulative risk over time
cum_risk <- rolling_risk |> 
   filter(asset_type != "portfolio") |>
   # change the order of the asset_type factor levels sorted by volatility
   mutate(asset_type = fct_relevel(asset_type, rev(volatility_levels))) |>
   ggplot(aes(x = date, y = risk_absolute, fill = asset_type)) +
   geom_area() +
   labs(title = "Cumulative Risk Decomposition",
        x = "Date",
        y = "Contribution to Portfolio Volatility") +
   theme_minimal() +
   scale_fill_manual(values = (RColorBrewer::brewer.pal(5,"Dark2")),
                     aesthetics = c("color","fill")) +
   scale_y_continuous(labels = scales::percent) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
   # increase text size of all labels
   theme(text = element_text(size = 20),
         axis.text.x = element_text(angle = 45, hjust = 1))

# latest bar plot of risk decomposition 
pal <- RColorBrewer::brewer.pal(5,"Dark2")
decomp <- rolling_risk_monthly |> 
   filter(date == max(date)) |>
   arrange(desc(weight)) |>
   pivot_longer(cols = -c(month,date,asset_type), names_to = "risk_type", values_to = "metric") |>
   mutate(risk_type = factor(risk_type, levels = c("weight","risk_contrib","marg_contrib"))) |>
   ggplot(aes(x = asset_type, metric, fill = asset_type)) +
   geom_col(position = "dodge") +
   facet_wrap(~risk_type, scales = "free_y",
              labeller = labeller(risk_type = facet_names)) +
   scale_fill_manual(values = pal,
                     aesthetics = c("color","fill")) +
   geom_text(aes(label = scales::percent(metric, accuracy = 0.1)), 
             position = position_dodge(width = 0.9),
             vjust = -0.5) +
   scale_y_continuous(labels = scales::label_percent(),
                      limits = function(x) c(0, x[2] * 1.1)) +
   labs(title = "Risk Decomposition",
        x = "Asset Type",
        y = NULL) +
   theme_minimal() +
   theme(legend.position = "none",
         text = element_text(size = 20),
         strip.text = element_text(size = 16, face = "bold"),
         axis.text.x = element_text(angle = 45, hjust = 1))

chosen_month <- "Apr 2020"

# summary values -------------------------------------------------

total_value <- agg_by_day |>
   filter(date == max(date)) |>
   filter(asset_type == "portfolio") |>
   summarize(total = sum(asset_value)/1000000) |>
   pull(total)

latest_vol <- agg_by_day |>
   filter(date == max(date)) |>
   filter(asset_type == "portfolio") |>
   pull(rolling_volatility)

historical_vol <-  mean(agg_by_day$rolling_volatility)

# save all the objects in the global environment to a file
save.image("data/dashboard_setup.RData")
