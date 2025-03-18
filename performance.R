# analyze asset holdings

# load library or install package if library not present ----------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("magrittr")) install.packages("magrittr")
# stock retrieval
if (!require("quantmod")) install.packages("quantmod")
if (!require("googlesheets4")) install.packages("googlesheets4")
if (!require("showtext")) install.packages("showtext")
if (!require("ggstream")) install.packages("ggstream")
# if (!require("glue")) install.packages("glue")

# load spreadsheets from google sheets -----------------------------------------
holdings_tickers <- "https://docs.google.com/spreadsheets/d/1S_tKUxNNfGq5Bq5gNcvjHYYCrwZ4TxTbw0E5-S_EcRU/edit?usp=sharing"
#gs4_auth_configure(path ="../googlesheets.json")
# gs4_auth()

ticker_sheet <- read_sheet(holdings_tickers, sheet = "Fund Detail")

ticker_sheet <- ticker_sheet |>
  mutate(across(c("asset", "asset_type", "where"), as.factor))

#ticker_sheet <- readxl::read_xlsx(path =  "Investment Inventory.xlsx",
#                                  sheet = "Fund Detail") |>
#   mutate(across(c("asset","asset_type","where"), as.factor))

tickers <- ticker_sheet |> pull(ticker)
tickers_unique <- tickers |> unique()


# download prices and combine into a list. Keep only the adjusted price. -------
# By default getSymbols gets all available dates

#prices_raw <- tickers_unique |>
#  map(~ getSymbols(.x, src = "yahoo", auto.assign = FALSE)) |>
#  map(as_tibble, rownames = "date") |>
#  map(select, date, contains("Adjusted"))

# save(prices_raw, file = "data/prices_raw.RData")
load("data/prices_raw.RData")
# convert the list of ticker prices to a single tidy data frame
# the map functions lets us step through the list of data frames
# and apply the same operations to each one
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
  left_join(ticker_sheet, by = "ticker") |>
  mutate(ticker = as_factor(ticker))

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

# plotting ---------------------------------------------------------------------

# calculate volatility for each asset_type
volatility <- values |>
  summarize(
    .by = asset_type,
    volatility = sd(log(1 + daily_return), na.rm = TRUE) * sqrt(252)
  ) |>
  arrange(desc(volatility))

# reorder asset_type factor by volatility
values$asset_type <- factor(values$asset_type, levels = volatility$asset_type)

# choose our color palette
pal = rev(
  c(
    "#003f5c",
    "#2f4b7c",
    "#665191",
    "#a05195",
    "#d45087",
    "#f95d6a",
    "#ff7c43",
    "#ffa600"
  )
)

shift_pal = 1
pal_short <- pal[
  (1 + shift_pal):(shift_pal + length(unique(values$asset_type)))
]
pal[(1 + shift_pal):(shift_pal + length(unique(values$asset_type)))]
value_by_type <- values |>
  summarize(.by = c(date, asset_type), value = sum(asset_value)) |>
  arrange(date)

# set up the plot
# Name of the fonts we need
font <- "Josefin Sans"
font2 <- "Open Sans"

# Use the font_add_google() function to load fonts from the web
font_add_google(family = font, font, db_cache = FALSE)
font_add_google(family = font2, font2, db_cache = FALSE)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
theme_set(theme_minimal(base_family = font2, base_size = 15))
bg <- "white"
txt_col <- "black"
showtext_auto(enable = TRUE)

title_pos <- list(x = 0, y = 0)
start_date <- as.Date("2021-03-15")
start_y = 0
scale_fact = 1000
title_pos$x <- start_date + (max(value_by_type$date) - start_date) / 5
title_pos$y <- start_y + (max(value_by_type$value) - start_y) / 2 * scale_fact


# mountain chart
# value_by_type |>
#   # filter(date > as.Date("2021-03-15")) |>
#   ggplot(aes(
#     x = date,
#     y = value,
#     fill = asset_type,
#     color = asset_type,
#     label = asset_type
#     )) +
#   # stacked area plot
#    geom_area()

# The error occurs because `geom_stream()` expects data for all groups (asset_type) to have the same number of rows, but your data has different date ranges for different assets. To fix this, you need to ensure all asset types have values for the same date range. Here's how to modify your data before plotting:

value_by_type_aligned <- value_by_type |>
  filter(date > as.Date("2020-03-15")) |>
  complete(
    date = seq.Date(min(date), max(date), by = "day"),
    asset_type = unique(asset_type),
    fill = list(value = 0)
  ) |>
  # remove weekends
  filter(!(weekdays(date) %in% c("Saturday", "Sunday"))) |>
  # remove all dates where all values are zero on that date
  group_by(date) |>
  filter(sum(value) > 0) |>
   # smooth series by taking the rolling weekly average
  group_by(asset_type) |>
  mutate(value = zoo::rollmean(value, 14, fill = NA, align = "right")) |>
  ungroup() |>
  drop_na() # |>

max_date <- value_by_type_aligned |>
  filter(date == max(date))

terminal_values <- value_by_type |>
  filter(date == max(date)) |>
  mutate(value = value * scale_fact) |>
  # sort by factor levels
  arrange(desc(asset_type)) |>
  # make a column of cumulative value
  mutate(cum_value = cumsum(value))

annotate_title <- ggplot2::annotate(
  "text",
  x = title_pos$x,
  y = title_pos$y,
  label = "Aggregated\nInvesment\nAssets",
  hjust = 0,
  size = 15,
  lineheight = .9,
  fontface = "bold",
  family = font,
  color = "black"
)

annotate_label <- function(index = 1) {
  if (terminal_values$asset_type[index] == "stock") {
    y_pos <- terminal_values$cum_value[index] * .9
  } else {
    y_pos <- terminal_values$cum_value[index]
  }
  ann <- annotate(
    "text",
    x = terminal_values$date[index],
    y = y_pos,
    label = paste0(
      str_to_title(terminal_values$asset_type[index]),
      " ",
      round(terminal_values$value[index] / 1000000, 3),
      " MM"
    ),
    hjust = 0,
    size = 5,
    lineheight = .8,
    fontface = "bold",
    family = font2,
    color = rev(pal_short) |> pluck(index)
  )
  return(ann)
}

value_by_type_aligned |>
  ggplot(aes(
    x = date,
    y = value * scale_fact,
    fill = asset_type
  )) +
  geom_area() +
  #geom_stream(true_range = "none")+
  annotate_title +
  # label_geoms +
  annotate_label(1) +
  annotate_label(2) +
  annotate_label(3) +
  annotate_label(4) +
  annotate_label(5) +
  labs(x = "", y = "") +
  #  theme_minimal() +
  #  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::dollar) +
  #  scale_x_date(date_labels = "%b %Y", date_minor_breaks = "1 month") |>
  # rotate x labels
  scale_fill_manual(values = pal_short) +
  scale_color_manual(values = pal_short) +
  coord_cartesian(clip = "off") +
  # increase size of axis labels
  theme(
     axis.line.x = element_line(linewidth = .75),
     panel.grid = element_blank(),
     # axis.text.y=element_blank(),
     
    axis.text = element_text(size = 10, color = txt_col),
    axis.title = element_text(size = 10, color = txt_col),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    # axis.ticks.length = unit(1, "cm"),
    # axis.ticks.margin = unit(1, "cm"),
    plot.caption = element_text(size = 10, color = txt_col),
    plot.title = element_text(size = 10, color = txt_col),
    plot.margin = margin(20,120,20,20),
    legend.text = element_text(size = 10, color = txt_col),
    legend.title = element_text(size = 10, color = txt_col),
    legend.position = "none",
    # panel.background = element_rect(fill = bg),
    plot.background = element_rect(fill = bg),
    panel.grid.major = element_line(color = "grey", size = 0.5),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
  )


# Treemap
# value_by_type |>
#   filter(date == max(date)) |>
#   ggplot(aes(area = value, fill = asset_type, label = asset_type)) +
#   geom_treemap() +
#   geom_treemap_text() +
#   labs(title = "Portfolio Value", x = "Date", y = "Value") +
#   scale_fill_manual(values = pal_short) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   scale_y_continuous(labels = scales::dollar) +
#   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") |>
#     # rotate x labels
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
