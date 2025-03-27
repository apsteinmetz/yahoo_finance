# analyze asset holdings

# load library or install package if library not present ----------------------
if (!require("tidyverse"))
   install.packages("tidyverse")
if (!require("magrittr"))
   install.packages("magrittr")
# stock retrieval
if (!require("quantmod"))
   install.packages("quantmod")
if (!require("googlesheets4"))
   install.packages("googlesheets4")
if (!require("showtext"))
   install.packages("showtext")
if (!require("RColorBrewer"))
   install.packages("RColorBrewer")
if (!require("treemap"))
   install.packages("treemap")
if (!require("treemapify"))
   install.packages("treemapify")
if (!require("gganimate"))
   install.packages("gganimate")
if (!require("ggfittext"))
   install.packages("ggfittext")

RELOAD <- TRUE

# load spreadsheets from google sheets -----------------------------------------
if (RELOAD) {
   holdings_tickers <- "https://docs.google.com/spreadsheets/d/16XDBWC4jvlSy0qkx9hFLgJ-RJ4FAwOLpVHtHSyC-lAQ/edit?usp=sharing"

      # gs4_auth_configure(path ="../googlesheets.json")
   # gs4_auth()
   # gs4_scopes()
   
   ticker_sheet <- read_sheet(holdings_tickers, sheet = "Fund Detail")
   
   ticker_sheet <- ticker_sheet |>
      mutate(across(c("asset", "asset_type", "where"), as.factor))
   
   #ticker_sheet <- readxl::read_xlsx(path =  "Investment Inventory.xlsx",
   #                                  sheet = "Fund Detail") |>
   #   mutate(across(c("asset","asset_type","where"), as.factor))
   
   tickers <- ticker_sheet |> pull(ticker)
   tickers_unique <- tickers |> unique()
   # load("data/prices_raw.RData")
   
   # download prices and combine into a list. Keep only the adjusted price. -------
   # By default getSymbols gets all available dates
   
   prices_raw <- tickers_unique |>
    map(~ getSymbols(.x, src = "yahoo", auto.assign = FALSE)) |>
    map(as_tibble, rownames = "date") |>
    map(select, date, contains("Adjusted"))
   save(prices_raw, file = "data/prices_raw.RData")

   # convert the list of ticker prices to a single tidy data frame
   # the map functions lets us step through the list of data frames
   
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
   theme_set(theme_minimal(base_family = font2, base_size = 15))
   bg <- "white"
   txt_col <- "black"
   showtext_auto(enable = TRUE)
}
# and apply the same operations to each one
prices <- prices_raw |>
   map( ~ rename_with(.x, ~ str_remove(.x, ".Adjusted"))) |>
   # convert date string to date type
   map( ~ mutate(.x, date = as.Date(date))) |>
   # combine the list of data frames into one
   reduce(full_join, by = c("date")) |>
   pivot_longer(cols = -c(date),
                names_to = "ticker",
                values_to = "adj_price") |>
   # change any ticker with a period to just the stem
   mutate(ticker = str_remove(ticker, "\\..*")) |>
   select(date, ticker, adj_price) |>
   drop_na() |>
   distinct() |>
   # now add the metadata from our spreadsheet
   left_join(ticker_sheet, by = "ticker", relationship = "many-to-many") |>
   mutate(ticker = as_factor(ticker))

# impute daily return and values of each asset ----------------------------------
# using prices$value as the terminal value, calculate the daily values based on
# the adj_price so this is sort of running the usual calculations in reverse.
# adjust return for beta
values <- prices |>
   arrange(date) |>
   mutate(.by = asset, daily_return = ((adj_price) / lag(adj_price) - 1) * beta) |>
   mutate(daily_return = ifelse(is.na(daily_return), 0, daily_return)) |>
   arrange(asset, desc(date)) |>
   mutate(.by = asset,
          asset_value = cumprod(1 - daily_return) * first(value))

date = seq.Date(min(values$date), max(values$date), by = "day")
# 

# calculate volatility for each asset_type

window = 60

#   calculate the fraction of the portfolio that each asset represents
asset_allocation <-
   values |> 
   mutate(wgt_value = asset_value*daily_return) |> 
   mutate(.by = c(date), allocation = asset_value / sum(asset_value,na.rm = TRUE)) |>
   summarize(.by = c(date, asset_type),
             asset_value = sum(asset_value,na.rm = TRUE),
             allocation = sum(allocation,na.rm = TRUE))|> 
   arrange(date) |> 
   group_by(asset_type) |> 
   mutate(daily_return = asset_value/lag(asset_value) - 1) |> 
   mutate(rolling_volatility = zoo::rollapply(daily_return, window, sd, fill = NA, align = "right")) |> 
   mutate(rolling_volatility = rolling_volatility *sqrt(252))

port_allocation <- values |> 
   mutate(wgt_value = asset_value*daily_return) |> 
   mutate(.by = c(date), allocation = asset_value / sum(asset_value,na.rm = TRUE)) |>
   summarize(.by = c(date),
             asset_value = sum(asset_value,na.rm = TRUE),
             allocation = sum(allocation,na.rm = TRUE))|> 
   arrange(date) |> 
   mutate(daily_return = asset_value/lag(asset_value) - 1) |>
   mutate(rolling_volatility = zoo::rollapply(daily_return, window, sd, fill = NA, align = "right")) |> 
   mutate(rolling_volatility = rolling_volatility *sqrt(252)) |> 
   mutate(.after = "date", asset_type = "portfolio")

filter_threshold = 3 # 300%
agg_by_day <- rbind(asset_allocation, port_allocation) |> 
   arrange(date) |> 
   # remove artifical spikes in the data do to asset inflows or outflows
   filter(rolling_volatility/lag(rolling_volatility,window +1) < filter_threshold) |> 
   filter(year(date) > 2019)

   

# compute the daily return for the portfolio
values_daily_portfolio <- values |>
   group_by(date) |>
   summarize(port_return = sum(daily_return * asset_value, na.rm = TRUE) / sum(asset_value, na.rm = TRUE),
             asset_value = sum(asset_value)) |>
   arrange(date) |> 
   # add annualized volatility
   mutate(rolling_volatility = (1+zoo::rollapply(port_return, window, sd, fill = NA, align = "right"))^12-1) |> 
   # a column for the total value
   mutate(asset_type="portfolio")






# plotting ---------------------------------------------------------------------
# plot the volatility by asset type over time
agg_by_day |> 
   ggplot(aes(x=date,y=rolling_volatility,color = asset_type)) +
   geom_line() +
   labs(x = "", y = "Volatility", title = "Rolling Volatility by Asset Type") +
   theme_minimal() +
   geom_line() +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   scale_color_brewer(palette = "Set2") +
   theme_minimal()

# plot the allocation by asset type over time
agg_by_day |> 
   filter(asset_type != "portfolio") |>
   ggplot(aes(x=date,y=allocation,color = asset_type, alpha = .1)) +
   geom_line() +
   theme_minimal() +
   geom_line() +
   labs(x = "", y = "Allocation", title = "Allocation by Asset Type") +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   scale_color_brewer(palette = "Set2") +
   theme_minimal()

agg_by_day |> 
   ggplot(aes(x = date, y = asset_value, color = asset_type)) +
   geom_line() +
   labs(x = "", y = "Asset Value", title = "Asset Value by Asset Type") +
   scale_color_brewer(palette = "Set2") +
   theme_minimal()


   terminal_values <- agg_by_day |>
      filter(date == max(date)) |>
      mutate(year = year(date)) |> 
      arrange(desc(asset_value)) |> 
      # change asset_type to a factor with levels ordered by asset_value
      
   
   
      |>
      # sort by factor levels
      arrange(desc(asset_type)) |>
      # make a column of cumulative value
      mutate(cum_value = cumsum(value))
   
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
         middle = c(item_list[1:index], value, item_list[index + 1:(length(item_list))]),
         last = c(item_list, value)
      )
      # stop NAs from being appended. I don't understand why
      if (position == "middle") {
         newlist <- newlist[1:(length(item_list) + 1)]
      }
      return(newlist)
   }
   
   
   # insert a string into a vector of strings
   # pal <- c("a", "b", "c")
   # insert_item(pal, 4, "new_item")
   
   # choose our color palette -------------------------------------------------
   
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
   
   pal_raw <- rev(brewer.pal(5, "Blues"))
   #setup some custom colors
   # we need five
   pal <- pal_raw[1:3] |>
      insert_item(0, "#FFC300") |>
      insert_item(4, "#FF5733")
   pal_short <- rev(pal)
   
   terminal_values <- tibble(terminal_values, color = pal_short)
   
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
   
   # set up the plot
   
   title_pos <- list(x = 0, y = 0)
   start_date <- as.Date("2021-03-15")
   start_y = 0
   scale_fact = 1000
   title_pos$x <- start_date + (max(value_by_type$date) - start_date) / 5
   title_pos$y <- start_y + (max(value_by_type$value) - start_y) / 2 * scale_fact
   
   
   max_date <- value_by_type_aligned |>
      filter(date == max(date))
   
   annotate_title <- ggplot2::annotate(
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
         # color = txt_col
         color = rev(pal_short) |> pluck(index)
      )
      return(ann)
   }
   annotate_labels <- function(index = 1) {
      ann <- index |>
         map(\(x) annotate_label(x)) |>
         unlist(recursive = FALSE)
      return(ann)
   }
   
   
   verticals <- function(year = 2022) {
      earliest <- value_by_type_aligned |>
         filter(year(date) == year) |>
         summarize(date = min(date)) |>
         pull(date)
      
      y_pos <- value_by_type_aligned |>
         filter(date == earliest) |>
         summarize(values = sum(value)) |>
         pull(values)
      
      year_date <- earliest
      
      ann <- list(
         geom_segment(
            aes(
               x = year_date,
               xend = year_date,
               y = 0,
               yend = y_pos * scale_fact * 1.1
            ),
            color = "black"
         ),
         geom_point(aes(
            x = year_date, y = y_pos * scale_fact * 1.1
         ), color = "black"),
         annotate(
            "text",
            x = year_date,
            y = y_pos * scale_fact * 1.2,
            label = paste0("$", as.character(round(y_pos / 1000, 3)), " MM"),
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
   
   
   annotate_verticals <- function(years = 2022) {
      ann <- years |> map(\(x) verticals(x)) |> unlist(recursive = FALSE)
      return(ann)
   }
   
   # plot the full chart ----------------------------------------------------------
   value_by_type_aligned |>
      filter(date > as.Date("2020-01-02")) |>
      ggplot(aes(
         x = date,
         y = value * scale_fact,
         fill = asset_type
      )) +
      geom_area() +
      # create text annotations
      annotate_verticals(2020:2025) +
      annotate_title +
      annotate_labels(1:nrow(terminal_values)) +
      labs(x = "", y = "") +
      scale_y_continuous(labels = scales::dollar) +
      scale_fill_manual(values = pal_short) +
      scale_color_manual(values = pal_short) +
      coord_cartesian(clip = "off") +
      # increase size of axis labels
      theme(
         axis.line.x = element_line(linewidth = .75),
         panel.grid = element_blank(),
         axis.text = element_text(size = 10, color = txt_col),
         axis.title = element_text(size = 10, color = txt_col),
         axis.line = element_line(color = "black"),
         axis.ticks = element_line(color = "black"),
         # axis.ticks.length = unit(1, "cm"),
         # axis.ticks.margin = unit(1, "cm"),
         plot.caption = element_text(size = 10, color = txt_col),
         plot.title = element_text(size = 10, color = txt_col),
         plot.margin = margin(20, 120, 20, 20),
         legend.text = element_text(size = 10, color = txt_col),
         legend.title = element_text(size = 10, color = txt_col),
         legend.position = "none",
         # panel.background = element_rect(fill = bg),
         plot.background = element_rect(fill = "lightyellow"),
         # panel.grid.major = element_line(color = "grey", size = 0.5),
         # turn off display of gridlines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         plot.title.position = "plot",
         plot.caption.position = "plot"
      )
   
   # plot treemap ----------------------------------------------------------------
   annual_values <- value_by_type_aligned |>
      mutate(year = as.integer(year(date))) |>
      group_by(year) |>
      # sum the value for the last day of each year
      # reframe(year = year,value = sum(value)) |>
      filter(date == max(date))
   
   year_subset <- annual_values |>
      filter(year > 2019)  |>
      # filter(year == year_single) |>
      group_by(year)   |>
      mutate(year = as.factor(year))
   #  mutate(total = sum(value)) |>
   # mutate(subtitles = paste0("Total $", round(sum(value) / 1000, 3), " MM"))
   
   build_treemap_2 <- function(year_subset, year_single = 2024) {
      if (year_single > 0) {
         year_subset <- filter(year_subset, year == year_single) |>
            group_by(year) |>
            arrange(desc(value))
      }
      levels(year_subset$asset_type) <- year_subset$asset_type
      
      data_tree <- year_subset |>
         treemap(
            ,
            index = c("year", "asset_type"),
            vSize = "value",
            vColor = "asset_type",
            draw = FALSE
         ) |>
         pluck(1) |>
         as_tibble() |>
         group_by(year) |>
         arrange(desc(vSize)) |>
         # map colors to assets
         select(-color) |>
         left_join(select(terminal_values, asset_type, color)) |>
         mutate(asset_type = ifelse(is.na(asset_type), "Total", asset_type))
      
      total = round(sum(data_tree$vSize) / 1000, 3) |>
         as.character() |>
         str_split("\\.") |>
         unlist()
      
      total[2] <- str_pad(total[2],
                          width = 3,
                          side = "right",
                          pad = "0")
      total <- paste0("Total: $", paste(total, collapse = "."), " MM")
      
      data_tree <- data_tree |>
         mutate(
            rank = row_number(),
            xmax = x0 + w,
            ymax = y0 + h,
            vColorValue = asset_type,
            label_asset = as.character(str_glue(
               "{asset_type}\n${round(vSize/1000,3)} MM"
            ))
         ) #|>
      gg <-
         data_tree |>
         ggplot() +
         geom_rect(
            aes(
               xmin = x0,
               ymin = y0,
               xmax = xmax,
               ymax = ymax,
               fill = asset_type
            ),
            linewidth = 0.1,
            colour = "#1E1D23",
            
            alpha = 0.9
         ) +
         geom_fit_text(
            aes(
               xmin = x0,
               xmax = xmax,
               ymin = y0,
               ymax = ymax,
               label = str_to_title(label_asset)
            ),
            # colour = "#E8EADC",
            color = txt_col,
            family = "Lora",
            min.size = 4,
            reflow = TRUE
         ) +
         labs(
            title = str_glue(" Investment Assets by Type: {year_single}"),
            subtitle = total,
            caption = "Red = Crypto, Green = Cash"
         ) +
         scale_fill_manual(values = data_tree$color) +
         # hide legend
         theme_void() +
         theme(legend.position = "none") +
         #increase size of titles
         theme(
            plot.title = element_text(size = rel(2), color = txt_col),
            plot.subtitle = element_text(size = rel(2), color = txt_col),
            plot.caption = element_text(size = 15, color = txt_col)
         )
      return(gg)
   }
   
   build_treemap <- function(y_subset) {
      # levels(y_subset$asset_type) <- y_subset$asset_type
      
      total = round(sum(y_subset$value) / 1000, 3)
      year_x = y_subset$year
      
      
      y_subset |> ggplot(aes(
         area = value,
         fill = asset_type,
         label = paste(
            str_to_title(asset_type),
            "\n$",
            round(value / 1000, 3),
            "MM",
            group = year_x
         )
      )) +
         geom_treemap(layout = "fixed", radius = unit(0, "pt")) +
         geom_treemap_text(
            colour = "black",
            place = "center",
            size = 15,
            layout = "fixed",
            grow = FALSE
         ) +
         scale_fill_manual(values = pal_short) +
         labs(# title = str_glue("Investment Assets by Type: {year_x}"),
            subtitle = str_glue("End of Year Value ${total} MM\n"),
            caption = "Red = Crypto, Yellow = Cash") +
         theme_void() +
         theme(legend.position = "none") +
         # increase size of titles
         theme(
            # put a border around the plot
            plot.title = element_text(size = rel(1), color = txt_col),
            plot.subtitle = element_text(size = rel(1), color = txt_col),
            plot.caption = element_text(size = 15, color = txt_col)
         )
   }
   
   # treemap <- build_treemap(year_subset)
   # treemap
   
   
   
   # animate the treemap ---------------------------------------------------------
   anim <- treemap +
      transition_states(
         year_x,
         transition_length = c(rep(1, length(
            unique(year_subset$year)
         ) - 1), 0),
         state_length = c(rep(1, length(
            unique(year_subset$year)
         ) - 1), 9)
      ) +
      
      ggtitle("Investment Assets by Type: {closest_state}") +
      enter_fade() +
      exit_fade() +
      ease_aes('linear')
   
   anim
   
   # Change duration and framerate
   anim <- animate(
      anim,
      width = 800,
      height = 600,
      fps = 10,
      duration = 10,
      endpause = 10
   )
   print(anim)
   
   
   anim
   