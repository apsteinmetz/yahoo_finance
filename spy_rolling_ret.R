spy_ret <- values |> filter(ticker == "SPY") |> 
   select(ticker,date,adj_price) |> 
   distinct(ticker,date,adj_price) |>
   arrange(date) |>
   mutate(returns = log(adj_price  / lag(adj_price,254))) |>
   filter(!is.na(returns))

# change frequency to monthly
spy_ret <- spy_ret |> 
   mutate(month = floor_date(date, "month")) |>
   filter(date == month)

hist_spy <- spy_ret |> 
   pull(returns) |> 
   hist(breaks = 50)

mode_spy = list(mid = hist_spy$mids[which.max(hist_spy$counts)],
     freq = hist_spy$counts[which.max(hist_spy$counts)])

spy_ret |>
   # plot histogram with ggplot2
   ggplot(aes(x = returns)) +
   geom_histogram(bins = 50, fill = "blue", color = "black", alpha = 0.7) +
   labs(title = "SPY Rolling 10-Year Annual Returns",
        subtitle= "Jan 2008 - Mar - 2025, Monthly Sampling",
        x = "Log Returns", y = "Frequency") +
   geom_vline(xintercept = mean(spy_ret$returns), color = "red",
              linetype = "dashed",linewidth = 1) +
   geom_vline(xintercept = median(spy_ret$returns), color = "yellow",
              linetype = "dashed",linewidth = 1) +
   # black line at zero
   geom_vline(xintercept = 0, color = "black",
              linewidth = 1) +
   # x scale in percent
   scale_x_continuous(labels = scales::percent) +
   # annotate with  vline labels
   annotate("text", x = mean(spy_ret$returns), y = 11, 
            label = paste("Mean:", round(mean(spy_ret$returns)*100, 1),"%"),
            color = "red", hjust = -.4, size = 10) +
   annotate("text", x = median(spy_ret$returns), y = 14, 
            label = paste("Median:", round(median(spy_ret$returns)*100, 1),"%"),
            color = "yellow", hjust = -.1,size = 10) +
   theme_minimal() + 
   # make plot background grey
   theme(panel.background = element_rect(fill = "darkgrey"),
         panel.grid.major = element_line(color = "white"),
         panel.grid.minor = element_line(color = "white"),
         text = element_text(size = 20))

# plot return vs time   
spy_ret |>
   ggplot() +
   geom_col(data = spy_ret |> filter(returns < 0), 
            aes(x = date, y = returns), fill = "sienna2") +
   geom_col(data = spy_ret |> filter(returns >= 0), 
            aes(x = date, y = returns), fill = "forestgreen") +
   labs(title = "SPY Rolling 10-Year Annualized Returns",
        subtitle= "Jan 2008 - Mar - 2025, Monthly Sampling",
        x = "Date", y = "Log Returns") +
   theme_minimal() +
   scale_x_date(date_labels = "%Y", date_breaks = "1 year")+ 
# y scale in percent
   theme(text = element_text(size = 20)) +
   #  make y-axis labels every 10  percent
   scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.1),
                      labels = scales::percent)
