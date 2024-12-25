# RGTI vs RGTIW
library(tidyverse)
library(quantmod)
library(tseries)

# utility functions -----------------------------------------------------------
warrant_strike = 11.5
warrant_expiry = as.Date("2027-03-31")

# utility functions
# r function to compute black-sholes option price
b_s_option_val <- function(S=100, K=100, time=1, r=.02, vol, type=c("call","put")) {
   # S: current stock price
   # K: option strike price
   # time: time to expiration (in years)
   # r: risk-free interest rate
   # vol: annualized volatility of the underlying stock
   # type: "call" or "put"
   
   d1 = (log(S/K) + (r + 0.5*vol^2)*time) / (vol*sqrt(time))
   d2 = d1 - vol*sqrt(time)
   
   if(type == "call") {
      price = S*pnorm(d1) - K*exp(-r*time)*pnorm(d2)
   } else if(type == "put") {
      price = K*exp(-r*time)*pnorm(-d2) - S*pnorm(-d1)
   } else {
      stop("Invalid option type. Must be 'call' or 'put'.")
   }
   
   return(price)
}

# function to compute implied volatility using bisection method
implied_vol <- function(S, K, time, r, opt_price, type = "call", tol=1e-6, max_iter=1000) {
   # S: current stock price
   # K: option strike price
   # time: time to expiration (in years)
   # r: risk-free interest rate
   # opt_price: observed option price
   # type: "call" or "put"
   # tol: tolerance for convergence
   # max_iter: maximum number of iterations
   
   b_s_option_val <- function(S=100, K=100, time=1, r=.02, vol, type=c("call","put")) {
      # S: current stock price
      # K: option strike price
      # time: time to expiration (in years)
      # r: risk-free interest rate
      # vol: annualized volatility of the underlying stock
      # type: "call" or "put"
      
      d1 = (log(S/K) + (r + 0.5*vol^2)*time) / (vol*sqrt(time))
      d2 = d1 - vol*sqrt(time)
      
      if(type == "call") {
         price = S*pnorm(d1) - K*exp(-r*time)*pnorm(d2)
      } else if(type == "put") {
         price = K*exp(-r*time)*pnorm(-d2) - S*pnorm(-d1)
      } else {
         stop("Invalid option type. Must be 'call' or 'put'.")
      }
      
      return(price)
   }
   # define function to solve for
   f <- function(vol) {
      b_s_option_val(S, K, time, r, vol, type) - opt_price
   }
   
   # initialize variables
   lower <- 0
   upper <- 5
   iter <- 0
   
   # iterate until convergence or max iterations
   while(abs(upper - lower) > tol && iter < max_iter) {
      iter <- iter + 1
      mid <- (lower + upper) / 2
      f_mid <- f(mid)
      # update bounds based on sign of function value
      if(f_mid > 0) {
         upper <- mid
      } else {
         lower <- mid
      }
   }
   # return implied volatility
   return(mid)
}

v_implied_vol <- Vectorize(implied_vol)
# Get Prices ------------------------------------------------------------------
getSymbols(c("RGTI","SPY"))

RGTI_raw <- RGTI
SPY_raw <- SPY
RGTIW_raw <- read_csv("RGTIW.csv")

RGTIW <- RGTIW_raw |> 
   set_names(paste0("RGTIW.",names(RGTIW_raw))) |> 
   # convert date in form MM/DD/YYYY to date type
   mutate(Date = as.Date(RGTIW.Date,"%m/%d/%Y"),.before = "RGTIW.Date") |> 
   select(-2) |> 
   drop_na()

# Get daily 1-year T-bill yield from FRED as risk-free rate
tbill_raw <- getSymbols("DGS1", src = "FRED", auto.assign = FALSE)

tbill <- tbill_raw |> 
   as_tibble(rownames = "Date") |> 
   mutate(Date = as.Date(Date)) |> 
   mutate(RF = DGS1/100) |> 
   select(Date,RF) |> 
   filter(Date >= min(RGTIW$Date))

# Clean and merge data --------------------------------------------------------
prices <-  RGTI_raw %>% 
   as_tibble(rownames = "Date") |> 
   mutate(Date = as.Date(Date)) |>
   as_tibble() |> 
   # add warrant prices
   #left_join(RGTIW, by = "Date") |> 
   # add risk-free rate
   #left_join(tbill, by = "Date") |>
   mutate(daily_return = log(RGTI.Close/lag(RGTI.Close))) |> 
   # add sd of trailing 30-day daily returns
   mutate(vol_30d = rollapply(daily_return, width = 30, FUN = sd, fill = NA, align = "right")) |>
   mutate(vol_30d = vol_30d*250^.5) |>
   # remove rows with NA in any column
   drop_na()
   

# prices <-  SPY_raw %>% 
#    as_tibble(rownames = "Date") |> 
#    mutate(Date = as.Date(Date)) |>
#    as_tibble() |> 
#    # add warrant prices
#    left_join(RGTIW, by = "Date") |> 
#    # add risk-free rate
#    left_join(tbill, by = "Date") |>
#    mutate(daily_return = log(SPY.Close/lag(SPY.Close))) |> 
#    # remove rows with NA in any column
#    drop_na()

# add GARCH volatility of RGTI.Close
garch.RGTI <- garch(prices$daily_return, order = c(1,1)) |> 
   predict() |> 
   as_tibble() |> 
   mutate(vol_garch = V1*(250^.5)) |> 
   select(vol_garch)

prices <- bind_cols(prices, garch.RGTI)


# plot vol_garch
prices |> 
   ggplot(aes(x = Date)) + 
   geom_line(aes(y = vol_garch),color = "red") +
   geom_line(aes(y = vol_30d),color = "darkgreen",linewidth = 1) +
   labs(title = "GARCH Volatility of RGTI.Close",
        x = "Date",
        y = "Volatility") +
   scale_y_continuous(labels = scales::percent) +
   theme_minimal()
   

# plot closing prices
prices |> 
   ggplot(aes(x = Date)) + 
   geom_line(aes(y = RGTI.Close, color = "RGTI")) +
   geom_line(aes(y = RGTIW.Close, color = "RGTIW")) +
   labs(title = "RGTI vs RGTIW",
        x = "Date",
        y = "Price") +
   theme_minimal()

prices



# # pivot to long
# prices |> 
#    pivot_longer(cols = -1, names_to = "ticker", values_to = "price") |> 
#    mutate(ticker = str_remove(ticker,"\\.Close")) |> 
#    group_by(ticker) |>
#    mutate(daily_return = price/lag(price)-1)


# build option values
opt_values <- prices |>
   # compute years to expiry
   mutate(.after = "Date",time = as.numeric(difftime(warrant_expiry, Date, units = "days")) / 365) |> 
   mutate(
      RGTIW_imp_vol = v_implied_vol(
         S = RGTI.Close,
         K = warrant_strike,
         time = time,
         r = RF,
         opt_price = RGTIW.Close,
         type = "call"
      )) |>
   # compute intrinsic value
   mutate(RGTIW_moneyness = RGTI.Close - warrant_strike, 0) |>
   select(Date, RGTI.Close,,time,RF,RGTIW.Close, RGTIW_imp_vol,RGTIW_moneyness,vol_garch) |>
   drop_na()

   
opt_values |>    
   ggplot(aes(x = Date)) + 
   geom_line(aes(y = RGTIW_imp_vol), color = "red") +
   geom_line(aes(y = vol_garch), color = "darkgreen") +
   geom_line(aes(y = RGTIW_moneyness), color = "blue") +
   # label the lines using annotate
   annotate("text",label = "RGTIW Imp Vol", x = as.Date("2022-01-01")+250, y = 0, color = "red") +
   annotate("text",label = "RGTI Garch Vol", x = as.Date("2022-01-01")+150, y = 3, color = "darkgreen") +
   annotate("text",label = "RGTIW Moneyness", x = as.Date("2022-01-01")+500, y = -5, color = "blue") +
   labs(title = "RGTIW Option implied",
        x = "Date",
        y = "Vol") +
   theme_minimal()

