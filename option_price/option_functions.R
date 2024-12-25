library(tidyverse)

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


b_s_option_val(11.5,11.5,r=0.05,vol=2,time=1,type="call")

# plot option value for a range of underlying prices
stock_price <- seq(0, 20, by = 0.1)
# calculate option value for each underlying price using map function
call_values <- map_dbl(stock_price, b_s_option_val,K=10, r=0.05, vol=0.2, time=2, type="call")
warrant_values <- map_dbl(stock_price, b_s_option_val,K=11.5, r=0.05, vol=0.2, time=2, type="call")


call_cost <- 5.05
warrant_cost <- 5.50
current_price <- 7
call_profit <- call_cost - call_values
warrant_profit <- warrant_cost - warrant_values

net_values <- warrant_values - call_values
net_profit <- warrant_profit - call_profit

trade_value <- tibble(stock_price,
                      call_values,
                      warrant_values,
                      call_profit,
                      warrant_profit,
                      net_profit
                      )

trade_value_plot <- trade_value |> 
   pivot_longer(cols = -c(stock_price), names_to = "type", values_to = "value")

trade_value_plot %>%
#   filter(type %in% c("call_profit")) |>
   ggplot(aes(stock_price, value, color = type)) +
   geom_line() +
   geom_vline(xintercept = current_price, linetype = "dashed") +
   xlab("Underlying Price") +
   ylab("Exit Values") +
   ggtitle("Option Value vs. Underlying Price") +
   theme_minimal()


# function to compute implied volatility using bisection method
implied_vol <- function(S, K, time, r, opt_price, type, tol=1e-6, max_iter=1000) {
  # S: current stock price
  # K: option strike price
  # time: time to expiration (in years)
  # r: risk-free interest rate
  # opt_price: observed option price
  # type: "call" or "put"
  # tol: tolerance for convergence
  # max_iter: maximum number of iterations
  
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
    # update iteration count
    iter <- iter + 1
    
    # calculate midpoint
    mid <- (lower + upper) / 2
    
    # evaluate function at midpoint
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

# test implied volatility function
implied_vol(S=11.5,K=11.5,time=1, r=0.05, opt_price=8, "call")

1:100 |> map_dbl(\(x) implied_vol(S=11.5,K=11.5,time=1, r=0.05, opt_price=x/10, "call"))
    
