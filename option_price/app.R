library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(bslib)

# constants
RF <- 4.5/100 # risk-free interest rate
UL = 20 # upper limit of underlying price
warrant_strike = 11.5
warrant_expiry = as.Date("2027-03-31")

# utility functions
# r function to compute black-scholes option price
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

# function to compute option delta
option_delta <- function(S, K, time, r, vol, type) {
    d1 = (log(S/K) + (r + 0.5*vol^2)*time) / (vol*sqrt(time))
    if(type == "call") {
        return(pnorm(d1))
    } else if(type == "put") {
        return(pnorm(d1) - 1)
    } else {
        stop("Invalid option type. Must be 'call' or 'put'.")
    }
}

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

# function to compute option gamma
option_gamma <- function(S, K, time, r, vol) {
    d1 = (log(S/K) + (r + 0.5*vol^2)*time) / (vol*sqrt(time))
    return(dnorm(d1) / (S*vol*sqrt(time)))
}

# return a tibble with the option price, delta and gamma
option_info <- function(S, K, time, r, vol, type = "call",direction = "long") {
    tibble(
        Price = b_s_option_val(S, K, time, r, vol, type),
        Delta = option_delta(S, K, time, r, vol, type),
        Gamma = option_gamma(S, K, time, r, vol)) %>%
      mutate(across(everything(),\(x) if_else(direction == "short",-x,x))) %>% 
      pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value")
}

#  date sequence of third friday of each month from 2025 to 2029
third_friday <- function(year) {
    # create a sequence of dates for the third friday of each month
    # from January to December in the given year
    dates <- lapply(1:12, function(month) {
        # get the first day of the month
        first_day <- as.Date(paste(year, month, "01", sep = "-"))
        # get the weekday of the first day
        first_weekday <- weekdays(first_day)
        # calculate the offset to the third friday
        offset <- match("Friday", c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) - match(first_weekday, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        # calculate the date of the third friday
        third_friday <- first_day + 14 + offset
        # return the date
        return(third_friday)
    })
    # unlist the dates
    return(reduce(dates,.f = c))
}

option_expiries <- sort(third_friday(2024:2029)) %>% 
    # remove dates not in future using base R
    (function(x) x[x > Sys.Date()])

#  option_expiries closest to warrant_expiry
closest_expiry <- which.min(abs(as.numeric(option_expiries - warrant_expiry)))

# Define UI for application that draws a histogram
ui <- page_sidebar(
    title = "Trade Value",
    
    # Sidebar with a slider input for number of bins
    sidebar = sidebar(
        card(
            card_header("Option Parameters"),
            selectInput(
                "expiry_date",
                "Expiry Date:",
                choices = option_expiries,
                selected = option_expiries[closest_expiry]
            ),
            sliderInput(
                "call_strike",
                "Call Strike:",
                min = 0,
                max = 20,
                value = 10,
                step = 0.5
            ),
            sliderInput(
                "call_vol",
                "Call Vol:",
                min = 0,
                max = 200,
                value = 50
            )
            
        ),
        card(
            card_header("Warrant Parameters"),
            sliderInput(
                "warrant_vol",
                "Warrant Vol:",
                min = 0,
                max = 200,
                value = 50
            )
        ),
        card(
            card_header("Trade Parameters"),
            sliderInput(
                "time_to_expiry",
                "Time to Expiry (years):",
                min = 0,
                max = 3,
                round = -2,
                value = 2,
                step = .01
            ),
            numericInput(
                "call_trade_price",
                "Call Sale Price:",
                min = 0,
                max = 20,
                value = 10,
                step = 0.5
            ),
            numericInput(
                "warrant_trade_price",
                "Warrant Buy Price:",
                min = 0,
                max = 20,
                value = 10,
                step = 0.5
            )
        )
    ),
    # Show a plot of the generated distribution
    card(
        title = "Option Value vs. Underlying Price",
        card(
           sliderInput(
              "stock_price",
              "Stock Price:",
              min = 0,
              max = UL,
              step = 0.1,
              value = warrant_strike,
              width = "100%"
           ),
           card(
             layout_columns(
                fill = FALSE,
                value_box(theme = "green","Warrant", p(tableOutput("warrant_info"))),
                value_box(theme = "pink","Call", p(tableOutput("call_info"))),
                value_box(theme = "blue","Net", p(tableOutput("net_info"))),
             )
             ),
        plotOutput("optPlot"))
    )
)

# SERVER ----------------------------------------------------------------------
server <- function(input, output,session) {
    # last date for existence of position
    last_date <- reactive({
        min(input$expiry_date, warrant_expiry)
    })
    
    observe({
       x <- round(as.numeric(difftime(
          min(input$expiry_date, warrant_expiry), Sys.Date(), units = "days")
          ) / 365,2)
       
        updateSliderInput(
            session,
            "time_to_expiry",
            value =  x,
            min = 0,
            max = x,
            step = .01
        )
    })

    values_df <- reactive({
        data.frame(stock_price = seq(0, UL, 0.1)) |> 
            mutate(
                call_val = -b_s_option_val(
                    S = stock_price,
                    K = input$call_strike,
                    time = input$time_to_expiry,
                    r = RF,
                    vol = input$call_vol/100,
                    type = "call"
                ),
                warrant_val = b_s_option_val(
                    S = stock_price,
                    K = warrant_strike,
                    time = input$time_to_expiry,
                    r = RF,
                    vol = input$warrant_vol/100,
                    type = "call"
                )
            ) |> 
            mutate(net_val = warrant_val + call_val)
    })
    
    output$optPlot <- renderPlot({
        ggplot(values_df(), aes(stock_price)) +
            # call option line
            geom_line(aes(y= call_val), color = "blue") +
            # warrant line
            geom_line(aes(y= warrant_val), color = "red") +
            geom_line(aes(y= net_val), color = "darkgreen",linewidth = 1) +
            xlab("Underlying Price") +
            ylab("Option Value") +
            ggtitle("Option Value vs. Underlying Price") + 
            # vertical line at strike price
            geom_vline(xintercept = input$call_strike, linetype = "dashed",color = "darkblue") +
            annotate("text", x = input$call_strike, y = 10, label = "Call Strike Price") +
            geom_vline(xintercept = warrant_strike, linetype = "dashed",color = "red") +
            geom_vline(xintercept = input$stock_price, ,color = "#007bc2",linewidth = 1) +
            annotate("text", x = warrant_strike, y = 8, label = "Warrant Exercise Price") +
            # label each line with the corresponding volatility
            annotate("text", x = 5, y = 10, label = "Call Option", color = "darkblue") +
            annotate("text", x = 5, y = 8, label = "Warrant", color = "red") +
            annotate("text", x = 5, y = 6, label = "Net Value", color = "darkgreen") +
            #add horizontal line at y = 0
            geom_hline(yintercept = 0, linetype = "solid",linewidth = 1) +
            #ylim(-10, 10) +
            #xlim(0,UL) + 
            # crop plot to limits
            coord_cartesian(ylim = c(-10, 10), xlim = c(0, UL)) +
            scale_x_continuous(expand = c(0,0))
        
    })

    values_position <- reactive({
       call = option_info(
          S = input$stock_price,
          K = input$call_strike,
          time = input$time_to_expiry,
          r = RF,
          vol = input$call_vol/100,
          type = "call",
          direction = "short") %>% 
          rename("Call" = "Value")
       warrant = option_info(
          S = input$stock_price,
          K = warrant_strike,
          time = input$time_to_expiry,
          r = RF,
          vol = input$warrant_vol/100,
          type = "call") %>% 
          rename("Warrant" = "Value")
       left_join(call, warrant, by = "Metric") %>% 
          mutate("Net" = Call + Warrant)
    })
    
    output$call_info <- renderTable({
       values_position()[,c("Metric","Call")]
    })
    output$warrant_info <-renderTable({
       values_position()[,c("Metric","Warrant")]
    })
    output$net_info <-renderTable({
       values_position()[,c("Metric","Net")]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
