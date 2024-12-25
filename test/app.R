library(shiny)

ui <- fluidPage(
    plotOutput("distPlot",click = "plot_click"),
    textOutput("x_click")
)

server <- function(input, output) {

    output$x_click <- renderPrint({
        input$plot_click$x
     })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
        # add vertical line at mean
        abline(v = input$plot_click$x, col = 'red')

    })
}
shinyApp(ui = ui, server = server)

