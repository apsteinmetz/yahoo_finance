library(shiny)

ui <- fluidPage(plotOutput("distPlot", click = "plot_click"),
                textOutput("x_click"))

server <- function(input, output) {
   
   x    <- faithful$waiting
   output$x_click <- renderPrint({
      input$plot_click$x
   })
   output$distPlot <- renderPlot({
      hist(x)
      # add vertical line at mean
      abline(v = isolate(input$plot_click$x), col = 'red')
      # invalidateLater(1000)
   })
}
shinyApp(ui = ui, server = server)
