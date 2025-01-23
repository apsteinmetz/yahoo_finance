library(shiny)

ui <- fluidPage(
   plotOutput("distPlot", click = "plot_click"),
   textOutput("x_click"))

server <- function(input, output) {
   
   x <- faithful$waiting
   v0 <- reactiveVal(10)
   observeEvent(input$plot_click$x,
                {
                   ifelse(is.null(input$plot_click), v0(), input$plot_click$x) |> v0()
                }
   )
   output$x_click <- renderPrint({
      v0()
   })
   output$distPlot <- renderPlot({
      hist(x)
      # add vertical line at click
      abline(v = v0(), col = 'red')
   })
}
shinyApp(ui = ui, server = server)