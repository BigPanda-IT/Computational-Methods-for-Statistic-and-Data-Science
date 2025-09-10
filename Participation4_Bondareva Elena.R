library(shiny)

ui <- fluidPage(
  titlePanel("Histogram of random numbers"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 0, max = 100, value = 55 ),
      selectInput("dist", "Distribution:", choices = c("Normal" = "norm", "Uniform" = "unif", "Exponential" = "exp"), selected = "norm")),
    
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output) {
  distribution <- reactive({
    switch(input$dist,
           "norm" = rnorm(input$obs),
           "unif" = runif(input$obs),
           "exp" = rexp(input$obs))
  })
  output$plot <- renderPlot({
    hist(distribution(),
         main = paste0("Histogram of ", input$dist, " distribution"),
         xlab = "Random values",
         col = "pink")
  })
  output$summary <- renderPrint({
    summary(distribution())
  })
}

shinyApp(ui = ui, server = server)




