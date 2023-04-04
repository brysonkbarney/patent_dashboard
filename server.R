# Define server logic
shinyServer(function(input, output) {
  
  # Generate scatter plot
  output$scatterPlot <- renderPlot({
    # Create a data frame with random data
    data <- data.frame(x = rnorm(input$num),
                       y = rnorm(input$num))
    
    # Generate scatter plot
    ggplot(data, aes(x = x, y = y)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Scatter plot of random data",
           x = "X-axis",
           y = "Y-axis")
  })
})
