library(shiny)

# Define UI for the app
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Basic Shiny App"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      # Add input controls
      sliderInput("num",
                  "Number of observations:",
                  min = 1,
                  max = 100,
                  value = 50)
    ),
    
    # Main panel for displaying output
    mainPanel(
      # Add output display
      plotOutput("scatterPlot")
    )
  )
))
