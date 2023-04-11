# Load required packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(data.table)

# Define the UI for the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Patent Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home Page", tabName = "landing", icon = icon("home")),
    menuItem(
      "Competition Analysis",
      tabName = "competition",
      icon = icon("chart-bar")
    ),
    menuItem(
      "Trends Analysis",
      tabName = "trends",
      icon = icon("chart-line")
    )
  )),
  dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    # Landing Page content
    tabItem(
      tabName = "landing",
      h2("Welcome to the Patent Dashboard!"),
      p(
        "Navigate to the analysis pages using the sidebar menu to perform some analysis."
      )
    ),
    
    # Competition Analysis page content
    tabItem(tabName = "competition",
            fluidRow(column(
              4,
              wellPanel(
                selectInput(
                  "patent_code_input",
                  "Patent Codes",
                  choices = c("B60L5/", "Option 2", "Option 3")
                ),
                selectInput(
                  "patent_subcode_input",
                  "Patent Subcodes",
                  choices = c("04", "18", "Option 3")
                ),
                selectInput(
                  "comp_graph_type_input",
                  "Graph Type",
                  choices = c("Total Patents", "CAGR", "Avg Claims")
                ),
                actionButton("comp_analyze", "Analyze")
              )
            ),
            column(
              8,
              box(
                title = "Competition Analysis Chart",
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                plotOutput("comp_chart", height = "300px")
              )
            ))),
    
    # Trends Analysis page content
    tabItem(tabName = "trends",
            fluidRow(column(
              4,
              wellPanel(
                selectInput(
                  "comp_input1",
                  "Patent Codes",
                  choices = c("Option 1", "Option 2", "Option 3")
                ),
                selectInput(
                  "comp_input2",
                  "Patent Subcodes",
                  choices = c("Option 1", "Option 2", "Option 3")
                ),
                selectInput(
                  "comp_input3",
                  "Graph Type",
                  choices = c("Option 1", "Option 2", "Option 3")
                ),
                actionButton("trends_analyze", "Create Graph")
              )
            ),
            column(
              8,
              box(
                title = "Trends Analysis Chart",
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                plotOutput("trends_chart", height = "300px")
              )
            )))
  ))
)

# Define the server logic
server <- function(input, output, session) {
  
  load_data <- function() {
    # Read the CSV files
    patent <- fread('g_patent_2012_2021.csv')
    assignee <- fread('g_assignee_disambiguated_2012_2021.csv')
    cpc <- fread('g_cpc_current_2012_2021.csv')
    
    # Convert patent_id to character
    cpc$patent_id <- as.character(cpc$patent_id)
    
    # Filter the cpc codes
    dt <-
      cpc %>% filter(grepl(
        pattern = 'B60L5',
        x = cpc$cpc_group,
        ignore.case = TRUE
      ))
    
    # Merge with patents and assignee
    dt <- merge(dt, patent, by = 'patent_id')
    dt <- merge(dt, assignee, by = 'patent_id')
    
    # Return the merged data table
    return(dt)
  }
  
  generateTotalPatentsChart <- function(patent_id) {
    totals <-
      dt %>% filter(disambig_assignee_organization != '') %>% 
      group_by(disambig_assignee_organization) %>% 
      summarize(total = uniqueN(patent_id)) %>% 
      arrange(desc(total)) %>% 
      slice(1:10)  # Select the top 10 companies
    
    chart <- ggplot(totals, aes(x = disambig_assignee_organization, y = total)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(title = "Total Patents", x = "Company", y = "Number of Patents") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
    
    return(chart)
  }
  
  generateCGRChart <- function(patent_id) {
    # Get top 10 companies
    totals <-
      dt %>% filter(disambig_assignee_organization != '') %>% group_by(disambig_assignee_organization) %>% summarize(total =
                                                                                                                       uniqueN(patent_id)) %>% arrange(desc(total)) %>% slice(1:10) # Select top 10 companies
    
    # Calculate patent count for each year for top 10 companies
    yearly_patent_count <- dt %>%
      filter(disambig_assignee_organization %in% totals$disambig_assignee_organization) %>%
      group_by(year = year(patent_date), disambig_assignee_organization) %>%
      summarise(n = uniqueN(patent_id))
    
    # Calculate CAGR for each company each year
    cagr_data <- yearly_patent_count %>%
      group_by(disambig_assignee_organization) %>%
      arrange(year) %>%
      mutate(prev_year_n = lag(n),
             cagr = ((n / prev_year_n)^(1/1)) - 1) %>%
      filter(!is.na(cagr))
    cat(cagr_data)
    # Create a line chart using ggplot2
    chart <- ggplot(cagr_data, aes(x = year, y = cagr, group = disambig_assignee_organization, color = disambig_assignee_organization)) +
      geom_line(size = 1) +
      geom_point() +
      theme_minimal() +
      labs(title = "Yearly CAGR", x = "Year", y = "CAGR") +
      scale_color_discrete(name = "Company") +
      theme(legend.position = "bottom", legend.text = element_text(size = 8))
    
    return(chart)
  }
  
  
  generateAvgClaimsChart <- function(patent_id) {
    # Get top 10 companies
    totals <-
      dt %>% filter(disambig_assignee_organization != '') %>% group_by(disambig_assignee_organization) %>% summarize(total =
                                                                                                                       uniqueN(patent_id)) %>% arrange(desc(total)) %>% slice(1:100)
    totals <-
      totals[order(totals$total, decreasing = T), ] %>% slice(1:100)
    
    # Calculate avg claim count for top 10 companies
    claims <- dt %>%
      filter(disambig_assignee_organization %in% totals$disambig_assignee_organization) %>%
      select(disambig_assignee_organization, patent_id, num_claims) %>%
      unique() %>%
      group_by(disambig_assignee_organization) %>%
      summarise(avg_claims = round(mean(num_claims)))
    
    # Create a bar chart using ggplot2
    chart <- ggplot(claims_data, aes(x = company, y = avg_claims)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(title = "Average Claims", x = "Company", y = "Average Number of Claims")
    
    return(chart)
  }
  
  # Reactive patent_id based on input
  patent_id <- reactive({
    paste0(input$patent_code_input, input$patent_subcode_input)
  })
  
  generateChartCompChart <- reactive({
    data <- data.frame(x = 1:10, y = rnorm(10))
    if (input$comp_graph_type_input == "Total Patents") {
      generateTotalPatentsChart(patent_id)
    } else if (input$comp_graph_type_input == "CAGR") {
      generateCGRChart(patent_id)
    } else if (input$comp_graph_type_input == "Avg Claims") {
      generateAvgClaimsChart(patent_id)
    }
  })
  
  
  # Update the plot when the analyze button is clicked
  observeEvent(input$comp_analyze, {
    output$comp_chart <- renderPlot({
      generateChartCompChart()
    })
  }, ignoreNULL = FALSE)
  
  # Trends Analysis chart output
  output$trends_chart <- renderPlot({
    # Placeholder for the chart generation code
  })
  
  # Load data when server starts
  dt <- load_data()
  
}

# Run the Shiny app
shinyApp(ui, server)
