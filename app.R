
library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tabsetPanel(
      tabPanel(
        "PS6_The opening page", ## PROJECT OVERVIEW
        
        h1(strong("The opening page"), align = "center"),
        
        tags$figure(
          
          h2(strong("Data Summary 📊")), # TITLE
          
          tags$div(
            style = "display:block; background-color: lightblue; border:1px solid black; padding:10px;",
            "This is a Shiny app that analyzes sleep efficiency based on different lifestyle choices 
            such as coffee, alcohol, smoking, and exercise. The app has four tabs: 'The opening page' 
            which gives an overview of the project, 'Analyze different age groups based on sleep percentages', 
            'Coffee vs. Alcohol Sleep Efficiency', and 'Awakenings & Sleep Efficiency'."
          ),
        )
      ),
      
      tabPanel(
        "Analyze different age groups based on sleep percentages", ## ANALYZE BY AGE
        sidebarLayout(
          sidebarPanel(
            p("Analyze different age groups:"),
            sliderInput("range2", "Age range:",
                        min = 9,
                        max = 69,
                        value = c(21, 35)),
            checkboxGroupInput("percentage", "Select attribute",
                               choices = c("REM sleep percentage", "Deep sleep percentage", "Light sleep percentage"),
                               selected = "REM sleep percentage"),
          ),
          mainPanel(
            tableOutput("table")
          )  
        )
      ),
      
      tabPanel(
        "Coffee vs. Alcohol Sleep Efficiency", ## COFFEE VS ALCOHOL
        sidebarLayout(
          sidebarPanel(
            p("Analyze sleep efficiency based on coffee and alcohol consumption"),
            fluidRow(
              column(6,
                     checkboxGroupInput("caffeine", "Caffeine intake:",
                                        choices = c(0.0, 25.0, 50.0, 75.0, 100.0, 200.0),
                                        selected = 25.0)),
              column(6,
                     checkboxGroupInput("alcohol", "Alcohol intake:",
                                        choices = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0),
                                        selected = 1.0))
            )
          ),
          mainPanel(
            plotOutput("coffeePlot"),
            plotOutput("alcPlot")
          )
        )
      ),
      
      tabPanel(
        "Awakeninngs & Sleep Efficiency", ## COFFEE VS ALCOHOL
        sidebarLayout(
          sidebarPanel(
            p("Analyze sleep efficiency based on awakenings"),
            fluidRow(
              column(5,
                     checkboxGroupInput("awake", "Awakenings:",
                                        choices = c(0.0, 1.0, 2.0, 3.0, 4.0),
                                        selected = 1.0)),
              
            )
          ),
          mainPanel(
            plotOutput("awakePlot")
          )
        )
      ),
    )
  )
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  sleep <- read_delim("Sleep_Efficiency.csv")

    # Analyze different age groups based on sleep percentages
    sample <- reactive({
      sleep[input$range2[1]:input$range2[2], ] %>% 
        select(Age, input$percentage) %>% 
        arrange(Age)
    })
    
    output$table <- renderTable({
      sample()
    })
    
    # Coffee vs. Alcohol Sleep Efficiency
    coffeeSample <- reactive({
      sleep %>% 
        filter(`Caffeine consumption` %in% input$caffeine)
    })
    
    alcoholSample <- reactive({
      sleep %>% 
        filter(`Alcohol consumption` %in% input$alcohol)
    })
    
    output$coffeePlot <- renderPlot({
      coffeeSample() %>% 
        ggplot(aes(`Sleep efficiency`, `Caffeine consumption`, fill = `Sleep efficiency`)) +
        geom_col() +
        ggtitle("Caffeine vs. Sleep efficiency")
    })
    
    output$alcPlot <- renderPlot({
      alcoholSample() %>% 
        ggplot(aes(`Sleep efficiency`, `Alcohol consumption`, fill = `Sleep efficiency`)) +
        geom_col() +
        ggtitle("Alcohol vs. Sleep efficiency")
    })
    
    # Awakenings Sleep Efficiency
    awakeSample <- reactive({
      sleep %>% 
        filter(`Awakenings` %in% input$awake)
    })
    
    output$awakePlot <- renderPlot({
      awakeSample() %>% 
        ggplot(aes(`Sleep efficiency`, `Awakenings`, fill = `Awakenings`)) +
        geom_point(color = "black", size = 3) +
        ggtitle("Awakenigs vs. Sleep efficiency") 

    })
    
  }
  
  
# Run the application 
shinyApp(ui = ui, server = server)
