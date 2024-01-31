# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plyr)

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Coverage Change Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year1", "Select Year 1", choices = c("2020", "2019", "2018", "2017", "2016")),
      selectInput("year2", "Select Year 2", choices = c("2023", "2022", "2021", "2020", "2019"))
    ),
    mainPanel(plotOutput("coveragePlot"))
  )
)

# Define server logic
server <- function(input, output) {
  output$coveragePlot <- renderPlot({
    # Assuming hreinsad, KM, FogS are available in your environment
    
    # Data manipulation
    data <- hreinsad %>%
      mutate(CoverageChange = as.numeric(get(input$year2)) - as.numeric(get(input$year1))) %>%
      left_join(KM, by = "Reitur") %>%
      left_join(FogS, by = "Reitur") %>%
      select(species, Type, Reitur, km, Fluor, Brennisteinn, everything()) %>%
      filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54", "R55", "R57", "R58", "R59", "R61", "R62")) %>%
      select(-c(`1976`, `1997`, `2006`, `2011`, `1999`,`2014`, `2017`)) %>%
      ddply(.(km, Reitur, Fluor, Brennisteinn), summarize, CoverageChange=mean(CoverageChange, na.rm=T))
    
    # Plot
    ggplot(data, aes(x = km, y = CoverageChange)) +
      geom_point(aes(shape = ifelse(Fluor == 1, 17, ifelse(Brennisteinn == 1 & Fluor == 0, 19, 16))),
                 color = ifelse(Brennisteinn == 1 & Fluor == 0, "red", "black")) +
      scale_shape_identity() +
      labs(title = "Mean Coverage by km and Reitur",
           x = "km",
           y = "Mean Coverage") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
