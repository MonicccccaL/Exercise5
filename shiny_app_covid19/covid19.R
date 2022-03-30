library(shiny)
library(tidyverse)
covid19 <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
covid19_state <- covid19%>%
  select(state)%>% distinct(state)%>% arrange(state)%>% pull(state)

ui <- fluidPage(
  selectInput(inputId = "state", 
              label = "State", 
              choices = covid19_state, 
              multiple = TRUE), 
  sliderInput(inputId = "date", 
              label = "Date:", 
              min = 2020,
              max = 2022, 
              value = c(2020, 2022), 
              sep = ""), #how to change the range of date by month?
  plotOutput(outputId = "covid19_plot"))
  
server <- function(input, output) {
  output$covid19_plot <- renderPlot(
    covid19%>%
      filter(state %in% input$state)%>%
      ggplot(aes(x = date, y = cases, color = state))+
      geom_line()+
      scale_y_log10()+
      theme_classic()+
      labs(title = "Number of Covid19 cases",
           x = "",
           y = ""))
}
shinyApp(ui = ui, server = server)