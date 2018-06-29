#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(gapminder)

select_continent <- c("Asia", "Europe", "Africa", "Americas", "Oceania")
min_year <- min(gapminder$year)
max_year <- max(gapminder$year)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Gapminder Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("continent_opts", "Select Continent",
                    choices = select_continent),
         sliderInput("year_filter", "Select Year", min = min_year,
                     max=max_year, value = min_year)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("scatter_plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  gp_filtered <- reactive({
    gapminder %>% filter(year > input$year_filter) %>% filter(continent == input$continent_opts)
  })
  
  output$scatter_plot <- renderPlot({
    gp_filtered() %>% ggplot(aes_string(y="lifeExp",                   #reactive
                                        x="year",
                                        color="country"
                                             )) + 
      geom_line() +
      geom_point(aes_string(size="pop"))
    
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

