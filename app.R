#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

data_pop <- read.csv("Population_Projections_in_Colorado.csv")

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Estimated Colorado population by age for a given year"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("year",
                     "Year:",
                     min = 1990,
                     max = 2050,
                     value = 2018,
                     sep="")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw plot
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      
      data_year <- data_pop %>% filter(year == input$year) %>%
        group_by(age) %>% summarise(totalPopulation = sum(totalPopulation))
      
      ggplot(data_year, aes(x=age, y=totalPopulation)) + geom_bar(stat="identity") + 
        labs(x="Age", y="Population")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

