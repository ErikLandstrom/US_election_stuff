# Load libraries ----------------------------------------------------------

library(shiny)
library(tidyverse)

# Load data ---------------------------------------------------------------

data <- read_tsv("data/elec_ethnicity_data.txt")


# Source helper functions -------------------------------------------------

source("perform_pca.R")
source("plot_pca.R")

# Perform PCA

pca_obj <- perform_pca(data, cat_var1 = won_2016, cat_var2 = diff_2016)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Principal Component Analysis"),
  
  plotOutput("pca_graph"),
  
  fluidRow(
    
    column(12,
           h4("Choose two principal components to plot."),
           selectInput("x", 
                       label = "Choose a variable to display",
                       choices = c("1", "2", "3", "4", "5", "6"),
                       selected = "1"),
           selectInput("y", 
                       label = "Choose a variable to display",
                       choices = c("1", "2", "3", "4", "5", "6"),
                       selected = "2")
    )
  )
)

server <- function(input, output) {
  
  output$pca_graph <- renderPlot({
    x_pc <- switch(input$x,
                   "1" = 1, 
                   "2" = 2,
                   "3" = 3, 
                   "4" = 4,
                   "5" = 5,
                   "6" = 5)
    
    y_pc <- switch(input$y,
                   "1" = 1, 
                   "2" = 2,
                   "3" = 3, 
                   "4" = 4,
                   "5" = 5,
                   "6" = 5)
    
    plot_pca(pca_obj, x_pc = x_pc, y_pc = y_pc, "PCA", color_group = "won_2016")
  })
  
}

# Run app -----------------------------------------------------------------

shinyApp(ui, server)
