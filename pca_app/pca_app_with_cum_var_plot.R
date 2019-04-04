# Load libraries ----------------------------------------------------------

library(shiny)
library(tidyverse)
library(knitr)
library(kableExtra)


# Load data ---------------------------------------------------------------

data <- read_tsv("data/elec_ethnicity_data.txt")


# Source helper functions -------------------------------------------------

source("perform_pca.R")
source("plot_pca.R")
source("calculate_variance_explained.R")
source("plot_cum_var_exp.R")

# Perform PCA

pca_obj <- perform_pca(data, cat_var1 = won_2016, cat_var2 = diff_2016)

# Calculate variance explained

var_explained <- calculate_variance_explained(pca_obj)


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Principal Component Analysis"),
  
  plotOutput("pca_graph"),
  
  fluidRow(
    
    column(2,
           br(),
           br(),
           br(),
           wellPanel(
           selectInput("x", 
                       label = "Choose a PC to display",
                       choices = c("1", "2", "3", "4", "5", "6"),
                       selected = "1"),
           selectInput("y", 
                       label = "Choose a PC to display",
                       choices = c("1", "2", "3", "4", "5", "6"),
                       selected = "2")
    )),
    
    column(4,
           h4("Proportion of variance explained."),
           tableOutput("kable")),
    
    column(6,
           plotOutput("cum_var_plot"))
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
    
    plot_pca(pca_obj, x_pc = x_pc, y_pc = y_pc, color_group = "won_2016")
  })
  
  output$kable <- renderTable({var_explained},
                              hover = TRUE,
                              striped = TRUE,
                              spacing = "xs",
                              align = "c")
  
  output$cum_var_plot <- renderPlot({plot_cum_var_exp(var_explained)})
}

# Run app -----------------------------------------------------------------

shinyApp(ui, server)
