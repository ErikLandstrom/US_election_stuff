# Load libraries ----------------------------------------------------------

library(shiny)
library(tidyverse)
library(maps)
library(ggplot2)

# Load data ---------------------------------------------------------------

county_demo_data <- read_tsv("data/2015_demographics_census_county_data.txt")


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
               information from the 2015 US Census."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent White", 
                              "Percent Black",
                              "Percent Hispanic", 
                              "Percent Asian",
                              "Percent Native American",
                              "Percent Pacific islanders"),
                  selected = "Percent Hispanic")
      ),
    
    mainPanel(
      plotOutput("map",
                 dblclick = "plot1_dblclick",
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE))
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$map <- renderPlot({
    ethnicity <- switch(input$var,
                   "Percent White"             = county_demo_data$White, 
                   "Percent Black"             = county_demo_data$Black,
                   "Percent Hispanic"          = county_demo_data$Hispanic, 
                   "Percent Asian"             = county_demo_data$Asian,
                   "Percent Native American"   = county_demo_data$Native,
                   "Percent Pacific islanders" = county_demo_data$Pacific)
    
    ggplot(data = county_demo_data) + 
      geom_polygon(aes(x = long, y = lat, group = group, fill = ethnicity), color = "black") + 
      scale_fill_gradientn(colours = rev(rainbow(6)),
                           breaks  = c(0, 25, 50, 75, 100)) +
      coord_fixed(ratio = 1.3, xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      # Removes the axes, lines, panels etc
      theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank()
      )
  

  })
  
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
}


# Run app -----------------------------------------------------------------

shinyApp(ui, server)