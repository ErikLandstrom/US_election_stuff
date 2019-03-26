### plot_scatterplot
### Author: Erik Ländström
### Date: 181220


# Description -------------------------------------------------------------

# Plots a scatterplot, with geom smoot being optional.


# Arguments ---------------------------------------------------------------

# data = tibble,
# column1 = column1
# column2 = column2
# ylabel = character, y label
# xlabel = character, x label
# title_string = option, plot title
# subtitle_string = option, plot subtitle
# caption title = optional, plot caption
# smooth = logical, defaults to FALSE. If TRUE plots a linear smoothing curve.


# Function ----------------------------------------------------------------

plot_scatterplot <- function(data, column1, column2, color_column = NULL,
                             y_label, x_label, 
                             title_string = "Scatterplot", 
                             subtitle_string = NULL, caption_string = NULL, 
                             smooth = FALSE) {
  # Library
  library(tidyverse)
  
  # Quote column names
  column1 <- enquo(column1)
  column2 <- enquo(column2)
  color_column <- enquo(color_column)
  
  # Scatter plot
  plot <- data %>%
    ggplot(aes(!!column1, !!column2, color = !!color_column)) +
    geom_point() +
    xlab(x_label) +
    ylab(y_label) +
    labs(
      title = title_string,
      subtitle = subtitle_string,
      caption = caption_string
    ) +
    theme(
      panel.background = element_blank(),
      axis.line = element_line(color = "black")
    )
  
  # Plots smoothing curve if smooth = TRUE
  if (smooth == TRUE) {
    plot +
      geom_smooth(method = "lm", color = "red", se = FALSE)
  }
  else {
    plot
  }
} 
