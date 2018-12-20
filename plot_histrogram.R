### plot_histogram
### Author: Erik Ländström
### Date: 181220


# Description -------------------------------------------------------------

# Plots a single histogram with a nice theme.

# If you want to expand the y axis, save the plot in an object and then 
# plot if again with scale_y_continous(limits = c(ymin, ymax)).


# Arguments ---------------------------------------------------------------

# data = tibble
# column = column to be plotted, as an object
# x_label = character, x label
# title_string = plot title
# subtitle_string = optional, plot subtitle
# caption_string = optional, plot caption 


# Function ----------------------------------------------------------------

plot_histogram <- function(data, column, x_label,
                           title_string = "Histogram", subtitle_string = NULL,
                           caption_string = NULL) {
  
  # Libraries
  library(tidyverse)
  
  # quote column variable
  column <- enquo(column)
  x_label <- enquo(x_label)
  
  # Create a vector of the data in the column
  vec <- as_vector(data %>%
                     select(!!column))
  
  # Calculates the optimal number of bins for one sample according to the Freedman-Diaconis rule
  b <- diff(range(vec, na.rm = TRUE)) / (2 * IQR(vec, na.rm = TRUE) / length(vec)^(1 / 3))
  
  # Plot histogram
  plot <- data %>%
    ggplot(aes(!!column)) +
    geom_histogram(bins = b, fill = "grey", color = "black") +
    xlab(!!x_label)
    ylab("Count") +
    labs(
      title = title_string,
      subtitle = subtitle_string,
      caption = caption_string
    ) +
    theme(
      panel.background = element_blank(),
      axis.line = element_line(color = "black")
    )
  
    plot +
      scale_y_continuous(expand = c(0,0),
                         limits=c(0,max(ggplot_build(plot)$data[[1]]$count)*1.1))
}

