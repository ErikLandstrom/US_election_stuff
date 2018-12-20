### plot_histogram
### Author: Erik Ländström
### Date: 181220


# Description -------------------------------------------------------------

# Plots a single histogram with a nice theme.


# Arguments ---------------------------------------------------------------

# data = tibble
# column = column to be plotted, as an object
# title_string = plot title
# subtitle_string = optional, plot subtitle
# caption_string = optional, plot caption 

# Function ----------------------------------------------------------------

plot_histogram <- function(data, column, title_string = "Histogram", subtitle_string = NULL, caption_string = NULL) {
  
  # Libraries
  library(tidyverse)
  
  # quote column variable
  column <- enquo(column)
  
  # Create a vector of the data in the column
  vec <- as_vector(data %>%
                     select(!!column))
  
  # Calculates the optimal number of bins for one sample according to the Freedman-Diaconis rule
  b <- diff(range(vec, na.rm = TRUE)) / (2 * IQR(vec, na.rm = TRUE) / length(vec)^(1 / 3))
  
  # Plot histogram
  data %>%
    ggplot(aes(!!column)) +
    geom_histogram(bins = b, fill = "grey", color = "black") +
    ylab("Count") +
    labs(
      title = title_string,
      subtitle = sub_string,
      caption = caption_string
    ) +
    theme(
      panel.background = element_blank(),
      axis.line = element_line(color = "black")
    ) +
    scale_y_continuous(expand = c(0,0))
}

