plot_pca <- function(tb, x_pc = 1, y_pc = 2, plot_title, color_group = NULL) {
  
  library(tidyverse)
  library(purrr)
  library(ggfortify)
  
  # Plot the first two principal components
  p <- tb %>% 
    mutate(
      pca_graph = map2(
        .x = pca,
        .y = data,
        ~ autoplot(
          .x,
          data         = .y,
          x = x_pc,
          y = y_pc,
          colour = color_group,
          loadings     = TRUE,
          loadings.label = TRUE,
          loadings.colour = "black",
          loadings.label.colour = "black",
          label        = FALSE) +
          theme(panel.background = element_blank(),
                axis.line        = element_line(color = "black"),
                plot.title       = element_text(hjust = 0.5),
                legend.key       = element_blank()) +
          labs(x     = "Principal Component 1",
               y     = "Principal Component 2",
               title = plot_title)
      )
    ) %>%
    pull(pca_graph)
  
  # Add horizontal and vertical lines to the plot
  p1 <- p[[1]] + # p[[1]] is used to get the "pure" plot
    geom_hline(yintercept = 0,
               color = "grey",
               linetype = "dashed") +
    geom_vline(xintercept = 0,
               color = "grey",
               linetype = "dashed")
  
  # Reverse the layers to plot lines beneath the scatterplot
  p1$layers <- rev(p1$layers)
  
  # Plot
  p1
}
