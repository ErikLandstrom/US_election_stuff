











plot_histogram <- function(data, column, title = "Histogram", subtitle = NULL, caption = NULL) {
  
  column <- enquo(column)
  
  vec <- as_vector(data %>%
                     select(!!column))
  
  b <- diff(range(vec, na.rm = TRUE)) / (2 * IQR(vec, na.rm = TRUE) / length(vec)^(1 / 3))
  
  data %>%
    ggplot(aes(!!column)) +
    geom_histogram(bins = b, fill = "white", color = "black") +
    xlab("Count") +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    )
}

