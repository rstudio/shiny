alpha_val <- 0.2

scatterPlot <- function(data, cols) {
  ggplot(data, aes_string(x = cols[1], y = cols[2])) +
    geom_point(aes(color = selected_), alpha = alpha_val) +
    scale_color_manual(values = c("black", "#66D65C"), guide = FALSE)
}
