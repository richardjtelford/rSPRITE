#' @importFrom moments skewness
#' @importFrom dplyr %>% mutate group_by n
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes geom_bar labs facet_wrap
#' @importFrom rlang .data

# Build a grid containing all the results charts.
rSprite.buildCharts <- function (sample, scaleMin, scaleMax, gridSize) {
  rows <- sample$rows
  
  df <- rows %>% as.data.frame() %>% 
    mutate(ID = 1:n()) %>% 
    pivot_longer(-.data$ID) %>% 
    group_by(.data$ID) %>% 
    mutate(sk = skewness(.data$value),
           ID = factor(.data$ID), 
           ID = fct_reorder(.data$ID, .data$sk))

  plot <- ggplot(df, aes(x = .data$value)) +
    geom_bar(fill="#0099ff", width=0.9) +
    labs(title = sample$label, x = "response", y = "count") +
    facet_wrap(~ .data$ID)
  print(plot)

}
