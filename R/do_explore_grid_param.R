#' Explore Hyperparameter Grid Performance
#' 
#' @aliases do_explore_grid_param
#'
#' @description 
#' Visualize the sensitivity values for the performance of the player 
#' recruitment algorithm using a grid of different combinations of
#' hyperparameters.
#' 
#' @usage 
#' do_explore_grid_param(grid_param)
#' 
#' @param grid_param Data set that includes different combinations of 
#' hyperparameters and the corresponding sensitivity values.
#' 
#' @return 
#' A plot.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' do_explore_grid_param(grid_param_fawir)
#' }                           
#' 
#' @importFrom ggplot2 geom_point facet_grid scale_color_viridis_d labeller
#'
#' @export

do_explore_grid_param <- function(grid_param){
  rank <- sensitivity <- alpha <- NULL
  
  # Ensure factors
  grid_param$rank <- factor(grid_param$rank)
  grid_param$context_window <- factor(grid_param$context_window)
  grid_param$max_co_occur <- factor(grid_param$max_co_occur)
  
  # Custom labeller function for newline in strip labels
  facet_labeller <- labeller(
    context_window = function(x) paste0("context_window:\n", x),
    max_co_occur = function(x) paste0("max_co_occur:\n", x)
  )
  
  # Find the row with the highest sensitivity
  max_point <- grid_param %>% 
    filter(sensitivity == max(sensitivity))
  
  gg <- ggplot(grid_param, aes(x = rank, y = sensitivity, color = factor(alpha), group = alpha)) +
    geom_line() +
    geom_point() +
    geom_point(data = max_point, aes(x = rank, y = sensitivity), 
               shape = 21, fill = "red", color = "black", size = 4, stroke = 1.5) +
    facet_grid(context_window ~ max_co_occur, labeller = facet_labeller) +
    scale_color_viridis_d() +
    labs(
      title = "Hyperparameter Grid Performance",
      x = "Rank Value",
      y = "Sensitivity",
      color = expression("Value of Alpha (" * alpha * ")")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
  
  return(gg)
}
