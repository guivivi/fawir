#' Explore PES Master playing styles
#' 
#' @aliases do_explore_playing_styles
#'
#' @description 
#' Visualize the number of playing styles.
#' 
#' @usage 
#' do_explore_playing_styles(pesmaster_data)
#' 
#' @param pesmaster_data PES Master data.
#' 
#' @return 
#' A plot.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' do_explore_playing_styles(pesmaster_sample)
#' }                           
#' 
#' @importFrom ggplot2 theme_bw
#'
#' @export

do_explore_playing_styles <- function(pesmaster_data){
  playing_style <- NULL
  
  df0_ps <- pesmaster_data %>%
    count(playing_style) 
  
  gg <- ggdotchart(df0_ps, x = "playing_style", y = "n", 
             label = "n", label.rectangle = TRUE,
             font.label = list(size = 6, color = "black"),
             rotate = TRUE, sorting = "desc", 
             xlab = "", ylab = "", ggtheme = theme_bw()) +
    theme(axis.text = element_text(size = 10))
  
  return(gg)
}
