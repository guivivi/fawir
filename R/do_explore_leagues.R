#' Explore PES Master leagues 
#' 
#' @aliases do_explore_leagues
#'
#' @description 
#' Visualize the number of teams and players per league.
#' 
#' @usage 
#' do_explore_leagues(pesmaster_data)
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
#' do_explore_leagues(pesmaster_sample)
#' }                           
#' 
#' @importFrom dplyr group_by summarise n_distinct ungroup count left_join
#' @importFrom ggplot2 theme element_blank element_text
#' @importFrom ggpubr ggdotchart theme_pubclean
#'
#' @export

do_explore_leagues <- function(pesmaster_data){
  league <- team <- NULL
  
  df0_teams <- pesmaster_data %>%
    group_by(league) %>%
    summarise(teams = n_distinct(team)) %>%
    ungroup()
  
  df0_players <- pesmaster_data %>%
    count(league, name = "players")
  
  df0_tp <- left_join(df0_teams, df0_players, by = "league") 
  
  gg <- ggdotchart(df0_tp, 
             x = "teams", y = "players", color = "league", 
             size = 7, 
             ggtheme = theme_pubclean()) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 14),
          axis.text.x = element_text(angle = 0, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14))
  
  return(gg)
}
