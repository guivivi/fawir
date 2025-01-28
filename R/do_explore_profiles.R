#' Explore PES Master player profiles
#' 
#' @aliases do_explore_profiles
#'
#' @description 
#' Visualize the average values of each variable for a certain type of player.
#' This is an interesting way to establish player profiles.
#' 
#' @usage 
#' do_explore_profiles(pes_algorithm, type_player)
#' 
#' @param pes_algorithm Average values for all the variables. This is computed
#' with \code{do_prepare_profiles}.
#' @param type_player String with the position. Options are Goalkeeper,
#' Defender, Midfielder and Forward.
#' 
#' @return 
#' A plot.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' # Prepare the data with the abilities related to each position:
#' pesmaster_pos_abi <- data.frame(type = rep("Goalkeeper", 6),
#'                                 ability = c("goalkeeping", "gk_awareness", "gk_catching", 
#'                                             "gk_parrying", "gk_reflexes", "gk_reach" ))
#'                                             
#' pesmaster_algorithm <- do_prepare_profiles(pesmaster_sample, 
#'                                            pesmaster_playing_styles, 
#'                                            pesmaster_pos_abi)                                             
#'                                             
#' do_explore_profiles(pesmaster_algorithm, "Goalkeeper")
#' }                           
#' 
#' @importFrom dplyr first
#' @importFrom ggplot2 scale_y_continuous annotate theme_classic scale_color_manual aes geom_line geom_vline unit
#' @importFrom ggtext element_markdown
#'
#' @export

do_explore_profiles <- function(pes_algorithm, type_player) {
  ability <- value <- type <- playing_style <- NULL
  
  data1 <- pes_algorithm %>%
    filter(type == type_player)
  data1$ability <- factor(data1$ability, levels = unique(data1$ability))
  
  # Get starting points for each category:
  start_points <- data1 %>%
    group_by(playing_style) %>%
    filter(ability == first(ability)) %>%
    ungroup() %>%
    mutate(ability = 0)
  
  if (type_player == "Goalkeeper") {
    gg <- ggplot(data1, aes(x = ability, y = value, group = playing_style, color = playing_style)) +
      geom_line() +
      scale_y_continuous(breaks = seq(40, 100, 10)) +
      labs(x = "", y = "", color = "GOALKEEPERS") +
      annotate("rect", xmin = 0, xmax = Inf, ymin = 40, ymax = 70, fill = "red", alpha = 0.2) + 
      annotate("rect", xmin = 0, xmax = Inf, ymin = 70, ymax = 80, fill = "orange", alpha = 0.5) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = 80, ymax = 90, fill = "green", alpha = 0.5) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = 90, ymax = 100, fill = "blue", alpha = 0.5) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90),
            axis.text = element_text(size = 10),
            legend.text = element_text(size = 14),
            legend.key.spacing.y = unit(0.3, "cm")) +
      scale_color_manual(values = c("#00FFFF", "#FF00FF"))
  }else if (type_player == "Defender") {
    colors_categs <- c("red", rep("black", 6), 
                       "red", rep("black", 4), 
                       "red", rep("black", 4))
    
    gg <- ggplot(data1, aes(x = ability, y = value, group = playing_style, color = playing_style)) +
      geom_line() +
      scale_y_continuous(breaks = seq(40, 100, 10)) +
      labs(x = "", y = "", color = "DEFENDERS") +
      geom_vline(xintercept = c(7.5, 12.5), linetype = "dashed", alpha = 0.9) +                 
      annotate("rect", xmin = 0, xmax = Inf, ymin = 40, ymax = 70, fill = "red", alpha = 0.2) + 
      annotate("rect", xmin = 0, xmax = Inf, ymin = 70, ymax = 80, fill = "orange", alpha = 0.5) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = 80, ymax = 90, fill = "green", alpha = 0.5) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = 90, ymax = 100, fill = "blue", alpha = 0.5) +
      theme_classic() +
      theme(axis.text.x = element_markdown(angle = 90, colour = colors_categs),
            axis.text = element_text(size = 10),
            legend.text = element_text(size = 14),
            legend.key.spacing.y = unit(0.3, "cm")) +
      scale_color_manual(values = c("#00FFFF", "#2CA02C", "#FF00FF", "#000000"))
  }else if (type_player == "Midfielder") {
    colors_categs <- c("red", rep("black", 3), 
                       "red", rep("black", 3), 
                       "red", rep("black", 6), 
                       "red", rep("black", 4),
                       "red", rep("black", 4))
    
    gg <- ggplot(data1, aes(x = ability, y = value, group = playing_style, color = playing_style)) +
      geom_line() +
      scale_y_continuous(breaks = seq(40, 100, 10)) +
      labs(x = "", y = "", color = "MIDFIELDERS") +
      geom_vline(xintercept = c(4.5, 8.5, 15.5, 20.5), linetype = "dashed", alpha = 0.9) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = 40, ymax = 70, fill = "red", alpha = 0.2) +   
      annotate("rect", xmin = 0, xmax = Inf, ymin = 70, ymax = 80, fill = "orange", alpha = 0.5) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = 80, ymax = 90, fill = "green", alpha = 0.5) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = 90, ymax = 100, fill = "blue", alpha = 0.5) +
      theme_classic() +
      theme(axis.text.x = element_markdown(angle = 90, colour = colors_categs),
            axis.text = element_text(size = 10),
            legend.text = element_text(size = 14),
            legend.key.spacing.y = unit(0.3, "cm")) +
      scale_color_manual(values = c("#00FFFF", "#2CA02C", "#FF00FF", "#000000", 
                                    "#D62728", "#1F77B4", "#FFD700"))
  }else if (type_player == "Forward") {
    colors_categs <- c("red", rep("black", 3), 
                       "red", rep("black", 3), 
                       "red", rep("black", 4), 
                       "red", rep("black", 4))
    
    gg <- ggplot(data1, aes(x = ability, y = value, group = playing_style, color = playing_style)) +
      geom_line() +
      scale_y_continuous(breaks = seq(40, 100, 10)) +
      labs(x = "", y = "", color = "FORWARDS") +
      geom_vline(xintercept = c(4.5, 8.5, 13.5), linetype = "dashed", alpha = 0.9) + 
      annotate("rect", xmin = 0, xmax = Inf, ymin = 40, ymax = 70, fill = "red", alpha = 0.2) +   
      annotate("rect", xmin = 0, xmax = Inf, ymin = 70, ymax = 80, fill = "orange", alpha = 0.5) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = 80, ymax = 90, fill = "green", alpha = 0.5) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = 90, ymax = 100, fill = "blue", alpha = 0.5) +
      theme_classic() +
      theme(axis.text.x = element_markdown(angle = 90, colour = colors_categs),
            axis.text = element_text(size = 10),
            legend.text = element_text(size = 14),
            legend.key.spacing.y = unit(0.3, "cm")) +
      scale_color_manual(values = c("#00FFFF", "#2CA02C", "#FF00FF", "#000000", 
                                    "#D62728", "#1F77B4", "#FFD700", "#7F7F7F", 
                                    "#1F77B4"))
  }
  
  return(gg)
}