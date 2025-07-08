#' Check the definition of PES Master playing styles
#' 
#' @aliases do_check_def_playing_styles
#'
#' @description 
#' This is an experiment to check how the PES Master playing styles are defined.
#' The experiment consists of sending a query with the playing style text
#' description and the corresponding average numeric variables to the player
#' recruiting algorithm and check how many times the target playing style is
#' returned.
#' 
#' @usage 
#' do_check_def_playing_styles(pesmaster_playing_styles, pesmaster_data, 
#'                             pesmaster_algorithm, type_player, rank_value = 50, 
#'                             skip_grams_window_value = 5, x_max_value = 10, 
#'                             alpha = 0.7, top_simil = 3, max_scaling = FALSE,
#'                             verbose)
#' 
#' @param pesmaster_playing_styles PES Master playing styles.
#' @param pesmaster_data PES Master data.
#' @param pesmaster_algorithm Average values for all the variables. This is computed
#' with \code{do_prepare_profiles}.
#' @param type_player String with the position. Options are Goalkeeper, Defender, Midfielder and Forward.
#' @param rank_value Number to indicate the desired dimension for the latent vectors. Default 50.
#' @param skip_grams_window_value Number to indicate the window for term-co-occurrence 
#' matrix construction. Default 5.
#' @param x_max_value Number to indicate the maximum number of co-occurrences to use in the 
#' weighting function. Default 10.
#' @param alpha Number between 0 and 1 to combine text-based cosine similarity 
#' and numeric similarity into a final similarity score. The closer to 0, the more important 
#' are the numeric variables. Default 0.7.
#' @param top_simil Number of potential candidates to return.
#' @param max_scaling Logical to indicate if the term-co-occurrence matrix must be scaled. 
#' This is suggested when there are words that are repeated in most documents. Default FALSE.
#' @param verbose Should R report information on progress? TRUE or FALSE.
#' 
#' @return 
#' A plot with a kind of confusion matrix.
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
#' do_check_def_playing_styles(pesmaster_playing_styles, pesmaster_sample, pesmaster_algorithm, 
#'                             c("Defensive Goalkeeper", "Offensive Goalkeeper"), 50, 5, 10, 
#'                             0.5, 10, TRUE, TRUE)
#' }                           
#'
#' @importFrom ggplot2 scale_colour_gradientn scale_y_discrete geom_text theme_minimal
#' @importFrom scales rescale
#'
#' @export

do_check_def_playing_styles <- function(pesmaster_playing_styles, pesmaster_data, pesmaster_algorithm, 
                                        type_player, rank_value = 50, skip_grams_window_value = 5, 
                                        x_max_value = 10, alpha = 0.7, top_simil = 3, max_scaling = FALSE,
                                        verbose){
  playing_style <- description <- skills <- playing_style_ai <- NULL
  characteristics <- skills_descr <- playing_style_ai_descr <- NULL
  scouting_report <- position <- goalkeeping <- value <- ability <- NULL
  text <- pred <- type <- playing_style <- obs <- NULL
  
  # Create confusion matrix:
  play_style <- type_player
  conf_mat <- matrix(NA, nrow = length(play_style), ncol = length(play_style))
  rownames(conf_mat) <- play_style
  colnames(conf_mat) <- play_style
  
  # Get predictions:
  conf_df <- data.frame()
  for (i in 1:length(play_style)) {
    if (verbose) {
      cat("ITERATION:", i, "\n")
      cat("TYPE OF PLAYER:", play_style[i], "\n") 
    }
    
    type_player <- play_style[i]
    
    type_player_descr <- pesmaster_playing_styles %>%
      filter(playing_style == type_player) %>%
      pull(description)
    
    # Create data for the algorithm:
    df1 <- pesmaster_data %>%
      select(-skills, -playing_style_ai) %>%
      unite("scouting_report", c(characteristics, skills_descr, playing_style_ai_descr), sep = ", ") %>%
      mutate(scouting_report = gsub(", NA", "", scouting_report)) 
    
    if (grepl("Goalkeeper", type_player)) {
      players <- df1 %>%
        filter(position == "GK")
    }else{
      players <- df1 %>%
        filter(position != "GK") %>%
        select(-contains("gk_"), -goalkeeping) 
    }
    
    prof_avg_ps <- pesmaster_algorithm %>%
      filter(playing_style == type_player) %>%
      mutate(value = round(value, 0)) %>%
      unite("text", c(ability, value), sep = " of ") %>%
      mutate(text = gsub("_", " ", text)) %>%
      pull(text)
    
    prof_avg_ps_txt <- paste0(prof_avg_ps, collapse = ", ")
    query <- paste0(type_player_descr, ", ", prof_avg_ps_txt)
    
    res_recr <- do_player_recruitment(players, rank_value, skip_grams_window_value, 
                                      x_max_value, alpha, top_simil, max_scaling, query) 
    
    preds <- table(res_recr$playing_style)
    
    conf_mat[i, match(names(preds), colnames(conf_mat))] <- preds 
    
    conf_df_iter <- data.frame(obs = type_player, pred = names(conf_mat[i,]), value = conf_mat[i,])
    rownames(conf_df_iter) <- NULL
    
    conf_df <- rbind(conf_df, conf_df_iter)
  }
  
  gg <- ggplot(conf_df %>% filter(!is.na(value)), aes(x = obs, y = pred, color = value)) +
    geom_text(aes(label = value), size = 6) +
    scale_y_discrete(limits = rev) +
    labs(x = "Target playing style", y = "Coincidences for the top 10 predictions", color = "") +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_colour_gradientn(limits = c(0, 10), breaks = c(0, 5, 10), labels = c(0, 5, 10),
                           colours = c("white", "grey40", "black"),  
                           values = rescale(c(0, 3, 10))) 
  
  return(gg)
}
