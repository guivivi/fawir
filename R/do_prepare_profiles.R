#' Prepare PES Master player profiles
#' 
#' @aliases do_prepare_profiles
#'
#' @description 
#' Prepare the player profiles by computing the mean values of all 
#' the variables related to the same type of player.
#' 
#' @usage 
#' do_prepare_profiles(pesmaster_data, pesmaster_playing_styles, pesmaster_pos_abi)
#' 
#' @param pesmaster_data PES Master data.
#' @param pesmaster_playing_styles PES Master playing styles.
#' @param pesmaster_pos_abi PES Master abilities related to each position.
#' 
#' @return 
#' A data frame.
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
#' }                           
#' 
#' @importFrom dplyr arrange across where
#' @importFrom tidyr pivot_longer
#'
#' @export

do_prepare_profiles <- function(pesmaster_data, pesmaster_playing_styles, pesmaster_pos_abi) {
  playing_style <- type <- description <- NULL
  
  # Mean values for each numeric variable:
  profile_ps <- pesmaster_data %>%
    group_by(playing_style) %>%
    summarise(across(where(is.numeric), function(x) mean(x, na.rm = TRUE))) %>%
    ungroup() 
  
  pesmaster_playing_styles1 <- pesmaster_playing_styles %>% select(-description)
  profile_ps1 <- left_join(pesmaster_playing_styles1, profile_ps, by = "playing_style") 
  
  profile_ps_long <- profile_ps1 %>%
    pivot_longer(!c(type, playing_style), names_to = "ability", values_to = "value") 
  
  profile_ps_long1 <- left_join(pesmaster_pos_abi, profile_ps_long, by = c("type", "ability")) %>%
    arrange(playing_style)
  
  return(profile_ps_long1)
}