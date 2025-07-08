#' Scrape PES Master data 
#' 
#' @aliases do_scrape_pesmaster
#'
#' @description 
#' Obtain numeric and text descriptions from football players included in 
#' the PES Master database (\url{https://www.pesmaster.com/efootball-2022/}).
#' 
#' @usage 
#' do_scrape_pesmaster(url_player, url_pes_base, df_players_links, 
#'                     pes_skills, pes_ai, analyst_name, verbose)
#' 
#' @param url_player PES Master url for a certain player.
#' @param url_pes_base PES Master baseline url.
#' @param df_players_links Data with the details needed to scrape.  
#' @param pes_skills Data with the skills definition.
#' @param pes_ai Data with the AI playing styles.
#' @param analyst_name Name to identify the user when doing web scraping. 
#' This is a polite way to do web scraping and certify that the user 
#' is working as transparently as possible with a research purpose.
#' @param verbose Should R report information on progress? TRUE or FALSE.
#' 
#' @return 
#' A data frame with the player data.
#' 
#' @note 
#' This function is devoted to friendly web scraping. It respects the 
#' information provided by the PES Master website:
#' \url{https://www.pesmaster.com/robots.txt} and 
#' \url{https://www.pesmaster.com/privacy-policy/}
#' 
#' Please be polite and respectful when doing web scraping.
#' Also include the command \code{Sys.sleep(x)} to pause between 
#' requests for x seconds if data are obtained iteratively. 
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' library(dplyr)
#' 
#' # Prepare the data with the details needed to scrape:
#' df_players_links <- data.frame(league = "Premier League",
#'                                team = "Arsenal",
#'                                position = "CB",
#'                                name = "William Saliba",
#'                                link = "/william-saliba/efootball-2022/player/126689/")
#'                                
#' # Prepare the data with the skills definition:                                
#' pes_skills <- data.frame(english = c("Heading", "Weighted Pass", "Man Marking", 
#'                                      "Interception", "Blocker", "Aerial Superiority", 
#'                                      "Acrobatic Clearance"),
#'                          description = c("Improve the accuracy of headers", 
#'                                          "Apply back-spin when playing lofted passes", 
#'                                          "Stick to chase down an opponent", 
#'                                          "Better interception skills", 
#'                                          "Player’s chances to block passes and shots", 
#'                                          "Player’s chances to win aerial balls", 
#'                                          "Clear the ball even from awkward positions"))                                
#' 
#' # Prepare the data with the AI playing styles:
#' pes_ai <- data.frame(english = "Long Ball Expert",
#'                      description = "A player who frequently plays the long ball")
#' 
#' url_player <- df_players_links %>% pull(link)
#' 
#' url_pes_base <- "https://www.pesmaster.com"
#' analyst_name <- "Guillermo Vinue"
#' 
#' data_pes <- do_scrape_pesmaster(url_player, url_pes_base, df_players_links, 
#'                                 pes_skills, pes_ai, analyst_name, TRUE)
#' }                           
#' 
#' @importFrom dplyr distinct pull mutate
#' @importFrom janitor clean_names
#' @importFrom magrittr "%>%"
#' @importFrom polite bow scrape
#' @importFrom robotstxt paths_allowed
#' @importFrom stringr str_match_all
#' @importFrom rvest html_nodes html_text
#' @importFrom tidyr pivot_wider unite
#'
#' @export

do_scrape_pesmaster <- function(url_player, url_pes_base, df_players_links, 
                                pes_skills, pes_ai, analyst_name, verbose){
  variable <- value <- characteristics <- skills <- skills_descr <- NULL
  playing_style_ai <- playing_style_ai_descr <- NULL
  
  url_pes_player <- paste0(url_pes_base, url_player)
  
  # Ask for permission:
  if (paths_allowed(url_pes_player)) {
    # Introduce yourself (use user-agent string): 
    session <- bow(url_pes_player, user_agent = paste0(analyst_name, ", polite R bot ", getOption("HTTPUserAgent")))
    
    if (verbose) {
      print(session)
    }
    
    stats_cont <- scrape(session) %>%
      html_nodes(xpath = './/div[@class="flex flex-wrap stats-block-container"]') %>%
      html_text() 
    
    # Numerical variables:
    aux <- gsub("\\r|\\n|\\t", "", stats_cont)
    num_val <- as.numeric(unlist(str_match_all(aux, "[0-9]+")))
    
    aux1 <- gsub("Characteristics.*", "", aux)
    aux2 <- gsub(paste0(num_val, collapse = "|"), ";", aux1)
    aux3 <- unlist(strsplit(aux2, ";"))
    aux4 <- aux3[-which(aux3 == "")]
    aux5 <- aux4[-grep("adsbygoogle", aux4)]
    
    variab <- gsub("google_language.*", "", aux5)
    
    stats_df <- data.frame(variable = variab, value = num_val) %>%
      distinct(variable, .keep_all = TRUE) 
    
    stats_df_wid <- stats_df %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      clean_names()
    
    # Text data. Characteristics:
    aux_char1 <- gsub(".*Characteristics", "", aux)
    aux_char2 <- unlist(strsplit(gsub("Weak Foot Usage|Weak Foot Acc.|Form|Injury Resistance", ";", aux_char1), ";"))
    aux_char3 <- aux_char2[-which(aux_char2 == "")]
    aux_char4 <- data.frame(variable = c("Weak Foot Usage", "Weak Foot Acc.", "Form", "Injury Resistance"),
                            value = aux_char3) %>%
      unite("characteristics", c(variable, value), sep = " is ") %>%
      pull(characteristics)
    
    charac1 <- paste0(aux_char4, collapse = ", ")
    
    # Text data. Playing style:
    charac_cont <- scrape(session) %>%
      html_nodes(xpath = './/div[@class="player-main-column"]') %>%
      html_text() 
    
    aux_sty1 <- gsub("\\r|\\n|\\t", "", charac_cont)
    aux_sty2 <- gsub("Playing Style|Skills|AI Playing Styles", "", aux_sty1) 
    
    # Skills:
    # Splitting string between capital and lowercase character:
    iter_text <- unlist(strsplit(aux_sty2[2], "(?<=[a-z])(?=[A-Z])", perl = TRUE))
    aux_sk <- paste0(iter_text, collapse = ", ")
    aux_sk_descr <- paste0(pes_skills$description[pes_skills$english %in% iter_text], collapse = ", ")
    
    # AI Playing Styles:
    iter_text <- unlist(strsplit(aux_sty2[3], "(?<=[a-z])(?=[A-Z])", perl = TRUE))
    aux_ai <- paste0(iter_text, collapse = ", ")
    aux_ai_descr <- paste0(pes_ai$description[pes_ai$english %in% iter_text], collapse = ", ")
    
    data_pes <- stats_df_wid %>%
      mutate(characteristics = charac1) %>%
      mutate(skills = aux_sk, .after = characteristics) %>%
      mutate(skills_descr = aux_sk_descr, .after = skills) %>%
      mutate(playing_style_ai = aux_ai, .after = skills_descr) %>%
      mutate(playing_style_ai_descr = aux_ai_descr, .after = playing_style_ai) %>%
      mutate(playing_style = aux_sty2[1], .after = playing_style_ai_descr) %>%
      mutate(name = df_players_links$name, .before = 1) %>%
      mutate(position = df_players_links$position, .before = 1) %>%
      mutate(team = df_players_links$team, .before = 1) %>%
      mutate(league = df_players_links$league, .before = 1)
    
    return(data_pes)
  }else{
    stop("No permission to access page")
  }
}