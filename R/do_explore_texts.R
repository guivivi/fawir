#' Explore PES Master texts
#' 
#' @aliases do_explore_texts
#'
#' @description 
#' Visualize the words that best describe each playing position.
#' 
#' @usage 
#' do_explore_texts(pesmaster_data, pesmaster_positions, custom_stopwords)
#' 
#' @param pesmaster_data PES Master data.
#' @param pesmaster_positions PES Master playing positions.
#' @param custom_stopwords Custom stop words to add to the default lists.
#' 
#' @return 
#' A plot.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' library(tibble)
#' mystopwords <- tibble(word = c("goalkeeper", "goalkeepers", "player", "ball", "frequently"))
#' do_explore_texts(pesmaster_sample, pesmaster_positions, mystopwords)
#' }                           
#' 
#' @importFrom dplyr select anti_join filter slice_max contains summarize
#' @importFrom ggplot2 ggplot geom_col facet_wrap coord_flip labs
#' @importFrom tidyr unite
#' @importFrom tidytext unnest_tokens bind_tf_idf scale_x_reordered reorder_within
#'
#' @export

do_explore_texts <- function(pesmaster_data, pesmaster_positions, custom_stopwords){
  position <- skills_descr <- playing_style_ai_descr <- text <- word <- n <- NULL
  tf_idf <- position_expl <- NULL
  
  df1 <- pesmaster_data %>%
    select(position, contains("descr")) %>%
    unite("text", c(skills_descr, playing_style_ai_descr), sep = ", ") %>%
    mutate(text = gsub("NA, |, NA", "", text)) %>%
    distinct(text, .keep_all = TRUE)
  
  df1_w <- df1 %>%
    unnest_tokens(word, text) %>%
    count(position, word, sort = TRUE) %>%
    anti_join(tidytext::stop_words, by = "word") %>%
    anti_join(custom_stopwords, by = "word") %>%
    filter(!grepl("player|plaver", word)) # To filter player's
  
  total_w <- df1_w %>% 
    group_by(position) %>% 
    summarize(total = sum(n)) %>%
    ungroup()
  
  df1_w  <- left_join(df1_w , total_w, by = "position")
  
  df1_w_tf_idf <- df1_w %>%
    bind_tf_idf(word, position, n)
  
  df1_w1 <- left_join(df1_w_tf_idf, pesmaster_positions, by = "position")
  
  df1_w2 <- df1_w1 %>%
    group_by(position) %>%
    slice_max(tf_idf, n = 3) %>%
    ungroup() %>%
    mutate(position_expl = factor(position_expl, levels = pesmaster_positions$position_expl),
           word = reorder_within(word, tf_idf, position_expl))
  
  gg <- ggplot(df1_w2, aes(x = word, y = tf_idf)) +
    geom_col(fill = "white", color = "blue") +
    facet_wrap(~position_expl, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = "", y = "tf-idf") + 
    theme(strip.text = element_text(size = 9),
          axis.text.x = element_text(size = 4),
          axis.text.y = element_text(size = 12))
  
  return(gg)
}
