#' Player recruitment algorithm
#' 
#' @aliases do_player_recruitment
#'
#' @description 
#' Algorithm for player recruitment using both the numeric and text description
#' of the players. An input data set collects the information from a number of
#' players. The user sends a query with the characteristics of the player of
#' interest and the algorithm returns a number of potential candidates.
#' 
#' @usage 
#' do_player_recruitment(data, rank_value = 50, skip_grams_window_value = 5, 
#'                       x_max_value = 10, alpha = 0.7, top_simil = 3, 
#'                       max_scaling = FALSE, query)
#' 
#' @param data Input data set with the numeric and text variables from a number of players. 
#' It must contain a column called scouting_report with the text descriptions.
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
#' @param query String with the description of a type of player of interest.
#' 
#' @return 
#' A data frame.
#' 
#' @author 
#' Guillermo Vinue with the support of ChatGPT.
#' 
#' @examples 
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#' 
#' # Prepare the scouting report:
#' df0 <- pesmaster_sample %>% 
#'   select(-skills, -playing_style_ai) %>%
#'   unite("scouting_report", c(characteristics, skills_descr, playing_style_ai_descr), sep = ", ") %>%
#'   mutate(scouting_report = gsub(", NA", "", scouting_report)) 
#' 
#' # Define the type of player to search for:
#' query <- "A predatory striker, attacking of 80, kicking power of 72, dribbling of 67"
#' 
#' # Run the algorithm:
#' res <- do_player_recruitment(df0, 50, 5, 10, 0.5, 3, TRUE, query)
#' res %>% select(league, team, name, position, playing_style)
#' }                           
#' 
#' @importFrom dplyr desc top_n
#' @importFrom RcppParallel setThreadOptions
#' @importFrom stats sd
#' @importFrom text2vec itoken word_tokenizer create_vocabulary vocab_vectorizer create_tcm GloVe GlobalVectors
#' @importFrom utils capture.output
#'
#' @export

do_player_recruitment <- function(data, rank_value = 50, skip_grams_window_value = 5, x_max_value = 10, 
                                  alpha = 0.7, top_simil = 3, max_scaling = FALSE, query) {
  text_similarity <- final_similarity <- NULL
  
  # Text data handling:
  reports <- data$scouting_report
  
  # Text processing setup:
  it <- itoken(reports, tolower, word_tokenizer)
  # U+2019 is a smart quote, whereas U+0027 is a straight quote.
  vocab <- create_vocabulary(it, stopwords = c(tidytext::stop_words$word, 
                                               "team\u2019s", "player\u2019s", "plaver\u2019s"))
  vectorizer <- vocab_vectorizer(vocab)
  tcm <- create_tcm(it, vectorizer, skip_grams_window = skip_grams_window_value)
  
  if (max_scaling) {
    tcm <- tcm / max(tcm)
  }
  
  # GloVe algorithm:
  # To avoid reproducibility:
  setThreadOptions(numThreads = 1)
  set.seed(42)
  glove_model <- GlobalVectors$new(rank = rank_value, x_max = x_max_value)
  # To avoid verbose:
  invisible(capture.output({word_vectors <- glove_model$fit_transform(tcm, n_threads = 1)}))
    
  # Compute text embeddings:
  text_embedding <- t(sapply(reports, function(text) get_text_embedding(text, word_vectors)))
  text_embedding_data <- split(text_embedding, row(text_embedding))
  
  # Text query handling:
  # Compute text embeddings:
  query_embedding <- get_text_embedding(query, word_vectors)
  
  # Compute the similarity between data texts and query texts:
  if (all(query_embedding == 0)) {
    data$text_similarity <- 0
  }else{
    data$text_similarity <- sapply(text_embedding_data, cosine_similarity, query_embedding)
    data$text_similarity <- round(data$text_similarity, 3) 
  }

  # --
  
  # Numbers handling:
  numeric_features <- data %>% select(where(is.numeric), -text_similarity)
  cols_num <- gsub("_", " ", colnames(numeric_features))
  
  # Obtain the numbers related to the abilities:
  numeric_constraints <- parse_all_numeric_constraints(query, cols_num)
  if (is.list(numeric_constraints)) {
    numeric_constraints[lengths(numeric_constraints) == 0] <- NA
    numeric_constraints <- unlist(numeric_constraints)
  }
  names(numeric_constraints) <- colnames(numeric_features)
  
  # Compute means and standard deviations of numeric features:
  feature_means <- colMeans(numeric_features)
  feature_sds <- apply(numeric_features, 2, sd)
  
  # Normalize query constraints using the parameters from the reports:
  numeric_constraints_normalized <- normalize_query(numeric_constraints, feature_means, feature_sds)
  
  # Normalize numeric features:
  normalized_numeric <- scale(numeric_features)
  
  # Compute numeric similarity:
  # The following apply() runs numeric_similarity() once per row of normalized_numeric.
  # This is the same as running a loop for 1 to the number of rows of normalized_numeric,
  # where in each iteration i, we run: 
  # numeric_similarity(numeric_constraints_normalized, 
  #                    normalized_numeric[i,], 
  #                    rep(1, ncol(numeric_features)))
  data$numeric_similarity <- apply(normalized_numeric, 
                                   1, 
                                   numeric_similarity, 
                                   constraints = numeric_constraints_normalized, 
                                   scale_factors = rep(1, ncol(numeric_features))) 
  data$numeric_similarity <- round(data$numeric_similarity, 3)
  
  data$final_similarity <- alpha * data$text_similarity + (1 - alpha) * data$numeric_similarity
  data$final_similarity <- round(data$final_similarity, 3)
  
  # Rank players by combined similarity
  results <- data %>%
    arrange(desc(final_similarity)) %>%
    top_n(n = top_simil, wt = final_similarity)

  return(results)
}
