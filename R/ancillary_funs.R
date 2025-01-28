#' Function to compute text embeddings
#' @importFrom purrr map
#' @importFrom readr parse_number
#' @noRd
get_text_embedding <- function(text, word_vectors) {
  tokens <- word_tokenizer(text)
  
  embeddings <- map(tokens[[1]], function(word) {
    if (word %in% rownames(word_vectors)) {
      word_vectors[word, , drop = FALSE]
    } else {
      NULL
    }
  })
  
  valid_embeddings <- do.call(rbind, embeddings)
  
  if (is.null(valid_embeddings) || nrow(valid_embeddings) == 0) {
    return(rep(0, ncol(word_vectors)))
  } else {
    return(colMeans(valid_embeddings, na.rm = TRUE))
  }
}

#' Function to compute the cosine similarity
#' @noRd
cosine_similarity <- function(x, y) {
  sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}

#' Extract numeric constraints for all variables
#' @noRd
parse_all_numeric_constraints <- function(query, numeric_variables) {
  sapply(numeric_variables, function(var) {
    if (grepl(var, query, ignore.case = TRUE)) {
      matches <- regmatches(query, regexpr(paste0(var, " of ", "\\d+(\\.\\d+)?"), query, perl = TRUE))
      parse_number(matches)
    } else {
      NA
    }
  })
}

#' Normalize query constraints
#' @noRd
normalize_query <- function(query_constraints, means, sds) {
  (query_constraints - means) / sds
}

#' Compute numeric similarity for all variables
#' @noRd
numeric_similarity <- function(constraints, player_data, scale_factors) {
  similarities <- mapply(function(target, actual, scale) {
    if (is.na(target)) return(1)       # No constraint provided
    exp(-abs(target - actual) / scale) # Gaussian similarity
  }, target = constraints, actual = player_data, scale = scale_factors)
  
  mean(similarities, na.rm = TRUE)     # Aggregate similarity across variables
}
