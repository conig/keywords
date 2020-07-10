#' keywords
#'
#' get keywords
#' @param path path to a PDF or folder containing PDFs
#' @param n the number of keywords per topic
#' @param topics the number of topics
#' @param corpus You can provide a vector of
#' @param ... additional arguments sent to pdf_words
#' @export keywords

keywords = function(path = NULL, n = 5, topics = 2, corpus = NULL, ...){

  if(is.null(corpus)) {
    raw <- pdf_words(path, ...)
    corpus = raw$content
  }


  corpus <- tm::VCorpus(tm::VectorSource(corpus)) %>%
    tm::tm_map(tm::content_transformer(tolower)) %>%
    tm::tm_map(tm::content_transformer(tm::removeNumbers)) %>%
    tm::tm_map(tm::content_transformer(tm::removePunctuation)) %>%
    tm::tm_map(tm::content_transformer(tm::removeWords),
               c("the", "and", tm::stopwords("english"))) %>%
    tm::tm_map(tm::content_transformer(tm::stripWhitespace))

  dtm <- tm::DocumentTermMatrix(corpus)
  dtm <- tm::removeSparseTerms(dtm, .99)

  top <- ifelse(topics < 2, 2, topics)

  dtm = dtm[unique(dtm$i),]

  terms <- topicmodels::LDA(dtm, k = top)
  terms <- suppressWarnings(tidytext::tidy(terms, matrix = "beta", log = FALSE)) %>%
    dplyr::arrange(dplyr::desc(beta)) %>%
    dplyr::group_by(topic) %>%
    dplyr::top_n(n, beta)

  keywords <- lapply(unique(terms$topic), function(x) unname(unlist(terms[terms$topic == x, "term"])))
  names(keywords) = paste0("topic",1:length(keywords))
  keywords[1:topics]
}

utils::globalVariables(c("topic","word","content"))

