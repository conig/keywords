#' pdf_words
#'
#' Extracts all words from PDFs
#' @param folder the path to a folder
#' @param subfolders a bool. if true, subfolders included
#' @param unnest_tokens a bool. If true, a dataset split into one word per row is returned.
#' @param exclude words to exclude

pdf_words = function(folder, subfolders = T, unnest_tokens = FALSE, exclude = c()){

  file_names = list.files(
    folder,
    pattern = ".pdf",
    recursive = subfolders,
    full.names = F
  )
  file_paths = list.files(
    folder,
    pattern = ".pdf",
    recursive = subfolders,
    full.names = T
  )

  if(length(file_names) == 0 & tools::file_ext(tolower(folder)) == "pdf"){
    file_names = basename(folder)
    file_paths = folder
  }

  if(length(file_names) == 0){
    stop("There aren't any .pdfs in that folder.", call. = F)
  }

  corpus = lapply(seq_along(file_names), function(x) {
    out = tibble::tibble(path = file_paths[x], content = getPdf(file_paths[x]))

    return(out)
  })


  corpus = do.call(rbind, corpus)
  if(!unnest_tokens) return(corpus)
  stop_words = tibble::tibble(word = tm::stopwords(), lexicon = "NA")

  errors = corpus[is.na(corpus$content), "path"]
  corpus = stats::na.omit(corpus)
  if (nrow(corpus) == 0) {
    stop("No pdf files could be read. Try different ones.")
  }
  backup = corpus
  message("cleaning...")
  suppressMessages(corpus <- tidytext::unnest_tokens(corpus, word, content))
  corpus$word = tolower(corpus$word)
  corpus$word = gsub("[[:digit:]]", "", corpus$word)
  corpus$word = gsub("[[:punct:]]", "", corpus$word)
  suppressMessages(
    corpus <- corpus %>%
      dplyr::filter(nchar(word) > 2) %>%
      dplyr::filter(!word %in% exclude) %>%
      dplyr::anti_join(stop_words)
  )

  return(corpus)

}

#' getPdf
#'
#' Safely imports a pdf
#' @param filename path

getPdf = function(filename) {
  out = NA
  tryCatch({
    suppressMessages(out <-
                       filename %>%
                       pdftools::pdf_text() %>%
                       tm::stripWhitespace() %>%
                       paste(collapse = " "))
  }, error = function(e) {

  })
  return(out)
}
