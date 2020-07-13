#' pdf_words
#'
#' Extracts all words from PDFs
#' @param folder the path to a folder
#' @param subfolders a bool. if true, subfolders included
#' @param unnest_tokens a bool. If true, a dataset split into one word per row is returned.
#' @param multiprocess if more than 100 files requested and if this argument is TRUE, multiple R sessions will be used to speed up reading
#' @param exclude regex for removing files
#' @export

pdf_words = function(folder, subfolders = T, multiprocess = T, unnest_tokens = FALSE, exclude = NULL){

  file_paths = list.files(
    folder,
    pattern = ".pdf",
    recursive = subfolders,
    full.names = T
  )

  if(!is.null(exclude)){
    file_paths = file_paths[!grepl(exclude, file_paths)]
  }

  file_names = gsub(".*\\/","",file_paths)

  if(length(file_names) == 0 & tools::file_ext(tolower(folder)) == "pdf"){
    file_names = basename(folder)
    file_paths = folder
  }

  if(length(file_names) == 0){
    stop("There aren't any .pdfs in that folder.", call. = F)
  }

  if(length(file_names) > 50){
    msg = glue::glue("Reading {length(file_names)} pdfs.")
    if(multiprocess) {
      future::plan(future::multiprocess)
      msg <- paste(msg, "Using multiple R sessions to speed things up.")
    }
    message(msg)
  }

  corpus = future.apply::future_lapply(seq_along(file_names), function(x) {
    out = data.frame(path = file_paths[x], content = getPdf(file_paths[x]))
    out
  })

  corpus = do.call(rbind, corpus)

  if(!unnest_tokens) return(stats::na.omit(corpus))

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

  stats::na.omit(corpus)

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
