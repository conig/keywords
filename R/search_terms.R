#' search_terms
#'
#' Constucts search terms
#'
#' @param ... a series of named vectors. They will be joined within by OR and between by AND
#' @param db a string for the database you want to generate terms for
#' @details Indicate adjacency using word1 w\\n word2 e.g. ('search w\\2 terms)' will return records where 'search' is within two words of 'terms'. This syntax will be modified for each database.
#' @export search_terms

search_terms = function(...,
                        db = c("scopus" ,"ovid", "ebscohost", "webofscience")) {
  terms <- list(...)

  or <- lapply(terms, function(x) {
    paste0("(", paste(x, collapse = " OR "), ")")
  })

  and <- paste(or, collapse = " AND ")

  scopus = glue::glue('TITLE-ABS-KEY({and})')
  ovid = gsub("(w/)", "adj", glue::glue('({and}).ab,kw,ti'))
  ebscohost = gsub("(w/)", "N", glue::glue('({and})'))
  webofscience = gsub("(w/)", "NEAR/", glue::glue('TS=({and})'))
  out <- list(
    scopus = scopus,
    ovid = ovid,
    ebscohost =  ebscohost,
    webofscience = webofscience
  )

  out[db]

}
