#' search_terms
#'
#' Constucts search terms
#'
#' @param ... a series of named vectors. They will be joined within by OR and between by AND
#' @param db a string for the database you want to generate terms for
#' @param parens If TRUE, all terms are surrounded by parentheses
#' @param term_combine a named list of glue statements e.g. "({term1}) w/3 ({term2})"
#' @details Indicate adjacency using word1 w/n word2 e.g. ('search w/2 terms)' will return records where 'search' is within two words of 'terms'. This syntax will be modified for each database.
#' @export search_terms

search_terms = function(...,
                        db = c("scopus" ,"ovid", "ebscohost", "webofscience"),
                        parens = TRUE,
                        term_combine = NULL) {
  # and <-
  #   process_searchterms(..., parens = parens, term_combine = term_combine)

  and_quoted <-
    process_searchterms(
      ...,
      parens = parens,
      quote_phrase = TRUE,
      term_combine = term_combine
    )

  scopus = glue::glue('TITLE-ABS-KEY({and_quoted})')
  ovid = gsub("(w/)", "adj", glue::glue('({and_quoted}).mp'))
  ebscohost = gsub("(w/)", "N", glue::glue('({and_quoted})'))
  webofscience = gsub("(w/)", "NEAR/", glue::glue('TS=({and_quoted})'))
  out <- list(
    scopus = scopus,
    ovid = ovid,
    ebscohost =  ebscohost,
    webofscience = webofscience
  )

  out[db]

}

# "[^w\\/\\d]+\\s[^w\\/\\d\\s]+"

process_searchterms <-
  function(...,
           parens = TRUE,
           quote_phrase = FALSE,
           term_combine) {
    terms <- list(...)


    if (quote_phrase) {
      terms <- lapply(terms, function(l) {
        sapply(l, function(x) {
          has_phrase <- grepl("[^w\\/\\d]+\\s[^w\\/\\d\\s]+", x, perl = TRUE)

          if (has_phrase) {
            phrase <-
              grep("[^w\\/\\d]+\\s[^w\\/\\d\\s]+",
                   x,
                   perl = TRUE,
                   value = TRUE)

            phrases <-
              unlist(lapply(strsplit(phrase, split = "w/\\d"), trimws))

            for (ph in phrases) {
              new_phrase <- paste0("\"", ph, "\"")
              x <- gsub(ph, new_phrase, x, fixed = TRUE)
            }

          }

          x

        })
      })

    }

    if (parens) {
      terms[] <- lapply(terms, function(i)
        paste0("(", i, ")"))
    }

    or <- lapply(terms, function(x) {
      paste0("(", paste(x, collapse = " OR "), ")")
    })


    additional <- lapply(term_combine, function(x) {
      new_term <- with(or, glue::glue(x))
      paste0("(", new_term, ")")

    })
    to_remove <- unlist(lapply(term_combine, function(x){
      regmatches(x, gregexpr("(?<=\\{).*?(?=\\})", x, perl=T))[[1]]
    }))


    for(i in seq_along(to_remove)){
      or[to_remove[[i]]] <- NULL
    }

    or <- append(or, additional)

    and <- paste(or, collapse = " AND ")
    and

  }
