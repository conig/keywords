#' simplify_authors
#'
#' Reduce a list of authors to et al where appropriate
#' @param x a character vector of comma separated author surnames
#' @param split regex rules by which to split up authors.
#' @export

simplify_authors = function(x, split = ",|&" ){
  y <- strsplit(x, split = split)
  out <- lapply(y, function(i){
    if(length(i) == 0) return(NA)
    i <- gsub("\\,.*","",i)
    if(length(i) == 1) return(i)
    if(length(i) == 2) return(paste(i, collapse = " & "))
    return(paste(i[[1]], "et al."))
  })
  gsub("\\s\\s"," ", out)
}

#' apply_gsub
#'
#' Batch together gsub commands
#' @param x vector of strings to modify
#' @param ... list syntax, replacement = c("words", "to", "replace")
#' @export
#' @details using NULL as a replacement string will result in NAs

apply_gsub = function(x, ...){
  instructions <- list(...)
  for(i in seq_along(instructions)){
    to_replace = names(instructions)[i]
    if(to_replace == "NULL"){
      to_replace <- NA
    }
    x <- gsub(paste(instructions[[i]], collapse = "|"), to_replace, x)
  }

  x

}

#' apply_grepl
#'
#' Batch together grepl commands
#' @param x vector of strings to modify
#' @param ... list syntax, replacement = c("words", "to", "replace")
#' @param other if TRUE, items not replaced by user commands will be replaced by "Other"
#' @param ignore.case if TRUE, case is not matched
#' @export
#' @details using NULL as a replacement string will result in NAs

apply_grepl <-
  function(x,
           ...,
           other = FALSE,
           ignore.case = TRUE,
           perl = FALSE,
           fixed = FALSE) {
    instructions <- list(...)

    for (i in seq_along(instructions)) {
      to_replace = names(instructions)[i]
      if (to_replace == "NULL") {
        to_replace <- NA
      }
      targets <- unlist(instructions[[i]])
      for (t in targets) {
        x[grepl(t,
                x,
                ignore.case = ignore.case,
                perl = perl,
                fixed = fixed)] <- to_replace
      }

    }

    if (other) {
      x[!x %in% names(instructions)] <- "Other"
    }
    x
  }
