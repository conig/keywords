#' simplify_authors
#'
#' Reduce a list of authors to et al where appropriate
#' @param x a character vector of comma separated author surnames
#' @param split regex rules by which to split up authors.
#' @export

simplify_authors = function(x, split = ",|&" ){

  y <- strsplit(x, split = split)
  out <- lapply(y, function(i){
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
