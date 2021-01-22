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
