#' simplify_authors
#'
#' Reduce a list of authors to et al where appropriate
#' @param x a character vector of comma separated author surnames
#' @export

simplify_authors = function(x){

  y <- strsplit(x, split = ",")
  out <- lapply(y, function(i){
    if(length(i) == 1) return(i)
    if(length(i) == 2) return(paste(i, collapse = " & "))
    return(paste(i[[1]], "et al."))
  })
  gsub("\\s\\s"," ", out)
}
