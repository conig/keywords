#' extract_difference.ris
#'
#' Extracts all references in the first argument, not in the second, then outputs them to the third.
#' @param ris1 path to the ris file to extract references not in the second
#' @param ris2 path to the reference ris file
#' @param output path to the output ris file
#' @param threshold scalar, when distances between references are bigger than this, they are retained
#' @export

extract_difference.ris = function(ris1, ris2, output, threshold = 3){
  # load ris in
  ris1 = revtools::read_bibliography(ris1, return_df = FALSE)
  ris2 = revtools::read_bibliography(ris2, return_df = FALSE)

  ris1_df = data.frame(ris1)
  ris2_df = data.frame(ris2)

  remove_punct <- function(x) gsub( "[^0-9 A-z]","",x)

  ris1_df$info = with(ris1_df, paste(title, substring(journal, 1,1), substring(year,3,4),
                                     substring(author,1,3))) %>% remove_punct()
  ris2_df$info = with(ris2_df, paste(title, substring(journal, 1,1), substring(year,3,4),
                                     substring(author,1,3))) %>% remove_punct()

  pb <- utils::txtProgressBar(min = 1, max = length(ris1_df$info), style = 3)

  distance = function(x, i){
    utils::setTxtProgressBar(pb, i)
    min(stringdist::stringdist(tolower(x), tolower(ris2_df$info)), na.rm = TRUE)
  }

  ris1_df$dist <- sapply(seq_along(ris1_df$info), function(x) distance(ris1_df$info[x], x))

  keep <- ris1_df$dist > threshold

  i <- ris1[which(keep)]

  revtools::write_bibliography(i, output)

}

