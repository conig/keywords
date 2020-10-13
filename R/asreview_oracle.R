#' asreview_oracle
#'
#' Opens asreview_oracle
#' @export

asreview <- function(){
  system("asreview oracle")
}

#' orcale.xlsx_to_ris
#'
#' Converts an xlsx to bibtex
#' @param csv path to the csv
#' @param file path to output
#' @export

oracle.xlsx_to_ris = function(csv, file){
  dat <- utils::read.csv("C:/Users/james/Desktop/export_result.csv")
  dat = dat[which(dat$final_included == 1), 2:14]
  names(dat) = c("TY","TI","T2","VL","SP","EP","PY","DO","AU","N1","M3","DB","UR")

  ris_entry = function(x) {
    x_names = names(x)[names(x) != "AU"]
    val_x = unlist(x)[names(x) != "AU"]

    authors <- trimws(unlist(strsplit(x$AU, split = "\\.,")))
    authors <-
      ifelse(!grepl("\\.$", authors), paste0(authors, "."), authors)

    x_names <-
      c(x_names[1:8], rep("AU", length(authors)), x_names[9:length(x_names)])
    val_x = c(val_x[1:8], authors , val_x[9:length(val_x)])
    paste0(paste(glue::glue("{x_names}  - {val_x}\n"), collapse = "\n"),
          "\nER  - ")
  }
  entries = unlist(lapply(seq_along(dat[, 1]), function(x)
    ris_entry(dat[x,])))
  out <- paste(entries, collapse = "\n\n")

  write(out, file)
}
