#' asreview_oracle
#'
#' Opens asreview_oracle
#' @export

asreview <- function(){
  system("asreview oracle")
}

#' oracle.csv_to_ris
#'
#' Converts an xlsx to bibtex
#' @param csv path to the csv
#' @param file path to output
#' @export

asreview.csv_to_ris = function(csv, file){
  dat <- utils::read.csv(csv)
  dat = dat[which(dat$final_included == 1),]
  dat <- dplyr::select(dat,
    TY = "type_of_reference",
    N2 = "abstract",
    T1 = "title",
    T2 = "short_title",
    VL = "volume",
    SP = "start_page",
    EP = "start_page",
    PY = "year",
    DO = "doi",
    N1 = "notes",
    AU = "authors")

    dat$SP = gsub("-.*","",dat$SP)
    dat$EP = gsub(".*-","",dat$EP)

  ris_entry = function(x) {
    x_names = names(x)[names(x) != "AU"]
    val_x = unlist(x)[names(x) != "AU"]

    authors <- trimws(unlist(strsplit(x$AU, split = "\\.,")))
    authors <-
      ifelse(!grepl("\\.$", authors), paste0(authors, "."), authors)

    x_names <-
      c(x_names[1:5], rep("AU", length(authors)), x_names[5:length(x_names)])
    val_x = c(val_x[1:5], authors , val_x[5:length(val_x)])
    paste0(paste(glue::glue("{x_names}  - {val_x}\n"), collapse = "\n"),
          "\nER  - ")
  }
  entries = unlist(lapply(seq_along(dat[, 1]), function(x)
    ris_entry(dat[x,])))
  out <- paste(entries, collapse = "\n\n")

  write(out, file)
}
