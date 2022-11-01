

#' prisma
#'
#' Create a PRISMA diagram for a meta-analysis

#' @param database_records n from original database
#' @param additional_records how many additional references
#' @param after_duplicates_removed how many remaining after duplicates removed?
#' @param fulltext_screened how many were full text screened
#' @param final how many were included in final extract
#' @param reasons a named list of reasons list("reason" = 0)
#' @param synthesis_description Text to describe synthesis
#' @param align 'r' (right alignment) or 'l' (left alignment)
#' @param path logical. If TRUE, diagram will be saved to image at the desired location. Ext must be pdf, svg, or png
#' @value grViz htmlwidget
#' @export

prisma = function(
  database_records = 0,
  additional_records = 0,
  after_duplicates_removed = 0,
  fulltext_screened = 0,
  final = 0,
  reasons = list("reason 1" = 0, "reason 2" = 0),
  synthesis_description = "Studies included in\nquantitative synthesis",
  synthesis_note = "",
  align = "r",
  path = NULL
){

  requireNamespace("webshot", quietly = TRUE)
  requireNamespace("DiagrammeR", quietly = TRUE)

  if(!webshot::is_phantomjs_installed()){
    warning("PhantomJS is needed to include this diagram in a pdf. Install with webshot::install_phantomjs()")
  }

  reasons_names = paste0(names(reasons), " ")

  reasons_total = sum(unlist(reasons))
  display <- function(x) format(x, big.mark = ",")
  reasons <- as.character(paste(glue::glue("{names(reasons)}: {display(unlist(reasons))}"), collapse = glue::glue("\\{align}")))


  ft_exclude = fulltext_screened - final
  abstract_screened = after_duplicates_removed
  abstract_excluded = abstract_screened - fulltext_screened

  if(ft_exclude != reasons_total) warning("The numbers of full-text excluded (", ft_exclude,
                                          ") and reasons (",reasons_total, ") do not add up to the same number")


  diagram_instructions = "

digraph G {

  graph [
    charset = 'UTF-8'
  ]

  #add node statements
  node [shape = box, width = 2.2,
         height = 1.1, fontsize = 13]

  A[label = 'Records identified\nthrough database\n searching\n(k = <[display(database_records)]>)']
  B[label = 'Additional records\nidentified through other\n sources\n(k = <[display(additional_records)]>)']
  C[label = 'Records after\nduplicates removed\n(k = <[display(after_duplicates_removed)]>)']
{rank = same; D E}
  D[label = 'Records screened\n(k = <[display(abstract_screened)]>)']
  E[label = 'Records excluded\n(k = <[display(abstract_excluded)]>)']
  F[label = 'Full-text reports\nassessed for\neligibility\n(k = <[display(fulltext_screened)]>)']
{rank = same; F G}
  G[label = 'Full-text reports excluded\n(k = <[ft_exclude]>)\n\n<[reasons]>\\<[align]>']
  H[label = '<[synthesis_description]>\n(k = <[final]>)<[display(synthesis_note)]>']

  #add edge statements
  A->C; B->C; C->D;D->F; D->E; F->G; F->H

}
                  "
diagram_instructions = as.character(glue::glue(diagram_instructions, .open = "<[", .close = "]>"))

diagram <- DiagrammeR::grViz(diagram_instructions, engine = "neato")

if(!is.null(path)){

  ext <- tolower(tools::file_ext(path))
  if(!ext %in% c("svg", "pdf", "png")){
    cli::cli_abort("path extention must be one of ['svg', 'pdf', 'png']")
  }
  temppath <- tempfile(fileext = ".svg")
  write(DiagrammeRsvg::export_svg(diagram), temppath)

  fn <- list(
    "svg" = rsvg::rsvg_svg,
    "png" = rsvg::rsvg_png,
    "pdf" = rsvg::rsvg_pdf
  )

  fn[[ext]](temppath, path)


}else{
  diagram
}


}



