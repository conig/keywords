

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
#' @export

prisma = function(
  database_records = 0,
  additional_records = 0,
  after_duplicates_removed = 0,
  fulltext_screened = 0,
  final = 0,
  reasons = list("reason1" = 0, "reason2" = 0),
  synthesis_description = "Studies included in\nquantitative synthesis",
  align = "r"
){

  requireNamespace("webshot", quietly = TRUE)
  requireNamespace("DiagrammeR", quietly = TRUE)

  if(!webshot::is_phantomjs_installed()){
    warning("PhantomJS is needed to include this diagram in a pdf. Install with webshot::install_phantomjs()")
  }

  reasons_names = paste0(names(reasons), " ")

  reasons_total = sum(unlist(reasons))
  reasons <- as.character(paste(glue::glue("{names(reasons)}: {unlist(reasons)}"), collapse = glue::glue("\\{align}")))


  ft_exclude = fulltext_screened - final
  abstract_screened = after_duplicates_removed
  abstract_excluded = abstract_screened - fulltext_screened

  if(ft_exclude != reasons_total) warning("The numbers of full-text excluded (", ft_exclude,
                                          ") and reasons (",reasons_total, ") do not add up to the same number")

  diagram_instructions = "

digraph boxes_and_circles {
  #graph statement
  graph [overlap = true]

  #add node statements
  node [shape = box]

  A[label = 'Records identified\nthrough database\n searching\n(n = <[database_records]>)']
  B[label = 'Additional records\nidentified through other\n sources\n(n = <[additional_records]>)']
  C[label = 'Records after\nduplicates removed\n(n = <[after_duplicates_removed]>)']
{rank = same; D E}
  D[label = 'Records screened\n(n = <[abstract_screened]>)']
  E[label = 'Records excluded\n(n = <[abstract_excluded]>)']
  F[label = 'Full-text articles\nassessed for\neligibility\n(n = <[fulltext_screened]>)']
{rank = same; F G}
  G[label = 'Full-text articles excluded\n(n = <[ft_exclude]>)\n\n<[reasons]>\\<[align]>']
  H[label = '<[synthesis_description]>\n(n = <[final]>)']

  #add edge statements
  A->C; B->C; C->D;D->F; D->E; F->G; F->H

}
                  "
diagram_instructions = as.character(glue::glue(diagram_instructions, .open = "<[", .close = "]>"))
DiagrammeR::grViz(diagram_instructions,
                  height = 1300, engine = "neato")

}



