#' covience_convert
#'
#' Covidence convert
#' @param string String from covidence

covidence_convert <- function(string){


extract <- function(x,info){
  as.numeric(gsub("\\s.*","",info[grepl(x, info)]))
}

info <- unlist(strsplit(string, split = "\n")  )

# database_records ----------
database_records = extract("references imported", info)

# additional_records --------
additional_records = 0

# after_duplicates removed---
after_duplicates_removed <- extract("screened against title and abstract", info)

#' fulltext_screened --------
fulltext_screened <- extract("assessed for full-text", info)

# reasons --------------------------------------

reasons_start <- which(grepl("studies excluded",info))[2] + 1

reasons_end <- which(grepl("studies ongoing", info)) - 1

reasons <- info[reasons_start:reasons_end]
reasons <- trimws(reasons)

values <- as.numeric(gsub("\\s.*","",reasons))
names <- trimws(gsub("[0-9]","", reasons))

reasons <- as.list(values)
names(reasons) <- names
# --------------------------

# final ---------------------
final = extract("included", info)

call <- as.call(list(database_records = database_records,
     additional_records = additional_records,
     after_duplicates_removed = after_duplicates_removed,
     fulltext_screened = fulltext_screened,
     reasons = reasons,
     final = final

     ))

call[[1]] <- quote(keywords::prisma)
call
}
