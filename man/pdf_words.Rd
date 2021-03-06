% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_words.R
\name{pdf_words}
\alias{pdf_words}
\title{pdf_words}
\usage{
pdf_words(
  folder,
  subfolders = T,
  multiprocess = T,
  unnest_tokens = FALSE,
  exclude = NULL
)
}
\arguments{
\item{folder}{the path to a folder}

\item{subfolders}{a bool. if true, subfolders included}

\item{multiprocess}{if more than 100 files requested and if this argument is TRUE, multiple R sessions will be used to speed up reading}

\item{unnest_tokens}{a bool. If true, a dataset split into one word per row is returned.}

\item{exclude}{regex for removing files}
}
\description{
Extracts all words from PDFs
}
