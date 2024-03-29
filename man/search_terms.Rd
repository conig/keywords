% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_terms.R
\name{search_terms}
\alias{search_terms}
\title{search_terms}
\usage{
search_terms(
  ...,
  db = c("scopus", "ovid", "ebscohost", "webofscience"),
  parens = TRUE,
  term_combine = NULL
)
}
\arguments{
\item{...}{a series of named vectors. They will be joined within by OR and between by AND}

\item{db}{a string for the database you want to generate terms for}

\item{parens}{If TRUE, all terms are surrounded by parentheses}

\item{term_combine}{a named list of glue statements e.g. "({term1}) w/3 ({term2})"}
}
\description{
Constucts search terms
}
\details{
Indicate adjacency using word1 w/n word2 e.g. ('search w/2 terms)' will return records where 'search' is within two words of 'terms'. This syntax will be modified for each database.
}
