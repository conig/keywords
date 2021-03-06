
<!-- README.md is generated from README.Rmd. Please edit that file -->

# keywords

<!-- badges: start -->

<!-- badges: end -->

{keywords} is a package for supporting systematic reviews. Convert
asreview csv exports to ris, generate PRISMA diagrams in R, and extract
keywords from PDF.

## Installation

{keywords} isn’t yet on CRAN but you can get it from
[GitHub](https://github.com/conig/keywords) using:

``` r
# install.packages("remotes")
remotes::install_github("conig/keywords")
```

## Generating keywords

To generate keywords, you just need to provide {keywords} with the path
to a PDF or a folder of PDFs. Text must be extractable from those PDFs.

An example paper is included in this package which we will use to
generate some keywords.

``` r
library(keywords)
path_to_pdf = system.file("example.pdf", package = "keywords")
```

Now we simply provide the number of topics, and keywords we want
produced. {keywords} performs topic modelling on the input PDF(s) to
return the top words related to each topic.

``` r
set.seed(1) # to make the results reproducible

keywords(path_to_pdf,
         n = 5,
         topics = 1)
#> $topic1
#> [1] "drinking"   "indigenous" "australian" "patterns"   "alcohol"
```

The example paper was a meta-analysis of Indigenous Australian drinking
patterns.

## PRISMA diagram

To create a PRISMA diagram, you simply need to provide the number of
records at each stage of your systematic review, and provide reasons for
exclusions. Here’s an example:

``` r
prisma(
  database_records = 1250,
  additional_records = 35,
  after_duplicates_removed = 1134,
  fulltext_screened = 60,
  reasons = list(
    "No original data" = 20,
    "TL;DR" = 10,
    "Weird font" = 5,
    "Misc" = 6
  ),
  final = 19
)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="60%" />
