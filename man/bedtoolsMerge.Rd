% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/bedtoolsMerge.R
\name{bedtoolsMerge}
\alias{bedtoolsMerge}
\title{Merge overlapping interavls within an individual bed file.}
\usage{
bedtoolsMerge(bedFiles, flank = 0, scorecol = 5, operation = "none",
  strand = FALSE, threads = getOption("threads", 1L))
}
\arguments{
\item{bedFiles}{A character vector of paths to bed files.}

\item{flank}{"Maximum distance between features allowed for features to be merged. Default is 0. That is, overlapping and/or book-ended features are merged."}

\item{scorecol}{Positive integer specifying the column with values for performing the specified operation.}

\item{operation}{to perform on overlapping bed intervals. Can be one of: none,sum,mean,collapse,distinct,count,count_distinct. Default is none, meaning no operation is performed. See bedtools docs for more info.}

\item{threads}{A positive integer specifying how many samples to process simultaneously.}
}
\description{
\code{bedtoolsMerge} is a wrapper for \code{bedtools merge} that merges overlapping intervals within individual bed files but NOT across bed files.
}

