% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/bedtoolsMulticov.R
\name{bedtoolsMulticov}
\alias{bedtoolsMulticov}
\title{Calculate coverage of multiple bams files over intervals in a single bed file.}
\usage{
bedtoolsMulticov(bamFiles, bedfile, outname, split = FALSE,
  samestrand = FALSE, diffstrand = FALSE, q = 10, duplicates = FALSE,
  onlyPaired = FALSE)
}
\arguments{
\item{bamFiles}{A character vector of paths to bam files.}

\item{bedfile}{A string specifying the path to the bed file within which to calculate coverage of bams.}

\item{outname}{A character vector of length one specifying the concatenated file name.}

\item{split}{Boolean.  If TRUE, “split” BAMs or BED12 entries will be treated as distinct BED intervals.}

\item{samestrand}{Boolean. If TRUE, same strandedness is required for a read to be counted. That is, only report hits in B that overlap A on the same strand. If FALSE, overlaps are reported without respect to strand. Conflicts with diffstrand.}

\item{diffstrand}{Boolean. If TRUE, different strandedness is required for a read to be counted. That is, only report hits in B that overlap A on the opposite strand. If FALSE, overlaps are reported without respect to strand. Conflicts with samestrand.}

\item{q}{A positive integer specifying the minimum quality of a read for it to be counted.}

\item{duplicates}{Boolean. If TRUE, include duplicate reads. Default (FALSE) counts non-duplicates only.}

\item{onlyPaired.}{Boolean. If TRUE, only properly-paired reads will be counted. Default counts all alignments with MAPQ > q, regardless of the BAM FLAG field.}
}
\description{
\code{bedtools.multicov} is a wrapper for bedtools multicov.
}

