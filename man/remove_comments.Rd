% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/health-parse-functions.R
\name{remove_comments}
\alias{remove_comments}
\title{Remove comments. And empty lines.}
\usage{
remove_comments(dat_txt)
}
\arguments{
\item{dat_txt}{Character vector. Usually, but not necessarily, contains
date, time, and one or more key-value pairs. Might also contain
comments after "##" characters.}
}
\value{
The same character vector, but with all comments removed.
}
\description{
Anything after a "##" is removed, including the "##".
}
\examples{
remove_comments("## this is a comment")
remove_comments("2018-08-10; 16:00; note = some text ## with some comment")
remove_comments(c(
  "## this is a comment",
  "2018-08-10; 16:00; note = some text ## with some comment")
)
}
