% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/health-parse-functions.R
\name{get_timestamp}
\alias{get_timestamp}
\title{Get timestamps from each line}
\usage{
get_timestamp(dat_txt)
}
\arguments{
\item{dat_txt}{Character vector that contains date, time, and one or more
key-value pairs.}
}
\value{
Timestamp of type \code{lubridate::Date}
}
\description{
Timestamps have to have the format of "2018-08-24; 17:40;"
}
\examples{
get_timestamp("2018-08-24; 17:40;")
\dontrun{
get_timestamp(dat_txt[1:2])
get_timestamp(dat_txt)
get_timestamp(remove_comments(dat_txt))
get_timestamp(dat_txt[409:414])
}
}
