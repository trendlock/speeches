% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean words.R, R/turnbull add speaker.R
\name{clean_text}
\alias{clean_text}
\alias{clean_text}
\alias{add_speaker}
\title{Clean words}
\usage{
clean_text(df)

add_speaker(df, keep = T)
}
\arguments{
\item{df}{Takes df with}

\item{keep}{Defaults to TRUE, if FALSE filters out "jorn"...}

\item{df}{Takes df with sentence level text.}
}
\value{
Returns data frame

Returns data frame
}
\description{
Removes numeric characters, stop words and white space from words...

Mutates variable \code{"speaker."} to either \code{"jorn"} or \code{"pm"}.
Also removes the sentences that are \code{"JOURNALIST:"} and \code{"PRIME MINISTER:"}.
}
