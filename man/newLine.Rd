% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newLine.R
\name{newLine}
\alias{newLine}
\title{Add new line at nth character}
\usage{
newLine(text, line_length = 40)
}
\arguments{
\item{text}{a character string}

\item{line_length}{a number indicating which character to add the new line}
}
\value{
a character string with new lines added at the nth character
}
\description{
This function adds a new line at the specified character of a string.
If that specified character is in the middle of a word, it will find
the next space and add a new line there.
}
\examples{
\dontrun{
newLine("This is a test to show the user how new line works.", 40)
}
}
\author{
Jaimie Harbin
}
