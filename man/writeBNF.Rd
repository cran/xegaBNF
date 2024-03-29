% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BNFfileIO.R
\name{writeBNF}
\alias{writeBNF}
\title{Write BNF into text file.}
\usage{
writeBNF(g, fn = NULL, eol = "\\n")
}
\arguments{
\item{g}{A named list with $filename and  $BNF as a character string.}

\item{fn}{A file name. Default: NULL.}

\item{eol}{End-of-line symbol(s). Default: \code{"\\n"}}
}
\value{
Invisible NULL.
}
\description{
\code{writeBNF()} writes a character string into a textfile.
}
\details{
The user writes the BNF to a text file which he edits.
         The newline symbols are inserted after each substitution variant 
         and after each production rule to improve the readability 
         of the grammar by the user.
}
\examples{
g<-booleanGrammar()
fn<-tempfile()
writeBNF(g, fn)
g1<-readBNF(fn, eol="\n")
unlink(fn)
}
\seealso{
Other File I/O: 
\code{\link{newBNF}()},
\code{\link{readBNF}()}
}
\concept{File I/O}
