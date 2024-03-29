% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BNF.R
\name{rules}
\alias{rules}
\title{Returns all indices of rules applicable for a non-terminal identifier.}
\usage{
rules(Id, LHS)
}
\arguments{
\item{Id}{A numerical identifier.}

\item{LHS}{The left-hand side of a production table.}
}
\value{
\itemize{
  \item A vector of indices of all applicable rules 
        in the production table or
  \item an empty integer (\code{integer(0)}), 
        if the numerical identifier is not found 
        in the left-hand side of the production table.}
}
\description{
\code{rules()} finds 
             all applicable production rules
             for a non-terminal identifier.
}
\examples{
a<-booleanGrammar()$BNF
ST<-makeSymbolTable(a)
PT<-makeProductionTable(a,ST)
rules(5, PT$LHS)
rules(8, PT$LHS)
rules(9, PT$LHS)
rules(1, PT$LHS)

}
\seealso{
Other Utility Functions: 
\code{\link{derive}()},
\code{\link{id2symb}()},
\code{\link{isNonTerminal}()},
\code{\link{isTerminal}()},
\code{\link{symb2id}()}
}
\concept{Utility Functions}
