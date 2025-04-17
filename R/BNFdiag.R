
#' Print a production table of a grammar.
#'
#' @param PT A production table of the grammar G.
#' @param G  A grammar object
#' @param verbose Print production table? Default: \code{TRUE}.
#'
#' @return An invisible list of the production table.
#'
#' @family Diagnostics
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' cat("Production table:\n")
#' l<-printPT(g$PT, g, verbose=TRUE)
#' cat("Short Production table:\n")
#' printPT(g$SPT, g, verbose=TRUE)
#' 
#' @export
printPT<-function(PT, G, verbose=TRUE)
{ l<-list()
  decodeSymVec<-function(v, ST)
     { return(Reduce(unlist(lapply(ST$Symbols[v],as.character)), f=paste0)) }
  for (i in (1:length(PT$LHS)))
     { a<-paste0(decodeSymVec(PT$LHS[i], G$ST), " := ")
       l[[i]]<-paste0(a, decodeSymVec(PT$RHS[[i]], G$ST))
       if (verbose) {cat(l[[i]], "\n")} }
return(invisible(l))}

#' The dataframe of a production table of a grammar (readable).
#'
#' @param PT A production table of the grammar G.
#' @param G  A grammar object
#'
#' @return A dataframe of the production table.
#'
#' @family Diagnostics
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' cat("Production table:\n")
#' l<-dataframePT(g$PT, g)
#' cat("Short Production table:\n")
#' dataframePT(g$SPT, g)
#' 
#' @export
dataframePT<-function(PT, G)
{ l<-list()
  decodeSymVec<-function(v, ST)
     { return(Reduce(unlist(lapply(ST$Symbols[v],as.character)), f=paste0)) }
  df<-data.frame()
  for (i in (1:length(PT$LHS)))
     { df<-rbind(df, c(decodeSymVec(PT$LHS[i], G$ST), 
                      decodeSymVec(PT$RHS[[i]], G$ST)))} 
colnames(df)<-c("LHS", "RHS")
rownames(df)<-1:nrow(df)
return(df)}
