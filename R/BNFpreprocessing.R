
#' Does the grammar contain macros? 
#'
#' @description Macros (R-code) in grammars starts and ends 
#'              with the pattern \code{" //R//"}.  
#'              \code{existsMacro()} tests if macro patterns are present
#'              in a grammar file.            
#' 
#' @param BNFfn A constant function which returns a BNF.
#'
#' @return Boolean.
#'
#' @family Grammar Preprocessor
#'
#' @examples
#' existsMacro(booleanGrammar)
#' existsMacro(booleanGrammarK)
#'
#' @export
existsMacro<-function(BNFfn)
{  return(grepl(" //R//", BNFfn()$BNF))}

#' Is the number macro patterns even?
#'
#' @description \code{evenMacro} tests if the macro start/end patterns
#'              are balanced (occur in even numbers).  
#'
#' @param BNFfn A constant function which returns a BNF.
#'
#' @return Boolean.
#'
#' @family Grammar Preprocessor
#'
#' @examples
#' evenMacro(booleanGrammar)
#' evenMacro(booleanGrammarK)
#'
#' @export
evenMacro<-function(BNFfn)
{ return((length(which(strsplit(BNFfn()$BNF, " ")[[1]] %in% "//R//")) %% 2)==0) }

#' Catenates a vector of strings into a single string.
#'
#' @description The vector elements are separated by a white space.
#'
#' @param tvec  A vector of strings.
#'
#' @return A string.
#'
#' @family Grammar Preprocessor
#'
#' @examples
#' a<-c("text", "text2")
#' pastePart(a)
#'
#' @export
pastePart<-function(tvec)
{ f<-""
  for (i in (1:length(tvec))) { f<-paste(f, tvec[[i]], sep=" ")}
  return(f)
}

#' BNF preprocessing.
#'
#' @description The BNF preprocessor executes macros 
#'              (R-code embedded in a BNF grammar definition) 
#'              and replaces the macros by the output they produce.
#'
#' @details The embedded R-code starts with \code{" //R//"} and ends 
#'          with "\code{" //R//"}. The preprocessor accepts a binding 
#'          list which binds R objects their values. The macros are 
#'          evaluated in an environment with these bindings. 
#'          The output of each macro is inserted into the grammar file.
#'          It is expected that after preprocessing, the grammar file 
#'          is in the BNF-notation. For example, generic grammar files
#'          can be provided for which the number of symbols of a certain 
#'          type (e.g. variables) can be specified by the bindings. 
#' 
#' @param BNFfn A constant function which returns a BNF.
#' @param genv  The list of bindings needed by the macros in the R-code.
#'
#' @return A list with elements \code{filename} and \code{BNF}
#'
#' @family Grammar Preprocessor
#' 
#' @importFrom utils tail
#' @examples
#' a<-preBNF(booleanGrammar)
#' b<-preBNF(booleanGrammarK, list(k=5))
#' 
#' @export
preBNF<-function(BNFfn, genv=NULL)
{ if (!existsMacro(BNFfn)) {return(BNFfn())}
  if (!evenMacro(BNFfn))   {stop("Unbalanced markers for R-code in ", BNFfn()$name)} 
  tvec<-strsplit(BNFfn()$BNF, " ")[[1]]
  front<-""
  while (length(tvec)>0) {
      mps<-which(tvec %in% "//R//")
      if (length(mps)==0) {break}
      if (!mps[1]==1) { front<-paste(front, pastePart(tvec[1:(mps[1]-1)]), sep=" ")}
      # extract and execute macro code.
      macro<- eval(parse(text=pastePart(tvec[(mps[1]+1):(mps[2]-1)])), 
                   envir=genv, enclos=parent.frame())
      front<-paste(front, macro, sep=" ")
      tvec<-utils::tail(tvec, length(tvec)-mps[2]) }
  return(list(filename=BNFfn()$filename, BNF=paste(front, pastePart(tvec), sep=" ")))
}

