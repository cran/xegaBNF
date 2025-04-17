
#' Generate synthetic variable names as list of rules in BNF.  
#'
#' @details Compiles BNF rules for variable names.
#'          The integers in \code{1:k} are the integer part 
#'          of the variable names generated. 
#'
#' @param varNT  String. The non-terminal symbol for variables.
#' @param varSym  String. The variable names (character part).
#' @param k       Integer. Number of variables.  
#'
#' @return Text vector with production rules for k variables.
#'
#' @family Syntactic support.
#'
#' @examples
#' cat(variableNamesBNF("<f0>", "D", 7))
#'
#' @export
variableNamesBNF<-function(varNT, varSym, k)
{ a<-unlist(lapply(1:k,
        FUN=function(x) paste(varNT, " := \"", varSym, x, "\";\n",sep="")))
     b<-""
     for (i in (1:length(a))) { b<-paste(b, a[i], sep="") }
     return(b) }

#' Generate synthetic variable names as list of rules in BNF.  
#'
#' @details Compiles the LHS part of a BNF rule for variable names.
#'          The integers in \code{1:k} are the integer part 
#'          of the variable names generated. 
#'
#' @param varSym  String. The variable names (character part).
#' @param k       Integer. Number of variables.  
#'
#' @return Text vector with production rules for k variables.
#'
#' @family Syntactic support.
#'
#' @examples
#' cat(variableNamesLHS("D", 7))
#'
#' @export
variableNamesLHS<-function(varSym, k)
{    if (k==1) {return(paste("\"", varSym, k, "\"", sep=""))} 
     a<-unlist(lapply(1:(k-1),
        FUN=function(x) paste("\"", varSym, x, "\" | ",sep="")))
     b<-""
     for (i in (1:length(a))) { b<-paste(b, a[i], sep="") }
     a<-paste(b, "\"", varSym, k, "\"", sep="") 
     return(a) }

#' R-code to bind variable names  with values from a vector.
#' 
#' @details Compiles R code for the assignment of a vector 
#'          to single variables. The variable names are formed
#'          by catenating the variable name \code{varSym} to 
#'          the character string of an integer in \code{1:k}.
#'
#' @param varSym  String.  Variable name (character part).
#' @param vecSym  String.  Vector name.
#' @param k       Integer. Number of variables.
#'
#' @return R-code which assigns the content of the vector to
#'         a list of k variables with synthetic names.
#'
#' @family Semantic support.
#'
#' @examples
#' bindKvariables("B", "v", 5) 
#' @export
bindKvariables<-function(varSym, vecSym, k)
{ a<-unlist(lapply(1:k,
        FUN=function(x) paste(varSym, x, "<-", vecSym, "[",x,"]\n",sep="")))
     b<-""
     for (i in (1:length(a))) { b<-paste(b, a[i], sep="") }
     return(b) }

