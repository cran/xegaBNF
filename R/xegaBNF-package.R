
#' \code{xegaBNF} implements a grammar compiler for context-free languages
#' specified in BNF and a few utility functions. 
#' The grammar compiler 
#' generates a grammar object. 
#' This object used by the package
#' \code{xegaDerivationTrees}, as well as for grammar-based genetic 
#' programming (\code{xegaGpGene}) and grammatical evolution 
#' (\code{xegaGeGene}. 
#'
#' @section BNF (Backus-Naur Form):
#'
#' Grammars of context-free languages are represented 
#' in Backus-Naur Form (BNF). See e.g. Backus et al. (1962).
#' 
#' The BNF is a meta-language for specifying the syntax of context-free 
#' languages. The BNF provides 
#' \enumerate{
#' \item non-terminal symbols,
#' \item terminal symbols, and
#' \item meta-symbols of the BNF. 
#' }
#' A non-terminal symbol has the following form:
#' \code{<pattern>}, where pattern is an arbitrary sequence of letters, numbers, 
#' and symbols.  
#'
#' A terminal symbol has the following form:
#' \code{"pattern"}, where pattern is an arbitrary sequence of letters, numbers, 
#' and symbols. 
#'
#' The BNF has three meta symbols, namely \code{::=}, \code{|}, and \code{;} 
#' which are used for the specification of production (substitution) rules. 
#' \code{::=} separates the left-hand side of the rule from the right-hand
#' side of the rule. \code{;} indicates the end of a production rule.
#' \code{|} separates the symbol sequences of a compound production rule. 
#' A production rule has the following form:
#'
#' \code{LHS ::= RHS;}
#'
#' where \code{LHS} is a single non-terminal symbol and 
#' \code{RHS} is either a simple symbol sequence or a compound symbol 
#' sequence.
#' 
#' A production rule with a simple symbol sequence 
#' specifies the substitution of 
#' the non-terminal symbol on the \code{LHS} by the symbol sequence of 
#' the \code{RHS}. 
#'
#' A production rule with a compound symbol sequence 
#' specifies the substitution of 
#' the non-terminal symbol on the \code{LHS} by one of the symbol sequences of 
#' the \code{RHS}. 
#'
#' @references 
#'   Backus, J. W., Bauer, F. L., Green, J., Katz, C., McCarthy, J., 
#'   Naur, Peter, Perlis, A. J., Ruthishauser, H.,  and Samelson, K.
#'   (1962)
#'   Revised Report on the Algorithmic Language ALGOL 60, IFIP, Rome.
#'   
#'
#' @section Editing BNFs:
#'
#' The BNF may be stored in ASCII text files and edited with standard editors.
#'
#' @section The Internal Representation of a Grammar Object:
#'
#' A grammar object is represented as a named list:
#' \itemize{
#' \item $name contains the filename of the BNF.
#' \item $ST   the symbol table.
#' \item $PT   the production table.
#' \item $Start the start symbol of the grammar.
#' \item $SPT  a short production table without recursive rules. 
#' }
#'
#' @section The Compilation Process:
#'
#' The main steps of the compilation process are:
#'  \enumerate{
#'  \item Store the filename.
#'  \item Make the symbol table. See \code{\link{makeSymbolTable}}.
#'  \item Make the production table. See \code{\link{makeProductionTable}}.
#'  \item Extract the start symbol. See \code{\link{makeStartSymbol}}.
#'  \item Compile a short production table. See \code{\link{compileShortPT}}.
#'  \item Return the grammar.}
#'
#' @section The User-Interface of the Compiler:
#'
#' \code{compileBNF(g)} where \code{g} is a character string with a BNF.
#'
#' @section Utility Functions for xegaX-Packages:
#'
#' \itemize{ 
#' \item isTerminal, isNonTerminal: For testing the symbol type of 
#'                                  identifiers in a grammar object.
#' \item rules, derives: For choosing rules and for substitutions.
#' }
#' 
#' @section The Architecture of the xegaX-Packages:
#' 
#' The xegaX-packages are a family of R-packages which implement 
#' eXtended Evolutionary and Genetic Algorithms (xega).  
#' The architecture has 3 layers, 
#' namely the user interface layer,
#' the population layer, and the gene layer: 
#' 
#' \itemize{
#' \item
#' The user interface layer (package \code{xega}) 
#' provides a function call interface and configuration support
#' for several algorithms: genetic algorithms (sga), 
#' permutation-based genetic algorithms (sgPerm), 
#' derivation-free algorithms as e.g. differential evolution (sgde), 
#' grammar-based genetic programming (sgp) and grammatical evolution
#' (sge). 
#'
#' \item
#' The population layer (package \code{xegaPopulation}) contains
#' population related functionality as well as support for 
#' population statistics dependent adaptive mechanisms and parallelization.
#'
#' \item 
#' The gene layer is split into a representation-independent and 
#' a representation-dependent part:
#' \enumerate{
#' \item 
#'  The representation indendent part (package \code{xegaSelectGene})
#'  is responsible for variants of selection operators, evaluation 
#'  strategies for genes, as well as profiling and timing capabilities.        
#' \item 
#'  The representation dependent part consists of the following packages: 
#' \itemize{
#' \item \code{xegaGaGene} for binary coded genetic algorithms.
#' \item \code{xegaPermGene} for permutation-based genetic algorithms.
#' \item \code{xegaDfGene} for derivation free algorithms as e.g. 
#'                         differential evolution.
#' \item \code{xegaGpGene} for grammar-based genetic algorithms.
#' \item \code{xegaGeGene} for grammatical evolution algorithms.
#' }
#' The packages \code{xegaDerivationTrees} and \code{xegaBNF} support
#' the last two packages:
#' \itemize{
#' \item \code{xegaBNF} essentially provides a grammar compiler.
#' \item \code{xegaDerivationTrees} implements an abstract data type for derivation trees.
#'   }
#' }} 
#' 
#' @family Package Description
#'
#' @name xegaBNF
#' @aliases xegaBNF
#' @docType package
#' @title Package xegaBNF
#' @author Andreas Geyer-Schulz
#' @section Copyright: (c) 2023 Andreas Geyer-Schulz
#' @section License: MIT
#' @section URL: <https://github.com/ageyerschulz/xegaBNF>
#' @section Installation: From CRAN by \code{install.packages('xegaBNF')}
NULL

