
#
# Grammar Compiler for BNFs
# (c) 2020 A. Geyer-Schulz
# Package BNF
#

# (2) Make Symbol Table ST 

#' Build a symbol table from a character string which contains a BNF.
#' 
#' @description \code{makeSymbolTable()} extracts all terminal 
#'              and non-terminal symbols from a BNF
#'              and builds a data frame with the columns 
#'              Symbols (string), NonTerminal (0 or 1), and SymbolId (int).
#'              The symbol "NotExpanded" is added which codes
#'              depth violations of a derivation tree.
#' 
#' @param BNF  A character string with the BNF.
#' 
#' @return A data frame with the columns 
#'    \code{Symbols}, \code{NonTerminal}, and \code{SymbolID}.
#' 
#' @family Compiler Steps
#'
#' @examples
#' makeSymbolTable(booleanGrammar()$BNF)
#'
#' @export
makeSymbolTable<-function(BNF)
{ b<-strsplit(BNF,";")[[1]]
  c<-chartr("|", " ",paste(b[2:length(b)], sep=" ", collapse=""))
  d<-gsub(pattern=":=", replacement="",x=c)
  e<-gsub(pattern="\"", replacement="",x=d)
  f<-unique(strsplit(e, " ")[[1]])
  Symbols<-c("NotExpanded", sort(f[!f==""]))
  NonTerminal<- as.numeric(grepl(pattern="<",Symbols))
  SymbolId<-1:length(Symbols)
return(data.frame(Symbols, NonTerminal, SymbolId))}

# Object definition:
# r<-list()
# r[["ST"]]<-ST
# r[["id"]]<-function(sym){return(ST[sym==ST[,1],3])}
# r[["symbol"]]<-function(SymbId){return(ST[SymbId==ST[,3],1])}
# r[["Terminal"]]<-function(SymbId){return(1-ST[SymbId,2])}
# r[["NonTerminal"]]<-function(SymbId){return(ST[SymbId,2])}

#' Convert a symbol to a numeric identifier.
#' 
#' @description \code{symb2id()} converts a symbol to a numeric id. 
#' 
#' @param sym  A character string with the symbol, e.g. <fe> or "NOT". 
#' @param ST   A symbol table. 
#' 
#' @return \itemize{
#'         \item A positive integer if the symbol exists or  
#'         \item an empty integer (\code{integer(0)}) 
#'               if the symbol does not exist.}
#' 
#' @family Utility Functions
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' symb2id("<fe>", g$ST)
#' symb2id("NOT", g$ST)
#' symb2id("<fe", g$ST)
#' symb2id("NO", g$ST)
#' identical(symb2id("NO", g$ST), integer(0))
#'
#' @export
symb2id<-function(sym, ST)
	{return(as.integer(ST[sym==ST[,1],3]))}

#' Convert a numeric identifier to a symbol.
#' 
#' @description \code{id2symb()} converts a numeric id to a symbol.
#' 
#' @param Id       A numeric identifier (integer).  
#' @param ST       A symbol table. 
#' 
#' @return \itemize{
#'         \item  A symbol string if the identifier exists or 
#'         \item  an empty character string (\code{character(0)}) 
#'                if the identifier 
#'                does not exist.}
#' 
#' @family Utility Functions
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' id2symb(1, g$ST)
#' id2symb(2, g$ST)
#' id2symb(5, g$ST)
#' id2symb(12, g$ST)
#' id2symb(15, g$ST)
#' identical(id2symb(15, g$ST), character(0))
#'
#' @export
id2symb<-function(Id, ST)
	{return(as.character(ST[Id==ST[,3],1]))}

#' Is the numeric identifier a terminal symbol?
#' 
#' @description \code{isTerminal()} tests if the numeric identifier 
#'        is a terminal symbol.
#'
#' @details \code{isTerminal()} is one of the most frequently used 
#'          functions of a grammar-based genetic programming algorithm.
#'          Careful coding pays off! 
#'          Do not index the symbol table as a matrix 
#'          (e.g. \code{ST[2,2]}), because this is really slow! 
#' 
#' @param Id  A numeric identifier (integer). 
#' @param ST  A symbol table. 
#' 
#' @return \itemize{ 
#'    \item \code{TRUE} if the numeric identifier is a terminal symbol.
#'    \item \code{FALSE} if the numeric identifier is a non-terminal symbol.
#'    \item \code{NA} if the symbol does not exist.}
#'
#' @family Utility Functions
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' isTerminal(1, g$ST)
#' isTerminal(2, g$ST)
#' isTerminal(5, g$ST)
#' isTerminal(12, g$ST)
#' isTerminal(15, g$ST)
#' identical(isTerminal(15, g$ST), NA)
#'
#' @export
isTerminal<-function(Id, ST)
       {return(as.logical(1-ST$NonTerminal[Id]))}
# isTerminal<-function(Id, ST)
#       {return(as.logical(1-ST[Id,2]))}

#' Is the numeric identifier a non-terminal symbol?
#' 
#' @description \code{isNonTerminal()} tests if the numeric identifier 
#'               is a non-terminal symbol.
#' 
#' @details \code{isNonTerminal()} is one of the most frequently used 
#'          functions of a grammar-based genetic programming algorithm.
#'          Careful coding pays off! 
#'          Do not index the symbol table as a matrix 
#'          (e.g. \code{ST[2,2]}), because this is really slow! 
#' 
#' @param Id     A numeric identifier (integer). 
#' @param ST     A symbol table. 
#' 
#' @return \itemize{ 
#'    \item \code{TRUE} if the numeric identifier is a terminal symbol.
#'    \item \code{FALSE} if the numeric identifier is a non-terminal symbol.
#'    \item \code{NA} if the symbol does not exist.}
#' 
#' @family Utility Functions
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' isNonTerminal(1, g$ST)
#' isNonTerminal(2, g$ST)
#' isNonTerminal(5, g$ST)
#' isNonTerminal(12, g$ST)
#' isNonTerminal(15, g$ST)
#' identical(isNonTerminal(15, g$ST), NA)
#'
#' @export
isNonTerminal<-function(Id, ST)
	{return(as.logical(ST$NonTerminal[Id]))}
#isNonTerminal<-function(Id, ST)
#	{return(as.logical(ST[Id,2]))}

# (3) Make Production Table PT 

#' Transforms a single BNF rule into a production table. 
#' 
#' @description \code{makeRule()} transforms a single BNF rule
#'              into a production table. 
#' 
#' @details Because a single BNF rule can provide a set of substitutions, 
#'          more than one line in a production table may result.
#'          The number of substitutions corresponds to the number of lines 
#'          in the production table.
#'
#' @param Rule     A rule. 
#' @param ST       A symbol table. 
#' 
#' @return A named list with 2 elements, namely \code{$LHS} and \code{$RHS}. 
#'         The left-hand side \code{$LHS} is 
#'         a vector of non-terminal identifiers
#'         and the right-hand side \code{$RHS} is a vector of vectors
#'         of numerical identifiers. 
#'         The list represents the substitution of \code{$LHS[i]} 
#'         by the identifier list \code{$RHS[[i]]}.  
#' 
#' @examples
#' c<-booleanGrammar()$BNF
#' ST<-makeSymbolTable(c)
#' c<-booleanGrammar()$BNF 
#' b<-strsplit(c,";")[[1]]
#' a<-b[2:4]
#' a<-gsub(pattern=";",replacement="", paste(a[1], a[2], a[3], sep=""))
#' makeRule(a, ST)
#'
#' @export
makeRule<-function(Rule, ST)
{ LHS<-vector('integer')
  RHS<-vector('list')
  PT<-list()
  c<-strsplit(Rule, ":=")[[1]]
  d<-gsub(pattern=" ", replacement="", c[1])
  left<-symb2id(d,ST) # length(LHS)==0 implies error!
  if (0==length(left))
    { PT[["LHS"]]<-LHS 
      PT[["RHS"]]<-RHS 
      return(PT) }
  d<-strsplit(c[2], "\\|")[[1]]
  for (i in 1:length(d))
    {e<-strsplit(gsub(pattern="\"",replacement="", d[i]), " ")[[1]]
     f<-vector("integer")
     for (j in 1:length(e))
        {f<-c(f,symb2id(e[j], ST))}
     LHS<-c(LHS, left)
     RHS[[i]]<-f }  
  PT[["LHS"]]<-LHS 
  PT[["RHS"]]<-RHS 
  return(PT) }

#' Produces a production table. 
#' 
#' @description \code{makeProductionTable()} produces a production table 
#'               from a specification of a BNF.   
#'               Warning: No error checking implemented.
#' 
#' @param BNF    A character string with the BNF.
#' @param ST     A symbol table.
#' 
#' @return A production table is a named list with elements 
#'         \code{$LHS} and \code{$RHS}:  
#'         \itemize{
#'         \item The left-hand side \code{LHS}
#          is a vector
#'         of non-terminal identifiers. 
#'         \item The right-hand side \code{RHS} is represented as a 
#'         vector of vectors of numerical identifiers. 
#'         }
#'         The non-terminal identifier \code{LHS[i]} derives into \code{RHS[i]}.
#'
#' @family Compiler Steps
#'
#' @examples
#' a<-booleanGrammar()$BNF
#' ST<-makeSymbolTable(a)
#' makeProductionTable(a,ST)
#'
#' @export
makeProductionTable<-function(BNF, ST)
{ LHS<-vector('integer')
  RHS<-vector('list')
 b<-strsplit(BNF,";")[[1]]
 c<-b[2:length(b)]
 for (i in 1:length(c))
   { rule<-makeRule(c[i], ST)
   LHS<-c(LHS, rule$LHS)
   RHS<-c(RHS, rule$RHS) }
 PT<-list()
 PT[["LHS"]]<-LHS 
 PT[["RHS"]]<-RHS 
 return(PT) }

# Object PT:
#
#   rules(numeric index of nonterminal)
# PT[["rules"]]<-function(sym){return(as.vector((1:length(LHS))[sym==LHS]))}
#   derive(index of rule in production table)
# PT[["derive"]]<-function(ri){return(RHS[[ri]])}
#
# If not OK, with some error checking ...
#
#  PT[["rules"]]<- function(sym) {
#	a<-(sym==LHS)
#	if (Reduce(f="|", a)) 
#	{return(as.vector((1:length(a))[a]))}
#        else
#	{return(vector("integer"))}}
# PT[["derive"]]<- function(rindex) {
#        if (rindex>length(LHS)){return(vector("integer"))}
#        if (rindex<1){return(vector("integer"))}
#        return(RHS[[rindex]]) }

#' Returns all indices of rules applicable for a non-terminal identifier.
#' 
#' @description \code{rules()} finds 
#'              all applicable production rules
#'              for a non-terminal identifier.
#' 
#' @param Id   A numerical identifier.
#' @param LHS  The left-hand side of a production table.
#' 
#' @return \itemize{
#'   \item A vector of indices of all applicable rules 
#'         in the production table or
#'   \item an empty integer (\code{integer(0)}), 
#'         if the numerical identifier is not found 
#'         in the left-hand side of the production table.}
#'
#' @family Utility Functions
#'
#' @examples
#' a<-booleanGrammar()$BNF
#' ST<-makeSymbolTable(a)
#' PT<-makeProductionTable(a,ST)
#' rules(5, PT$LHS)
#' rules(8, PT$LHS)
#' rules(9, PT$LHS)
#' rules(1, PT$LHS)
#'
#' @export
rules<-function(Id, LHS){return(as.vector((1:length(LHS))[Id==LHS]))}

#' Derives the identifier list which expands the non-terminal identifier. 
#' 
#' @description \code{derives()} returns the identifier list which expands 
#'              a non-terminal identifier.
#'               Warning: No error checking implemented.
#' 
#' @param RuleIndex     An index (integer) in the production table. 
#' @param RHS           The right-hand side of the production table. 
#' 
#' @return A vector of numerical identifiers.
#'
#' @family Utility Functions
#'
#' @examples
#' a<-booleanGrammar()$BNF
#' ST<-makeSymbolTable(a)
#' PT<-makeProductionTable(a,ST)
#' derive(1, PT$RHS)
#' derive(2, PT$RHS)
#' derive(3, PT$RHS)
#' derive(5, PT$RHS)
#'
#' @export
derive<-function(RuleIndex, RHS){return(RHS[[RuleIndex]])}

# (4) Make Start Symbol

#' Extracts the numerical identifier of the start symbol of the grammar. 
#' 
#' @description \code{makeStartSymbol()} returns 
#'              the start symbol's numerical identifier 
#'               from a specification of a context-free grammar in BNF.   
#'               Warning: No error checking implemented.
#' 
#' @param BNF   A character string with the BNF.
#' @param ST    A symbol table.
#' 
#' @return The numerical identifier of the start symbol of the BNF. 
#'
#' @family Compiler Steps
#'
#' @examples
#' a<-booleanGrammar()$BNF
#' ST<-makeSymbolTable(a)
#' makeStartSymbol(a,ST)
#'
#' @export
makeStartSymbol<-function(BNF, ST)
{
 b<-strsplit(BNF,";")[[1]]
 c<-strsplit(b,":=")[[1]]
 d<-gsub(pattern=" ", replacement="", x=c[2])
 return(symb2id(d, ST))
}

# (5) Make Short Production Table PT 

#' Produces a production table with non-recursive productions only.
#' 
#' @description \code{compileShortPT()} produces a ``short'' production table 
#'               from a context-free grammar. The short production table does not
#'               contain recursive production rules.  
#'               Warning: No error checking implemented.
#' 
#' @param G      A grammar with symbol table \code{ST}, 
#'               production table \code{PT}, 
#'               and start symbol \code{Start}.
#' 
#' @details \code{compileShortPT()} starts with production rules whose 
#'          right-hand side contains only terminals. 
#'          It incrementally builds up the new PT until in the new PT 
#'          at least one
#'          production rule exists for each non-terminal 
#'          which replaces the non-terminal symbol by a list 
#'          of terminal symbols.
#' 
#'          The short production rule table provides for each non-terminal 
#'          symbol a minimal finite derivation into terminals. 
#'          It contains a finite subset of the context-free language 
#'          as defined by the grammar \code{G}.
#'          Instead
#'          of the full production table, it is used
#'          for generating depth-bounded derivation trees.
#'          The first idea of defining such a finite part of the language
#'          is due to M. P. Schützenberger (1966).
#'
#' @return A (short) production table is a named list with 2 columns.
#'         The first column
#'         (the left-hand side \code{LHS}) is a vector
#'         of non-terminal identifiers. 
#'         The second column
#'         (the right-hand side \code{RHS}) is a 
#'         vector of vectors of numerical identifiers. 
#'         \code{LHS[i]} derives into \code{RHS[i]}.
#'
#' @references Schützenberger, M. P. (1966): 
#'         Classification of Chomsky Languages. In: 
#'         Steel, T. B. Jr. (Ed.) 
#'         Formal Language Description Languages for Computer Programming. 
#'         Proceedings of the IFIP Workshop on Formal 
#'         Language Description Languages.
#'         North-Holland, Amsterdam, 100 -104. 
#'
#' @family Compiler Steps
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' compileShortPT(g)
#'
#' @export
compileShortPT<-function(G)
{ ST<-G$ST
  RHS<-G$PT$RHS
  LHS<-G$PT$LHS
  nonTerminals<-nonTerminalsOfG(G)
  finiteRules<-finiteRulesOfG(G)
  directRecursiveRules<-directRecursion(G)
  restOfTheRules<-!(finiteRules | directRecursiveRules)
# Finite Rules. 
  SPT<-newPT(LHS=LHS[finiteRules], RHS=RHS[finiteRules])
  finiteNTs<-unique(SPT$LHS)
# if all non-terminals have a finite branch, we are done.
if (length(finiteNTs)==length(nonTerminals)) {PT<-SPT; return(PT)}
# Rest
restPT<-newPT(LHS=LHS[restOfTheRules], RHS=RHS[restOfTheRules])
# If only one rule is left, we have to expand the rule, and we are done.
if (length(restPT$LHS)==1) 
   {PT<-expandRules(rPT=restPT, SPT=SPT, G); return(PT)}
# If more than one rule is left, we have to loop ...
# 1. find the next rule which depends only on fNTS.
# 2. expandRules.
# 3. termination: we have rules for all NTS in fNTS.
repeat {
   finiteNTs<-unique(SPT$LHS)
   if (length(finiteNTs)==length(nonTerminals)) {PT<-SPT; return(PT)}
   nxtRules<-findNextRuleForExpansion(restPT, finiteNTs, G)
### Smallest expansion only!
   expPT<-newPT(LHS=restPT$LHS[nxtRules], RHS=restPT$RHS[nxtRules])
   small<-smallestRules(expPT)
   expPTsmall<-newPT(LHS=expPT$LHS[small], RHS=expPT$RHS[small])
   SPT<-expandRules(rPT=expPTsmall, SPT=SPT, G)
   index<-!((1:length(restPT$LHS)) %in% nxtRules)
   restPT<-newPT(restPT$LHS[index], restPT$RHS[index])}
}

# (6) Compile BNF 

#' Compile a  BNF (Backus-Naur Form) of a context-free grammar.
#' 
#' @description \code{compileBNF} produces a context-free grammar  
#'               from its specification in Backus-Naur form (BNF).   
#'               Warning: No error checking is implemented.
#'
#' @details A grammar consists of the symbol table \code{ST}, the production
#'          table \code{PT}, the start symbol \code{Start}, 
#'          and the short production
#'          table \code{SPT}. 
#'
#' @details The function performs the following steps:
#'  \enumerate{
#'  \item Make the symbol table. See \code{\link{makeSymbolTable}}.
#'  \item Make the production table. See \code{\link{makeProductionTable}}.
#'  \item Extract the start symbol. See \code{\link{makeStartSymbol}}.
#'  \item Compile a short production table. See \code{\link{compileShortPT}}.
#'  \item Return the grammar.}
#' 
#' @param g  A character string with a BNF. 
#' @param verbose  Boolean. TRUE: Show progress. Default: FALSE. 
#' 
#' @return A grammar object (list) with the attributes 
#'         \itemize{
#'         \item \code{name} (the filename of the grammar),
#'         \item \code{ST} (symbol table), 
#'         \item \code{PT} (production table), 
#'         \item \code{Start} (the start symbol of the grammar), and
#'         \item \code{SPT} (the short production table).
#'         }
#'
#' @references   Geyer-Schulz, Andreas (1997):
#'      \emph{Fuzzy Rule-Based Expert Systems and Genetic Machine Learning},
#'      Physica, Heidelberg. (ISBN:978-3-7908-0830-X)
#'
#' @family Grammar Compiler
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' g$ST
#' g$PT
#' g$Start
#' g$SPT
#' @export
compileBNF<-function(g, verbose=FALSE)
{
  CFG<-list()
  CFG[["name"]]<-g$filename
  ST<-makeSymbolTable(g$BNF)
  if (verbose) {cat("Symbol table done.\n")}
  CFG[["ST"]]<-ST
  CFG[["PT"]]<-makeProductionTable(g$BNF,ST)
  if (verbose) {cat("Production table done.\n")}
  CFG[["Start"]]<-makeStartSymbol(g$BNF,ST)
  if (verbose) {cat("Start symbol done.\n")}
  CFG[["SPT"]]<-compileShortPT(CFG)
  if (verbose) {cat("Short production table done.\n")}
  return(CFG)
}

# end of file
