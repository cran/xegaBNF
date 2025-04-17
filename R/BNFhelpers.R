
#' Returns the list of symbol identifiers of nonterminal symbols in G.
#'
#' @param G A compiled context-free grammar.
#'
#' @return The list of the symbol identifiers of all
#'         nonterminal symbols of grammar G
#'
#' @family Compilation of short production table
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' nonTerminalsOfG(g)
#' 
#'@export
nonTerminalsOfG<-function(G)
{return(G$ST$SymbolId[as.logical(G$ST$NonTerminal)])}

#' Are all symbols of vector of symbols terminal symbols?
#'
#' @param symbols A vector of symbol identifiers.
#' @param ST      A symbol table.
#'
#' @return Boolean. 
#' 
#' @family Compilation of short production table
#' 
#' @examples
#' g<-compileBNF(booleanGrammar())
#' s<-c(2, 10, 3)
#' allTerminal(s, g$ST)
#'
#' @export
allTerminal<-function(symbols, ST=ST)
{ return(Reduce(as.logical(unlist(
         lapply(symbols,FUN=isTerminal, ST=ST))), f="&")) }

#' Which production rules produce only terminal symbols?
#'
#' @param G A compiled context-free grammar.
#'
#' @return Vector of booleans.
#' 
#' @family Compilation of short production table
#' 
#' @examples
#' g<-compileBNF(booleanGrammar())
#' finiteRulesOfG(g)
#' 
#' @export
finiteRulesOfG<-function(G)
{return(unlist(lapply(G$PT$RHS, FUN=allTerminal, ST=G$ST)))}

#' Which production rules contain a direct recursion?
#'
#' @details Direct recursion means the nonterminal on the LHS 
#'          is also a symbol on RHS of production rule.
#'
#' @param G A compiled context-free grammar.
#'
#' @return Vector of booleans.
#' 
#' @family Compilation of short production table
#' 
#' @examples
#' g<-compileBNF(booleanGrammar())
#' directRecursion(g)
#' 
#' @export
directRecursion<-function(G)
{ r<-vector()
for (i in (1:length(G$PT$LHS))) { r<-c(r, G$PT$LHS[i] %in% G$PT$RHS[[i]])}
return(r) }

#' Combines two lists.
#'
#' @details Each element of the result 
#'          has the form \code{unlist(c(x[i], y[j]))} 
#'          for all \code{i} and for all \code{j}. 
#'
#' @param x  A list.
#' @param y  A list.
#' 
#' @return A list.
#'
#' @family Compilation of short production table
#' 
#' @examples
#' a<-cL(c(1, 2), c(2, 3))
#' b<-cL(a, c(6))
#'
#' @export
cL<-function(x, y)
{ r<-list(); k<-1
for (i in 1:length(x)) {
   for (j in 1:length(y)) {r[[k]]<-unlist(c(x[i], y[j])); k<-k+1} }
return(r)}

#' Expands a vector of symbol vectors.
#'
#' @param obj A vector of symbol vectors.
#'
#' @return A list of symbol vectors which is the Cartesian product
#'         of all symbol vectors in \code{obj}. 
#'
#' @family Compilation of short production table
#'
#' @examples
#' l<-list()
#' l[[1]]<-c(1, 2, 3)
#' l[[2]]<-c(4, 5)
#' expandGrid(l)
#'
#' @export
expandGrid<-function(obj)
{ cols<-length(obj)
colLengths<-unlist(lapply(obj, FUN=length))
r<-obj[[1]]
if (cols==1) {return(r)}
for (i in (2:cols)) {r<-cL(r, obj[[i]])}
return(r) }
 
#' Replaces rules with fNTs and terminals by a new set of rules with terminals. 
#'
#' @param rPT  Rules fNTs and terminals.
#' @param SPT  Current short production table (SPT).
#' @param   G  The grammar.
#'
#' @return The extended short production table.
#'
#' @family Compilation of short production table
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' finiteRules<-finiteRulesOfG(g)
#' SPT<-newPT(LHS=g$PT$LHS[finiteRules], RHS=g$PT$RHS[finiteRules])
#' rest<-!(finiteRulesOfG(g) | directRecursion(g))
#' restPT<-newPT(LHS=g$PT$LHS[rest], RHS=g$PT$RHS[rest])
#' nSPT<-expandRules(rPT=restPT, SPT=SPT, g)
#' printPT(nSPT, g)
#'
#' @export
expandRules<-function(rPT, SPT, G)
{ sRHS<-SPT$RHS; sLHS<-SPT$LHS
for (i in (1:length(rPT$LHS)))
   {newfNT<-rPT$LHS[i]; symlist<-rPT$RHS[[i]]
     lj<-list(); n<-1
     for (j in (1:length(symlist)))
        {plist<-vector()
         if (isTerminal(symlist[j], G$ST)) {lj[[j]]<-list(symlist[j])}
         else 
            {p<-rules(symlist[j], SPT$LHS)
             lk<-list()
             for (k in (1:length(p))) {lk[[k]]<-derive(p[k], SPT$RHS)}
             n<-n*length(p)
             lj[[j]]<-lk}}
         sLHS<-append(sLHS, rep(newfNT, n))
         sRHS<-c(sRHS, expandGrid(lj))}
ePT<-newPT(sLHS, sRHS)
return(ePT)}

#' Find next rule which must be expanded.
#'
#' @param PT     Production table.
#' @param fNTS   List of finite non terminals.
#' @param G      A grammar.
#'
#' @return A list of indices. 
#'
#' @family Compilation of short production table
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' finiteRules<-finiteRulesOfG(g)
#' SPT<-newPT(LHS=g$PT$LHS[finiteRules], RHS=g$PT$RHS[finiteRules])
#' finiteNTs<-unique(SPT$LHS)
#' rest<-!(finiteRulesOfG(g) | directRecursion(g))
#' restPT<-newPT(LHS=g$PT$LHS[rest], RHS=g$PT$RHS[rest])
#' findNextRuleForExpansion(restPT, finiteNTs, g)
#' 
#' @export
findNextRuleForExpansion<-function(PT, fNTS, G)
{ ST<-G$ST; LHS<-PT$LHS; RHS<-PT$RHS
NTs<-unique(LHS)
### Modify RHS.
newRHS<-list()
for (i in (1:length(RHS)))
    { symbols<-RHS[[i]]; newSymbols<-vector()
      for (j in (1:length(symbols)))
         { if (isNonTerminal(symbols[j], G$ST) & (!symbols[j] %in% fNTS))
         {newSymbols<-append(newSymbols, symbols[j])}}
     newRHS[[i]]<-unique(newSymbols)}
### Remove empty rules and duplicates.
index<-1:length(LHS)
newindex<-vector(); emptyindex<-vector()
### Which rules are empty? 
for (i in (1:length(LHS)))
    {if (!(length(newRHS[[i]])==0))
    {newindex<-append(newindex, index[i])} else
    {emptyindex<-append(emptyindex, index[i])} }
return(emptyindex)
}

#' List of rules with the smallest number of nonterminals.
#'
#' @param PT Production table.
#'
#' @returns List of indices of the production rule(s) with the 
#'          smallest number of non terminals.
#' 
#' @family Compilation of short production table
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' smallestRules(g$PT)
#'
#' @importFrom utils head
#' @export
smallestRules<-function(PT)
{
NTs<-unique(PT$LHS)
rulelist<-list()
for (i in (1:length(NTs))) { rulelist[[i]]<-rules(NTs[i], PT$LHS) }
smallest<-vector() 
for (i in (1:length(NTs)))
{ crl<-rulelist[[i]]
  size<-vector()
  for (j in (1:length(crl)))
    {size<-append(size, length(derive(crl[j], PT$RHS))) }
    smallest<-append(smallest, utils::head(crl[min(size)== size])) }
return(smallest)
}

#' Constructs a new production table.
#' 
#' @param LHS  The vector of non-terminal identifiers.
#' @param RHS  A list of vectors of symbols.
#'
#' @return A production table (a named list) with the elements
#'         \enumerate{
#'         \item \code{$LHS}: A vector of nonterminals.
#'         \item \code{$RHS}: A list of vectors of symbols.
#'          } 
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' nPT<-newPT(g$PT$LHS, g$PT$RHS)
#' 
#' @export
newPT<-function(LHS, RHS)
{ PT<-list()
  PT$LHS<-LHS
  PT$RHS<-RHS
  a<-PT$LHS; a<-PT$RHS
return(PT)
}

