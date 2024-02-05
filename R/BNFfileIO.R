
#
# Grammar Compiler for BNFs
# (c) 2020 A. Geyer-Schulz
# Package BNF
#

#' A constant function which returns the BNF (Backus-Naur Form) 
#' of a context-free grammar for the XOR problem.
#'
#' @return A named list with $filename and  $BNF, 
#'         the grammar of a boolean grammar with two variables and
#'         the boolean functions AND, OR, and NOT.
#'
#' @examples
#' booleanGrammar()
#' @export
booleanGrammar<-function()
{
   fn<-"~/dev/cran/xega/xegaBNF/BooleanGrammar.txt"
   bnf<-"S := <fe>; <fe> := <f0> | "
   bnf<-paste(bnf, "  <f1> \"(\" <fe> \")\" | ")
   bnf<-paste(bnf, " <f2> \"(\" <fe> \",\" <fe> \")\"; ")
   bnf<-paste(bnf, " <f0> := \"D1\" | \"D2\"; ")
   bnf<-paste(bnf, " <f1> := \"NOT\"; ")
   bnf<-paste(bnf, "  <f2> := \"OR\" | \"AND\"; ")
   return(list(filename=fn, BNF=bnf))
}
#booleanGrammar<-newBNF("~/dev/cran/xega/xegaBNF/BooleanGrammar.txt")

#' Write BNF into text file.
#'
#' @description \code{writeBNF()} writes a character string into a textfile.
#'
#' @details The user writes the BNF to a text file which he edits.
#'          The newline symbols are inserted after each substitution variant 
#'          and after each production rule to improve the readability 
#'          of the grammar by the user.
#'
#' @param g     A named list with $filename and  $BNF as a character string.
#' @param fn    A file name. Default: NULL.
#' @param eol   End-of-line symbol(s). Default: \code{"\\n"}
#'
#' @return  Invisible NULL.
#'
#' @family File I/O
#'
#' @examples
#' g<-booleanGrammar()
#' fn<-tempfile()
#' writeBNF(g, fn)
#' g1<-readBNF(fn, eol="\n")
#' unlink(fn)
#' @export
writeBNF<-function(g, fn=NULL, eol="\n")
{ fname<-fn
if (identical(fn, NULL)) {fname<-g$filename}
con<-file(fname, "wb")
g1<-gsub("\\|", paste0("\\|",eol), gsub(";", paste0(";",eol), g$BNF))
b<-writeChar(g1, con, eos=NULL)
close(con)
return(invisible(b))
}

#' Read text file.
#'
#' @description \code{readBNF()} reads a text file and 
#'              returns a character string.
#'
#' @param filename    A file name.
#' @param eol   End-of-line symbol(s). Default: ""
#'
#' @return A named list with
#'     \itemize{
#'         \item $filename  the filename.
#'         \item $BNF a character string with the newline symbol \\n.
#'         }
#'
#' @family File I/O
#'
#' @examples
#' g<-booleanGrammar()
#' fn<-tempfile()
#' writeBNF(g, fn)
#' g1<-readBNF(fn)
#' unlink(fn)

#' @export
readBNF<-function(filename, eol="")
{
con<-file(filename, "rb")
BNF<-readChar(con, file.size(filename))
if (!identical(eol, "")) {BNF<-gsub(eol, "", BNF)}
close(con)
return(list(filename=filename, BNF=BNF))
}

#' Convert grammar file into a constant function.
#' 
#' @description \code{newBNF()} reads a text file and 
#'              returns a constant function which returns
#'              the BNF as a character string.  
#'
#' @details The purpose of this function is to include examples
#'          of grammars in packages.
#'
#' @param filename A file name.
#' @param eol   End-of-line symbol(s). Default: \code{"\\n"}
#' 
#' @return Returns a constant function which returns a BNF.
#' 
#' @family File I/O
#'
#' @examples
#' g<-booleanGrammar()
#' fn<-tempfile()
#' writeBNF(g, fn)
#' g1<-newBNF(fn)
#' unlink(fn)
#' @export
newBNF<-function(filename, eol="\n")
{
parm<-function(x){function() {return(x)}}
b<-readBNF(filename)
b$BNF<-gsub(eol, "", b$BNF)
f<-parm(b)
x<-f()
return(f)
}

# end of file
