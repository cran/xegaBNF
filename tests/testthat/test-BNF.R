
library(testthat)
library(xegaBNF)

test_that("makeSymbolTable OK", 
{
           G<-booleanGrammar()$BNF 
	   A<-makeSymbolTable(G)
           n<-c("Symbols", "NonTerminal", "SymbolId")
           c2<-c(rep(0, 4), rep(1, 4), rep(0, 5))
           c3<-c("NotExpanded", "(", ")", ",", 
                 "<f0>", "<f1>", "<f2>", "<fe>",
                 "AND", "D1", "D2", "NOT", "OR")
           expect_identical(class(A), "data.frame")
           expect_equal(nrow(A), 13)
           expect_equal(ncol(A), 3)
           expect_equal(ncol(A), 3)
	   expect_identical((names(A) %in% n), rep(TRUE, 3))  
	   expect_identical(A[,3], (1:13))
	   expect_identical(A[,2], c2)
	   expect_identical(A[,1], c3)
}
)

test_that("symb2id OK", 
{
           G<-compileBNF(booleanGrammar()) 
	   expect_identical(symb2id("NO", G$ST), integer(0))
	   expect_equal(symb2id("<fe>", G$ST), 8)
	   expect_equal(symb2id("NOT", G$ST), 12)
	   expect_equal(symb2id("(", G$ST), 2)
}
)

test_that("id2symb2 OK", 
{
           G<-compileBNF(booleanGrammar()) 
	   expect_identical(id2symb(15, G$ST), character(0))
	   expect_equal(id2symb(8, G$ST), "<fe>")
	   expect_equal(id2symb(12, G$ST), "NOT")
	   expect_equal(id2symb(2, G$ST), "(")
}
)

test_that("isTerminal OK", 
{
           G<-compileBNF(booleanGrammar()) 
	   expect_identical(isTerminal(15, G$ST), NA)
	   expect_identical(isTerminal(8, G$ST), FALSE)
	   expect_identical(isTerminal(12, G$ST), TRUE)
	   expect_identical(isTerminal(2, G$ST), TRUE)
}
)

test_that("isNonTerminal OK", 
{
           G<-compileBNF(booleanGrammar()) 
	   expect_identical(isNonTerminal(15, G$ST), NA)
	   expect_identical(isNonTerminal(8, G$ST), TRUE)
	   expect_identical(isNonTerminal(12, G$ST), FALSE)
	   expect_identical(isNonTerminal(2, G$ST), FALSE)
}
)

test_that("rules OK", 
{
           G<-compileBNF(booleanGrammar()) 
	   expect_equal(rules(8, G$PT$LHS), c(1, 2, 3))
	   expect_equal(rules(5, G$PT$LHS), c(4, 5))
	   expect_equal(rules(6, G$PT$LHS), c(6))
	   expect_identical(rules(9, G$PT$LHS), integer(0))
}
)

test_that("derive OK", 
{
           G<-compileBNF(booleanGrammar()) 
	   expect_equal(derive(3, G$PT$RHS), c(7, 2, 8, 4, 8, 3))
	   expect_equal(derive(2, G$PT$RHS), c(6, 2, 8, 3))
	   expect_equal(derive(1, G$PT$RHS), c(5))
	   expect_error(derive(9, G$PT$RHS), "subscript")
}
)

test_that("compileBNF verbose OK", 
{
expect_output(compileBNF(booleanGrammar(), verbose=TRUE), "Symbol table done.") 
expect_output(compileBNF(booleanGrammar(), verbose=TRUE), "Production table done.") 
expect_output(compileBNF(booleanGrammar(), verbose=TRUE), "Start symbol done.") 
expect_output(compileBNF(booleanGrammar(), verbose=TRUE), "Short production table done.") 
}
)

