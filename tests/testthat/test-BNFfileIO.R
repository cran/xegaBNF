
library(testthat)
library(xegaBNF)

test_that("booleanGrammar OK", 
{
           G<-booleanGrammar() 
           fn<-"~/dev/cran/xega/xegaBNF/BooleanGrammar.txt"
	   expect_identical(G$filename, fn)
	   expect_identical("BNF" %in% names(G), TRUE)
}
)

test_that("readBNF, writeBNF, and newBNF",
{
# 
# environment variable NOT_CRAN=true (set by devtools!)
#
# For covr: Sys.setenv(NOT_CRAN = "true")
skip_on_cran()
G<-booleanGrammar()
G$filename<-"TestReadWriteBNF.txt"
if(file.exists(G$filename)) {file.remove(G$filename)}
f<-writeBNF(G)
G1<-readBNF(G$filename, eol="\n")
G2<-readBNF(G$filename)
expect_identical(G, G1)
GBNFfunction<-newBNF(G$filename)
G3<-GBNFfunction()
G2$BNF<-gsub("\n", " ", G2$BNF)
expect_identical(identical(G2$BNF, G3$BNF), TRUE)
if(file.exists(G$filename)) {file.remove(G$filename)}
}
)

