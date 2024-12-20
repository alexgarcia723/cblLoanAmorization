# cblLoanAmorization
A loan amorization scheduler written in COBOL for z/Os

# File descriptions
KC03B4D.LOAN.CBL(LOAN) - the cobol program to compile

KC03B4D.LOAN.JCL(COMP) - the jcl for compiling the program

KC03B4D.LOAN.JCL(LOAN01) - the jcl for running the program (currently set use the file KC03B4D.COBOL.LOANIN(LOAN01) with parameter 'HTML')

KC03B4D.COBOL.LOANIN(LOANXX) - the loan file containing the loan info in the form
<br>LOAN TITLE<br>LOAN AMOUNT <br> INTEREST <br> YEARS <br> ADDITIONAL PAYMENTS (optional)

KC03B4D.LOAN.OUT(LOAN) - example output from running the program with loan01 with the html parameter
