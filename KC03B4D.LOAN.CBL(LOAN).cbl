      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOAN.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT loan_info ASSIGN TO INFILE
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS LOANFILE-STATUS.
            SELECT optional loan_out ASSIGN TO OUTFILE
               ORGANIZATION IS SEQUENTIAL.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.

       FD loan_info recording mode f.

       01 loan_file.
           05 loan_record PIC X(80).

       FD loan_out recording mode f
            record contains 1000 characters.

       01 loan_table PIC X(1000).

       WORKING-STORAGE SECTION.

       01 loan_title PIC X(80) value zeroes.
       01 loan_amount PIC X(80) value zeroes.
       01 loan_interest PIC X(80) value zeroes.
       01 loan_terms PIC X(80) value zeroes.
       01 loan_additional PIC X(80) value zero.

       01 ws_loan_title PIC X(40) VALUE ZEROES.
       01 ws_loan_amount PIC 9(7)v99 VALUE ZEROES.
       01 ws_loan_interest PIC 99v99 VALUE ZEROES.
       01 ws_loan_interest_p PIC .9999.
       01 ws_loan_terms PIC 9(2) VALUE ZEROES.
       01 ws_loan_additional PIC 9(7)v99 VALUE zero.



       01 ws_monthly_payment PIC 9(7)v99.
       01 ws_interest_payment PIC 9(7)v99.
       01 ws_principal_payment PIC 9(7)v99.
       01 ws_total_payment PIC 9(7)v99.

       01 ds_monthly_payment PIC 9(7).99.
       01 ds_interest_payment PIC 9(7).99.
       01 ds_principal_payment PIC 9(7).99.
       01 ds_total_payment PIC 9(7).99.
       01 ds_loan_amount PIC 9(7).99.
       01 ds_loan_additional PIC 9(7).99.

       01 ws_month PIC 9(4) value 0.
       01 ws_loan_payments PIC 9(3).
       01 ws_monthly_interest PIC v999999.


       01 output-format PIC X(4) VALUE ZEROES.

      *validation variables

       01 ws-input pic x(80).
       01 ws-valid-flag pic x value "Y".
       01 ws-index pic 9(4) comp VALUE 1.
       01 ws-len pic 9(4) comp.
       01 ws-char pic x.
       01 ws-decimal-count pic 9 value 0.

       01  LOANFILE-STATUS     PIC X(2).

       01 ws_output_line PIC X(1000) value spaces.
       01 ds_space PIC X(3) value " | ".


      * HTML stuff
       01 h_tr pic x(4) value "<tr>".
       01 h_str pic x(5) value "</tr>".
       01 h_td pic x(4) value "<td>".
       01 h_std pic x(5) value "</td>".






       LINKAGE SECTION.
       01  parameters.
           2 parameters-total-length pic 9(4) usage comp.
           2 parameter-values        pic x(20).


      ******************************************************************
       PROCEDURE DIVISION using parameters.
       MAIN-LOGIC Section.
           Perform Open-Read-Loan-Info

           move parameter-values(1:4) to output-format

           display "paramter is: " output-format

           display "loan info direct from file"
           display loan_title
           display loan_amount
           display loan_interest
           display loan_terms
           display loan_additional
           DISPLAY " "

           Perform Data-Check
           if ws-valid-flag = "Y" THEN
              compute WS_LOAN_AMOUNT = function numval(loan_amount)
              compute ws_loan_interest  = function numval(loan_interest)
              compute ws_loan_terms = function numval(loan_terms)
              compute ws_loan_additional = function numval(
               loan_additional(1:18) )
           ELSE
              OPEN OUTPUT loan_out
              move "invalid input" to loan_table
              write loan_table
              close loan_out
              goback
           end-if




           display " "
           display "loan info after putting into ws"
           display ws_loan_title
           display ws_loan_amount
           display ws_loan_interest
           display ws_loan_terms
           display ws_loan_additional

           compute ws_loan_payments = ws_loan_terms * 12
           compute WS_LOAN_INTEREST_P = ws_loan_interest / 100
           compute ws_monthly_interest = ws_loan_interest / 12




           compute ws_monthly_payment = ws_loan_amount  *
      -    function annuity(ws_monthly_interest / 100, ws_loan_payments)

           display " "
           display "loan payments: " ws_loan_payments
           display "loan interest: " ws_loan_interest_p
           display "monthly payment " ws_monthly_payment

           OPEN OUTPUT loan_out


           if output-format = "HTML" THEN
              move spaces to ws_output_line
              STRING
              "<!DOCTYPE html><html lang = ""en"">" delimited by SIZE
              "<head><meta charset=""UTF-8"">" delimited by SIZE
              "<meta name=""viewport""" delimited by SIZE
              "content=""width=device-width" delimited by SIZE
              ",initial-scale=1.0"">" delimited by SIZE
              "<title>" DELIMITED by SIZE
              function trim (ws_loan_title) delimited by SIZE
              "</title></head>" delimited by SIZE
              into ws_output_line
              move spaces to loan_table

              move function trim(ws_output_line) to loan_table
              write loan_table

              move spaces to ws_output_line
              STRING
              "<body>" DELIMITED BY SIZE
              "<h1>" DELIMITED BY SIZE
              function trim (ws_loan_title) delimited by SIZE
              "</h1>" DELIMITED BY SIZE
              "<table border=""1"">" DELIMITED BY SIZE
              into ws_output_line
              move spaces to loan_table
              move function trim(ws_output_line) to loan_table
              write loan_table

              move spaces to ws_output_line
              STRING
              "<thead><tr><th>Month</th>" DELIMITED BY SIZE
              "<th>Monthly Payment</th>" DELIMITED BY SIZE
              "<th>Interest Paid</th>" DELIMITED BY SIZE
              "<th>Principal Paid</th>" DELIMITED BY SIZE
              "<th>Additional Paid</th>" DELIMITED BY SIZE
              "<th>Total Payment</th>" DELIMITED BY SIZE
              "<th>New Balance</th>" DELIMITED BY SIZE
              "</tr></thead>" DELIMITED BY SIZE
              "<tbody>" DELIMITED BY SIZE
              into ws_output_line
              move spaces to loan_table
              move function trim(ws_output_line) to loan_table
              write loan_table
           else
              move "TEXT" to output-format
              move ws_loan_title to loan_table
              write loan_table
           end-if

           display " "
           display " "
           display "-----table-----"
           display " "
           display
           "month|monthlypaynt|interestpaid|princpalpaid|aditinalpaid|"
           "totalpayment|newbalance"

           if output-format = "TEXT" THEN
              move spaces to ws_output_line
              STRING
                 "month|monthlypaynt|interestpaid|" delimited by size
                 "princpalpaid|aditinalpaid|" delimited by size
                 "totalpayment|newbalance" delimited by size
                 into WS_OUTPUT_LINE
           end-string
           move spaces to loan_table
           move function trim(ws_output_line) to loan_table
           write loan_table
           end-if

           perform Amorization until ws_loan_amount = 0

           if output-format = "HTML"
              move spaces to ws_output_line
              STRING
              "</tbody>" DELIMITED BY SIZE
              "</table>" DELIMITED BY SIZE
              "</body>" DELIMITED BY SIZE
              "</html>" DELIMITED BY SIZE
              into ws_output_line
              move spaces to loan_table
              move function trim(ws_output_line) to loan_table
              write loan_table
           end-if


           close loan_out


           GOBACK.

       Amorization.
           compute ws_month = ws_month + 1
           compute ws_interest_payment =
      -      ws_loan_amount * ws_monthly_interest / 1000

           compute ws_principal_payment =
      -      ws_monthly_payment - ws_interest_payment

           compute ws_total_payment =
      -      ws_loan_additional + ws_principal_payment

           if (ws_total_payment > ws_loan_amount) THEN
              compute ws_total_payment = ws_loan_amount
              compute ws_principal_payment =
      -       ws_total_payment - ws_interest_payment
              compute ws_loan_additional = 0
           end-if

           compute ws_loan_amount =
      -       ws_loan_amount - ws_total_payment




           MOVE ws_monthly_payment TO ds_monthly_payment.
           MOVE ws_interest_payment TO ds_interest_payment.
           MOVE ws_principal_payment TO ds_principal_payment.
           MOVE ws_total_payment TO ds_total_payment.
           MOVE ws_loan_amount TO ds_loan_amount.
           MOVE ws_loan_additional TO ds_loan_additional.

           move spaces to ws_output_line
           STRING
              ws_month delimited by size
              ds_space delimited by size
              ds_monthly_payment DELIMITED BY SIZE
              ds_space DELIMITED BY SIZE
              ds_interest_payment DELIMITED BY SIZE
              ds_space delimited by SIZE
              ds_principal_payment DELIMITED BY SIZE
              ds_space DELIMITED BY SIZE
              ds_loan_additional DELIMITED BY SIZE
              ds_space DELIMITED BY SIZE
              ds_total_payment DELIMITED BY SIZE
              ds_space DELIMITED BY SIZE
              ds_loan_amount DELIMITED BY SIZE
              into WS_OUTPUT_LINE
           END-STRING

           display ws_output_line(1:90)

           if output-format = "TEXT" THEN
              move ws_output_line(1:90) to loan_table
              write loan_table
           end-if

           if output-format  = "HTML" THEN
              move spaces to ws_output_line
              STRING
               h_tr DELIMITED BY SIZE
               h_td DELIMITED BY SIZE
               ws_month DELIMITED BY SIZE
               h_std DELIMITED BY SIZE

               h_td DELIMITED BY SIZE
               ds_monthly_payment DELIMITED BY SIZE
               h_std DELIMITED BY SIZE

               h_td DELIMITED BY SIZE
               ds_interest_payment DELIMITED BY SIZE
               h_std DELIMITED BY SIZE

               h_td DELIMITED BY SIZE
               ds_principal_payment DELIMITED BY SIZE
               h_std DELIMITED BY SIZE

               h_td DELIMITED BY SIZE
               ds_loan_additional DELIMITED BY SIZE
               h_std DELIMITED BY SIZE

               h_td DELIMITED BY SIZE
               ds_total_payment DELIMITED BY SIZE
               h_std DELIMITED BY SIZE

               h_td DELIMITED BY SIZE
               ds_loan_amount DELIMITED BY SIZE
               h_std DELIMITED BY SIZE

               h_str DELIMITED BY SIZE
           INTO WS_OUTPUT_LINE
           END-STRING

              move spaces to loan_table
              move function trim(ws_output_line) to loan_table
              write loan_table


           end-if

           exit.

       Open-Read-Loan-Info.
           OPEN INPUT loan_info

           READ loan_info NEXT
           move loan_record to loan_title

           READ loan_info NEXT
           move loan_record to loan_amount

           READ loan_info NEXT
           move loan_record to loan_interest

           READ loan_info NEXT
           move loan_record to loan_terms

           READ loan_info NEXT
           if LOANFILE-STATUS = '00'
              move loan_record to loan_additional
           END-IF

           CLOSE loan_info
           exit.



       Data-Check.
           display "checking loan amount"
           move zeroes to ws-input
           move function trim(loan_amount) to ws-input
           perform input-check
           display "valid is " ws-valid-flag


           display "checking loan interest"
           move zeroes to ws-input
           move function trim(loan_interest) to ws-input
           perform input-check
           display "valid is " ws-valid-flag


           display "checking loan terms"
           move zeroes to ws-input
           move function trim(loan_terms) to ws-input
           perform input-check
           display "valid is " ws-valid-flag


           display "checking loan additional"
           move zeroes to ws-input
           move function trim(loan_additional ) to ws-input
           perform input-check
           display "valid is " ws-valid-flag


           move loan_title to ws_loan_title



           exit.

       Input-Check.
           move function trim(ws-input) to ws-input
           compute ws-len = function length(
            function trim(ws-input))
           compute ws-decimal-count = 0

           display "checking: " function trim(ws-input)
           display "length is " ws-len


           PERFORM varying ws-index from 1 by 1
           until ws-index > ws-len
              display " "
              move ws-input(ws-index:1) to ws-char
              display "checking character " ws-input(ws-index:1) "at "
              "index " ws-index
              if ws-char >= '0' and ws-char <= '9'
                 display "character " ws-index "is valid"
                 CONTINUE
              else
                 if ws-char = '.'
                    display "character " ws-index "is decimal"
                    compute ws-decimal-count = ws-decimal-count + 1
                    display ws-decimal-count " decimal(s)"
                 ELSE
                    move 'N' to ws-valid-flag
                    EXIT PERFORM
                 END-IF
                 if ws-decimal-count > 1
                    move 'N' to ws-valid-flag
                    EXIT PERFORM
                 END-IF
              END-IF

           END-PERFORM
           exit.
