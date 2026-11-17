       IDENTIFICATION DIVISION.
       PROGRAM-ID. Q07.
      *> QUESTION 7: TRANSACTION COUNT BY TYPE
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  FD-INPUT-RECORD   PIC X(100).
       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS     PIC XX.
       01 WS-FILE-OPENED     PIC 9 VALUE 0.
       01 WS-INPUT-LINE      PIC X(100).
       01 WS-N               PIC S9(5) VALUE 0.
       01 WS-I               PIC 9(5) VALUE 0.
       01 WS-TYPE            PIC X(10).
       01 WS-AMT-STR         PIC X(30).
       01 WS-AMT             PIC S9(10) VALUE 0.
       01 WS-DEP-COUNT       PIC 9(5) VALUE 0.
       01 WS-DEP-TOTAL       PIC 9(12) VALUE 0.
       01 WS-WDR-COUNT       PIC 9(5) VALUE 0.
       01 WS-WDR-TOTAL       PIC 9(12) VALUE 0.
       01 WS-INVALID-BATCH   PIC 9 VALUE 0.
       01 WS-DISP-DC         PIC Z(4)9.
       01 WS-DISP-DT         PIC Z(11)9.
       01 WS-DISP-WC         PIC Z(4)9.
       01 WS-DISP-WT         PIC Z(11)9.
       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT INPUT-FILE
           IF WS-FILE-STATUS = "00"
               MOVE 1 TO WS-FILE-OPENED
           ELSE
               MOVE 0 TO WS-FILE-OPENED
           END-IF

           PERFORM READ-LINE
           COMPUTE WS-N = FUNCTION NUMVAL(WS-INPUT-LINE)

           IF WS-N < 1
               DISPLAY "INVALID BATCH"
               IF WS-FILE-OPENED = 1
                   CLOSE INPUT-FILE
               END-IF
               STOP RUN
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-N
               PERFORM READ-LINE
               INITIALIZE WS-TYPE WS-AMT-STR
               UNSTRING WS-INPUT-LINE DELIMITED BY ALL SPACES
                   INTO WS-TYPE WS-AMT-STR
               COMPUTE WS-AMT = FUNCTION NUMVAL(WS-AMT-STR)

               IF WS-AMT < 0
                   MOVE 1 TO WS-INVALID-BATCH
               ELSE
                   IF WS-TYPE = "D" OR WS-TYPE = "d"
                       ADD 1 TO WS-DEP-COUNT
                       ADD WS-AMT TO WS-DEP-TOTAL
                   ELSE
                       IF WS-TYPE = "W" OR WS-TYPE = "w"
                           ADD 1 TO WS-WDR-COUNT
                           ADD WS-AMT TO WS-WDR-TOTAL
                       ELSE
                           MOVE 1 TO WS-INVALID-BATCH
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF WS-INVALID-BATCH = 1
               DISPLAY "INVALID BATCH"
           ELSE
               MOVE WS-DEP-COUNT TO WS-DISP-DC
               MOVE WS-DEP-TOTAL TO WS-DISP-DT
               MOVE WS-WDR-COUNT TO WS-DISP-WC
               MOVE WS-WDR-TOTAL TO WS-DISP-WT

               DISPLAY "Deposit count: " FUNCTION TRIM(WS-DISP-DC)
               DISPLAY "Deposit total: " FUNCTION TRIM(WS-DISP-DT)
               DISPLAY "Withdrawal count: " FUNCTION TRIM(WS-DISP-WC)
               DISPLAY "Withdrawal total: " FUNCTION TRIM(WS-DISP-WT)
           END-IF

           IF WS-FILE-OPENED = 1
               CLOSE INPUT-FILE
           END-IF

           STOP RUN.

       READ-LINE.
           IF WS-FILE-OPENED = 1
               READ INPUT-FILE INTO WS-INPUT-LINE
                   AT END
                       MOVE SPACES TO WS-INPUT-LINE
               END-READ
           ELSE
               ACCEPT WS-INPUT-LINE
           END-IF.
