       IDENTIFICATION DIVISION.
       PROGRAM-ID. Q05.
      *> QUESTION 5: DAILY TRANSACTION TOTAL
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
       01 WS-INPUT-LINE      PIC X(50).
       01 WS-N               PIC S9(5) VALUE 0.
       01 WS-I               PIC 9(5) VALUE 0.
       01 WS-AMT             PIC S9(10) VALUE 0.
       01 WS-TOTAL           PIC 9(12) VALUE 0.
       01 WS-INVALID-FLAG    PIC 9 VALUE 0.
       01 WS-DISP-TOT        PIC Z(11)9.
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
               DISPLAY "INVALID TRANSACTION"
               IF WS-FILE-OPENED = 1
                   CLOSE INPUT-FILE
               END-IF
               STOP RUN
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-N
               PERFORM READ-LINE
               COMPUTE WS-AMT = FUNCTION NUMVAL(WS-INPUT-LINE)
               IF WS-AMT < 0
                   MOVE 1 TO WS-INVALID-FLAG
               ELSE
                   ADD WS-AMT TO WS-TOTAL
               END-IF
           END-PERFORM

           IF WS-INVALID-FLAG = 1
               DISPLAY "INVALID TRANSACTION"
           ELSE
               MOVE WS-TOTAL TO WS-DISP-TOT
               DISPLAY FUNCTION TRIM(WS-DISP-TOT)
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
