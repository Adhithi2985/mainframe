       IDENTIFICATION DIVISION.
       PROGRAM-ID. Q04.
      *> QUESTION 4: MINIMUM BALANCE FEE
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
       01 WS-BALANCE         PIC S9(10) VALUE 0.
       01 WS-MIN-BAL         PIC S9(10) VALUE 0.
       01 WS-FEE             PIC S9(10) VALUE 0.
       01 WS-FINAL-BAL       PIC S9(10) VALUE 0.
       01 WS-DISP-BAL        PIC Z(9)9.
       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT INPUT-FILE
           IF WS-FILE-STATUS = "00"
               MOVE 1 TO WS-FILE-OPENED
           ELSE
               MOVE 0 TO WS-FILE-OPENED
           END-IF

           PERFORM READ-LINE
           COMPUTE WS-BALANCE = FUNCTION NUMVAL(WS-INPUT-LINE)

           PERFORM READ-LINE
           COMPUTE WS-MIN-BAL = FUNCTION NUMVAL(WS-INPUT-LINE)

           PERFORM READ-LINE
           COMPUTE WS-FEE = FUNCTION NUMVAL(WS-INPUT-LINE)

           IF WS-BALANCE < 0 OR WS-MIN-BAL < 0 OR WS-FEE < 0
               DISPLAY "INVALID INPUT"
           ELSE
               IF WS-BALANCE < WS-MIN-BAL
                   IF WS-BALANCE >= WS-FEE
                       COMPUTE WS-FINAL-BAL = WS-BALANCE - WS-FEE
                       MOVE WS-FINAL-BAL TO WS-DISP-BAL
                       DISPLAY FUNCTION TRIM(WS-DISP-BAL)
                   ELSE
                       DISPLAY "FEE CANNOT BE COLLECTED"
                   END-IF
               ELSE
                   MOVE WS-BALANCE TO WS-DISP-BAL
                   DISPLAY FUNCTION TRIM(WS-DISP-BAL)
               END-IF
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
