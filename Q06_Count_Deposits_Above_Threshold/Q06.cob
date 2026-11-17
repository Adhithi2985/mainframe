       IDENTIFICATION DIVISION.
       PROGRAM-ID. Q06.
      *> QUESTION 6: COUNT DEPOSITS ABOVE A THRESHOLD
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
       01 WS-THRESHOLD       PIC S9(10) VALUE 0.
       01 WS-COUNT           PIC 9(5) VALUE 0.
       01 WS-DISP-COUNT      PIC Z(4)9.
       01 WS-DEPOSITS-TABLE.
          05 WS-DEPOSIT      PIC S9(10) OCCURS 1000 TIMES.
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

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-N
               PERFORM READ-LINE
               COMPUTE WS-DEPOSIT(WS-I) =
                   FUNCTION NUMVAL(WS-INPUT-LINE)
           END-PERFORM

           PERFORM READ-LINE
           COMPUTE WS-THRESHOLD = FUNCTION NUMVAL(WS-INPUT-LINE)

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-N
               IF WS-DEPOSIT(WS-I) > WS-THRESHOLD
                   ADD 1 TO WS-COUNT
               END-IF
           END-PERFORM

           MOVE WS-COUNT TO WS-DISP-COUNT
           DISPLAY FUNCTION TRIM(WS-DISP-COUNT)

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
