       IDENTIFICATION DIVISION.
       PROGRAM-ID. Q09.
      *> QUESTION 9: CUSTOMER ACCOUNT ELIGIBILITY
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
       01 WS-AGE             PIC S9(5) VALUE 0.
       01 WS-VERIF-FLAG      PIC X(10).
       01 WS-FLAG-CHAR       PIC X.
       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT INPUT-FILE
           IF WS-FILE-STATUS = "00"
               MOVE 1 TO WS-FILE-OPENED
           ELSE
               MOVE 0 TO WS-FILE-OPENED
           END-IF

           PERFORM READ-LINE
           COMPUTE WS-AGE = FUNCTION NUMVAL(WS-INPUT-LINE)

           PERFORM READ-LINE
           MOVE FUNCTION TRIM(WS-INPUT-LINE) TO WS-VERIF-FLAG
           MOVE WS-VERIF-FLAG(1:1) TO WS-FLAG-CHAR

           IF WS-AGE >= 18 AND
              (WS-FLAG-CHAR = "Y" OR WS-FLAG-CHAR = "y")
               DISPLAY "ELIGIBLE"
           ELSE
               DISPLAY "NOT ELIGIBLE"
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
