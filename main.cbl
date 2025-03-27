       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MAIN. 

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-PRIX             PIC 9(8) VALUE ZEROS. 
       01  WS-CAPITAL          PIC 9(8) VALUE ZEROS. 
       01  WS-TAUX             PIC 9(1).99 VALUE ZEROS.
       01  WS-MENSUALITE       PIC 9(8) VALUE ZEROS. 
       01  WS-TEMP-INT         PIC 9(8) VALUE ZEROS.
       01  WS-TEMP-FLOAT         PIC 9(8)V99 VALUE ZEROS.
       01  WS-MENU-CHOICE      PIC 9(1) VALUE ZEROS. 
       01  WS-FORMATED-INT     PIC ZZ,ZZZ,ZZZ VALUE ZEROS. 
       01  WS-FORMATED-FLOAT   PIC ZZ,ZZZ,ZZZ.99 VALUE ZEROS.
       01 WS-LIST-INDEX    PIC 9(2) VALUE 1.
       01  TABLEAU.
           05 WS-LIST-APPARTEMENT OCCURS 64.
               10  WS-CARAC-APPARTEMENT OCCURS 4.
                   15 FILLER   PIC 9(8) VALUE ZEROS.
                   15 FILLER   PIC 9(8) VALUE ZEROS.
                   15 FILLER   PIC 9(1)V99 VALUE ZEROS.
                   15 FILLER   PIC 9(8) VALUE ZEROS.


       PROCEDURE DIVISION.

           START-MENU-CHOICE.
               DISPLAY "Choisissez votre  option" X'0A' 
                   " 1 : ajouter un nouvelle appartment" X'0A' 
                   " 2 : consulter vos appartments".
               ACCEPT WS-MENU-CHOICE.
               
               EVALUATE WS-MENU-CHOICE
                   WHEN 1
                       GO TO START-AJOUT-APPARTEMENT
                   WHEN 2
                       GO TO START-AFFICHER-APPARTEMENT
                   WHEN other
                       DISPLAY "Valeur invalide"
                       GO TO START-MENU-CHOICE
               END-EVALUATE.

           END-MENU-CHOICE. 

           START-AJOUT-APPARTEMENT.

               CALL 'FindFirstFreeIndex' USING 
                   TABLEAU, 
                   WS-LIST-INDEX.

               DISPLAY "Entrez le prix de votre appartement : ".
               ACCEPT WS-PRIX.
               MOVE WS-PRIX TO WS-CAPITAL.

               DISPLAY "Entrez le taux d'emprunt : ".
               ACCEPT WS-TAUX.

               DISPLAY "Entrez le nombre de mensualité : ".
               ACCEPT WS-MENSUALITE.

               MOVE WS-PRIX TO 
               WS-CARAC-APPARTEMENT(WS-LIST-INDEX, 1).
               MOVE WS-PRIX TO 
               WS-CARAC-APPARTEMENT(WS-LIST-INDEX, 2).
               MOVE WS-TAUX TO 
               WS-CARAC-APPARTEMENT(WS-LIST-INDEX, 3).
               MOVE WS-MENSUALITE TO 
               WS-CARAC-APPARTEMENT(WS-LIST-INDEX, 4).
               
               DISPLAY "Appartement n° "WS-LIST-INDEX " ajouté. " 
               WITH NO ADVANCING.
               add 1 TO WS-LIST-INDEX.
               DISPLAY "Vous pouvez entrer l'appartement n° "
                   WS-LIST-INDEX X'0A'.

           GO TO START-MENU-CHOICE.
           END-AJOUT-APPARTEMENT.

           
           START-AFFICHER-APPARTEMENT.

           MOVE 1 TO WS-LIST-INDEX.
           PERFORM UNTIL WS-CARAC-APPARTEMENT(WS-LIST-INDEX, 1) = ZEROS

               MOVE WS-LIST-INDEX TO WS-TEMP-INT
               MOVE WS-TEMP-INT TO WS-FORMATED-INT
               DISPLAY "Appartement n° " WS-FORMATED-INT " :"
           
               MOVE WS-CARAC-APPARTEMENT(WS-LIST-INDEX, 1)
                   TO WS-TEMP-INT
               MOVE WS-TEMP-INT TO WS-FORMATED-INT
               DISPLAY " - Prix : " WS-FORMATED-INT
           
               MOVE WS-CARAC-APPARTEMENT(WS-LIST-INDEX, 2)
                   TO WS-TEMP-INT
               MOVE WS-TEMP-INT TO WS-FORMATED-INT
               DISPLAY " - Capital restant dû : " WS-FORMATED-INT X'0A'
               " - Taux d'emprunt : "
                WS-CARAC-APPARTEMENT(WS-LIST-INDEX, 3) X'0A'
           
               ADD 1 TO WS-LIST-INDEX

           END-PERFORM.

           GO TO START-MENU-CHOICE.
           END-AFFICHER-APPARTEMENT.

           STOP RUN.

       END PROGRAM MAIN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  FindFirstFreeIndex. 
       
       DATA DIVISION.
       LINKAGE SECTION.
       
       01  TABLEAU.
           05 WS-LIST-APPARTEMENT OCCURS 64.
               10  WS-CARAC-APPARTEMENT OCCURS 4.
                   15 FILLER   PIC 9(8) VALUE ZEROS.
                   15 FILLER   PIC 9(8) VALUE ZEROS.
                   15 FILLER   PIC 9(1)V99 VALUE ZEROS.
                   15 FILLER   PIC 9(8) VALUE ZEROS.
       01  WS-LIST-INDEX    PIC 9(2) VALUE 1.

       PROCEDURE DIVISION USING TABLEAU, WS-LIST-INDEX.
           PERFORM UNTIL WS-CARAC-APPARTEMENT(WS-LIST-INDEX, 1) = ZERO
              ADD 1 TO WS-LIST-INDEX
           END-PERFORM
           EXIT PROGRAM.
       END PROGRAM FindFirstFreeIndex.
