       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MAIN. 

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-PRIX             PIC 9(8) VALUE ZEROS. 
       01  WS-CAPITAL          PIC 9(8) VALUE ZEROS. 
       01  WS-TAUX             PIC 9(1).99 VALUE ZEROS.
       01  WS-MENSUALITE       PIC 9(8) VALUE ZEROS. 
       01  WS-TEMP-INT         PIC 9(8) VALUE ZEROS.
       01  WS-TEMP-FLOAT       PIC 9(8)V99 VALUE ZEROS.
       01  WS-MENU-CHOICE      PIC 9(1) VALUE ZEROS. 
       01  WS-CONITUNER-AJOUT  PIC X(1) VALUE SPACES.
       01  WS-FORMATED-INT     PIC ZZ,ZZZ,ZZZ VALUE ZEROS. 
       01  WS-FORMATED-FLOAT   PIC ZZ,ZZZ,ZZZ.99 VALUE ZEROS.
       01 WS-LIST-INDEX    PIC 9(2) VALUE 1.
       01  TABLEAU.
           05  WS-LIST-APPARTEMENT OCCURS 64.
               10  WS-CARAC-APPARTEMENT OCCURS 4.
                   15 FILLER   PIC 9(8) VALUE ZEROS.
                   15 FILLER   PIC 9(8) VALUE ZEROS.
                   15 FILLER   PIC 9(1)V99 VALUE ZEROS.
                   15 FILLER   PIC 9(8) VALUE ZEROS.
           05  WS-APPART-TOEDIT    PIC 9(2) VALUE ZEROS.


       PROCEDURE DIVISION.

       START-MENU-CHOICE.
               DISPLAY "Choisissez votre  option" X'0A' 
                   " 1 : ajouter un nouvelle appartment" X'0A' 
                   " 2 : consulter vos appartments" X'0A' 
                   " 3 : Modifier un appartment".
               ACCEPT WS-MENU-CHOICE.
               
               EVALUATE WS-MENU-CHOICE
                   WHEN 1
                       GO TO START-AJOUT-APPARTEMENT
                   WHEN 2
                       GO TO START-AFFICHER-APPARTEMENT
                   WHEN 3
                       GO TO START-EDITER-APPARTEMENT   
                   WHEN other
                       DISPLAY WS-MENU-CHOICE
                       STOP RUN
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

                MOVE SPACES TO WS-CONITUNER-AJOUT.
                PERFORM UNTIL WS-CONITUNER-AJOUT = 'y' OR 
                   WS-CONITUNER-AJOUT = 'n'

                   DISPLAY "Voulez-vous ajouter un autre bien ? (y/n)"
                   ACCEPT WS-CONITUNER-AJOUT
        
                   EVALUATE WS-CONITUNER-AJOUT
                       WHEN 'y'
                           GO TO START-AJOUT-APPARTEMENT
                       WHEN 'n'
                           GO TO END-AJOUT-APPARTEMENT
                       WHEN other
                           DISPLAY "Valeur invalide"
                           GO TO START-MENU-CHOICE
                   END-EVALUATE
        
               END-PERFORM.

       END-AJOUT-APPARTEMENT.
           GO TO START-MENU-CHOICE.

           
           START-AFFICHER-APPARTEMENT.

           DISPLAY X'0A'"Voici vos appartements : " X'0A'.

           CALL 'PrintAppartmennts' USING 
               TABLEAU, 
               WS-LIST-INDEX.

       END-AFFICHER-APPARTEMENT.
           GO TO START-MENU-CHOICE.

       START-EDITER-APPARTEMENT.
               
           DISPLAY X'0A'.
           CALL 'PrintAppartmennts' USING 
               TABLEAU, 
               WS-LIST-INDEX.

           MOVE ZEROS TO WS-APPART-TOEDIT.
           PERFORM UNTIL WS-APPART-TOEDIT IS NOT = ZEROS AND 
               WS-CARAC-APPARTEMENT(WS-APPART-TOEDIT, 1) IS NOT = ZEROS
               
               DISPLAY "Quel appartement voulez éditer ? "
               ACCEPT WS-APPART-TOEDIT

               IF WS-CARAC-APPARTEMENT(WS-APPART-TOEDIT, 1) = ZEROS
                   DISPLAY "Numéro de l'appartement invalide"
                   MOVE ZEROS TO WS-APPART-TOEDIT
               END-IF

           END-PERFORM.

           MOVE WS-CARAC-APPARTEMENT(WS-APPART-TOEDIT, 1) 
               TO WS-TEMP-INT.
           MOVE WS-TEMP-INT TO WS-FORMATED-INT.
           DISPLAY "Entrez le prix ou appuyez sur entrer pour conserver"
               " la valeur actuel ("WS-FORMATED-INT") :".

           ACCEPT WS-TEMP-INT.
           IF WS-TEMP-INT > 0
               MOVE WS-TEMP-INT TO 
               WS-CARAC-APPARTEMENT(WS-APPART-TOEDIT, 1)
           END-IF.
           
           DISPLAY "Entrez le taux d'emprunt ou appuyez sur entrer" 
              " pour conserver la valeur actuel ("
              WS-CARAC-APPARTEMENT(WS-APPART-TOEDIT, 3) ") :".

           ACCEPT WS-TEMP-FLOAT.
           IF WS-TEMP-FLOAT > 0
               MOVE WS-TEMP-FLOAT TO 
               WS-CARAC-APPARTEMENT(WS-APPART-TOEDIT, 3)
           END-IF.

           MOVE WS-CARAC-APPARTEMENT(WS-APPART-TOEDIT, 4) 
               TO WS-TEMP-INT.
           MOVE WS-TEMP-INT TO WS-FORMATED-INT.
           DISPLAY "Entrez les mensualités ou appuyez sur entrer"
               " pour conserver la valeur actuel ("WS-FORMATED-INT") :".

           ACCEPT WS-TEMP-INT.
           IF WS-TEMP-INT > 0
               MOVE WS-TEMP-INT TO 
               WS-CARAC-APPARTEMENT(WS-APPART-TOEDIT, 4)
           END-IF.

       END-EDITER-APPARTEMENT.
           GO TO START-MENU-CHOICE.


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

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PrintAppartmennts. 
       
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  WS-TEMP-INT         PIC 9(8) VALUE ZEROS.
       01  WS-TEMP-FLOAT       PIC 9(8)V99 VALUE ZEROS.
       01  WS-FORMATED-INT     PIC ZZ,ZZZ,ZZZ VALUE ZEROS. 
       01  WS-FORMATED-FLOAT   PIC ZZ,ZZZ,ZZZ.99 VALUE ZEROS.

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

               MOVE WS-CARAC-APPARTEMENT(WS-LIST-INDEX, 4)
                   TO WS-TEMP-INT
               MOVE WS-TEMP-INT TO WS-FORMATED-INT
               DISPLAY " - Mensualités : " WS-FORMATED-INT X'0A'
           
               ADD 1 TO WS-LIST-INDEX

           END-PERFORM.

           EXIT PROGRAM.
       END PROGRAM PrintAppartmennts.
