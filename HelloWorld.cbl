           identification division.
           program-id.  HELLO-WORLD. 

           data division.
           working-storage section.

           01  WS-PRIX             PIC 9(8) value zeros. 
           01  WS-CAPITAL          PIC 9(8) value zeros. 
           01  WS-TAUX             PIC 9(1)V99 value zeros.
           01  WS-MENSUALITE       PIC 9(8) value zeros. 
           01  WS-TEMP-INT       PIC 9(8) value zeros.
           01  WS-FORMATED-INT     PIC ZZ,ZZZ,ZZZ value zeros. 
           01  WS-FORMATED-FLOAT   PIC ZZ,ZZZ,ZZZ.99 value zeros.


           01 TABLEAU.
               05 WS-LIST-INDEX    PIC 9(2) value 1.
               05 WS-LIST-APPARTEMENT occurs 64.
                   10  WS-CARACTERISTIQUE-APPARTEMENT occurs 4.
                       15 FILLER   PIC 9(8) value zeros.
                       15 FILLER   PIC 9(8) value zeros.
                       15 FILLER   PIC 9(1)V99 value zeros.
                       15 FILLER   PIC 9(8) value zeros.



           procedure division.

           START-AJOUT-APPARTEMENT.

               display "Entrez le prix de votre appartement : ".
               accept WS-PRIX.
               move WS-PRIX to WS-CAPITAL.

               display "Entrez le taux d'emprunt : ".
               accept WS-TAUX.

               display "Entrez le nombre de mensualité : ".
               accept WS-MENSUALITE.

               move WS-PRIX to 
               WS-CARACTERISTIQUE-APPARTEMENT(WS-LIST-INDEX, 1).
               move WS-PRIX to 
               WS-CARACTERISTIQUE-APPARTEMENT(WS-LIST-INDEX, 2).
               move WS-TAUX to 
               WS-CARACTERISTIQUE-APPARTEMENT(WS-LIST-INDEX, 3).
               move WS-MENSUALITE to 
               WS-CARACTERISTIQUE-APPARTEMENT(WS-LIST-INDEX, 4).
               
               display "Appartement n° "WS-LIST-INDEX "ajouté." 
               WITH NO ADVANCING.
               add 1 to WS-LIST-INDEX.
               display "Vous pouvez entrer l'appartement n° "
                   WS-LIST-INDEX.
           END-AJOUT-APPARTEMENT.
           stop run.
