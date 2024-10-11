IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY-GAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-FILE ASSIGN TO "INVENTORY.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ITEM-ID.
       DATA DIVISION.
       FILE SECTION.
       FD INVENTORY-FILE.
       01 INVENTORY-RECORD.
           05 ITEM-ID            PIC 9(5).
           05 ITEM-NAME          PIC X(20).
           05 ITEM-QUANTITY      PIC 9(5).
           05 ITEM-PRICE         PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 WS-INVENTORY-RECORD.
           05 WS-ITEM-ID         PIC 9(5).
           05 WS-ITEM-NAME       PIC X(20).
           05 WS-ITEM-QUANTITY   PIC 9(5).
           05 WS-ITEM-PRICE      PIC 9(5)V99.
       01 WS-EOF                 PIC X VALUE 'N'.
       01 WS-CHOICE              PIC 9.
       01 WS-TEMP-QUANTITY       PIC 9(5).
       01 WS-TEMP-PRICE          PIC 9(5)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-GAME
           PERFORM GAME-LOOP UNTIL WS-CHOICE = 5
           PERFORM END-GAME
           STOP RUN.

       INITIALIZE-GAME.
           OPEN I-O INVENTORY-FILE
           DISPLAY "Welcome to the Inventory Management Game!"
           DISPLAY "Initializing inventory..."
           PERFORM INITIALIZE-INVENTORY
           DISPLAY "Inventory initialized successfully.".

       INITIALIZE-INVENTORY.
           MOVE 10001 TO ITEM-ID
           MOVE "Widget A" TO ITEM-NAME
           MOVE 100 TO ITEM-QUANTITY
           MOVE 9.99 TO ITEM-PRICE
           WRITE INVENTORY-RECORD
               INVALID KEY
                   DISPLAY "Error initializing inventory."
           END-WRITE

           MOVE 10002 TO ITEM-ID
           MOVE "Gadget B" TO ITEM-NAME
           MOVE 50 TO ITEM-QUANTITY
           MOVE 19.99 TO ITEM-PRICE
           WRITE INVENTORY-RECORD
               INVALID KEY
                   DISPLAY "Error initializing inventory."
           END-WRITE.

       GAME-LOOP.
           DISPLAY " "
           DISPLAY "1. View Inventory"
           DISPLAY "2. Add Stock"
           DISPLAY "3. Remove Stock"
           DISPLAY "4. Update Price"
           DISPLAY "5. Exit"
           DISPLAY "Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM VIEW-INVENTORY
               WHEN 2
                   PERFORM ADD-STOCK
               WHEN 3
                   PERFORM REMOVE-STOCK
               WHEN 4
                   PERFORM UPDATE-PRICE
               WHEN 5
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Invalid choice. Please try again."
           END-EVALUATE.

       VIEW-INVENTORY.
           DISPLAY " "
           DISPLAY "Current Inventory:"
           DISPLAY "ID    | Name                | Quantity | Price"
           DISPLAY "----------------------------------------------"
           MOVE "N" TO WS-EOF
           START INVENTORY-FILE KEY IS NOT LESS THAN ITEM-ID
               INVALID KEY
                   DISPLAY "Error starting inventory file."
           END-START
           PERFORM UNTIL WS-EOF = "Y"
               READ INVENTORY-FILE NEXT RECORD
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       DISPLAY ITEM-ID " | " 
                               ITEM-NAME " | "
                               ITEM-QUANTITY " | $"
                               ITEM-PRICE
               END-READ
           END-PERFORM.

       ADD-STOCK.
           DISPLAY "Enter Item ID to add stock: " WITH NO ADVANCING
           ACCEPT WS-ITEM-ID
           MOVE WS-ITEM-ID TO ITEM-ID
           READ INVENTORY-FILE
               INVALID KEY
                   DISPLAY "Item not found."
               NOT INVALID KEY
                   DISPLAY "Current quantity: " ITEM-QUANTITY
                   DISPLAY "Enter quantity to add: " WITH NO ADVANCING
                   ACCEPT WS-TEMP-QUANTITY
                   ADD WS-TEMP-QUANTITY TO ITEM-QUANTITY
                   REWRITE INVENTORY-RECORD
                       INVALID KEY
                           DISPLAY "Error updating inventory."
                   END-REWRITE
                   DISPLAY "Stock updated successfully."
           END-READ.

       REMOVE-STOCK.
           DISPLAY "Enter Item ID to remove stock: " WITH NO ADVANCING
           ACCEPT WS-ITEM-ID
           MOVE WS-ITEM-ID TO ITEM-ID
           READ INVENTORY-FILE
               INVALID KEY
                   DISPLAY "Item not found."
               NOT INVALID KEY
                   DISPLAY "Current quantity: " ITEM-QUANTITY
                   DISPLAY "Enter quantity to remove: " WITH NO ADVANCING
                   ACCEPT WS-TEMP-QUANTITY
                   IF WS-TEMP-QUANTITY > ITEM-QUANTITY
                       DISPLAY "Error: Not enough stock."
                   ELSE
                       SUBTRACT WS-TEMP-QUANTITY FROM ITEM-QUANTITY
                       REWRITE INVENTORY-RECORD
                           INVALID KEY
                               DISPLAY "Error updating inventory."
                       END-REWRITE
                       DISPLAY "Stock updated successfully."
                   END-IF
           END-READ.

       UPDATE-PRICE.
           DISPLAY "Enter Item ID to update price: " WITH NO ADVANCING
           ACCEPT WS-ITEM-ID
           MOVE WS-ITEM-ID TO ITEM-ID
           READ INVENTORY-FILE
               INVALID KEY
                   DISPLAY "Item not found."
               NOT INVALID KEY
                   DISPLAY "Current price: $" ITEM-PRICE
                   DISPLAY "Enter new price: $" WITH NO ADVANCING
                   ACCEPT WS-TEMP-PRICE
                   MOVE WS-TEMP-PRICE TO ITEM-PRICE
                   REWRITE INVENTORY-RECORD
                       INVALID KEY
                           DISPLAY "Error updating inventory."
                   END-REWRITE
                   DISPLAY "Price updated successfully."
           END-READ.

       END-GAME.
           CLOSE INVENTORY-FILE
           DISPLAY "Thank you for playing the Inventory Management Game!"
           DISPLAY "Exiting...".