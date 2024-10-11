# Inventory Management Game

This COBOL program simulates a simple inventory management system, demonstrating basic concepts used in real-world applications on AS/400 and IBM i systems.

## Purpose

The purpose of this program is to:

1. Showcase COBOL programming on AS/400 and IBM i systems.
2. Demonstrate basic inventory management operations.
3. Illustrate the use of indexed files for data storage and retrieval.

## Relation to AS/400 and IBM i5 Systems

This program is designed to run on AS/400 and IBM i systems, which are known for their reliability and widespread use in business applications, especially in industries that require robust transaction processing and data management.

Key points:

- COBOL is a common language used on these systems for business applications.
- The program uses indexed file organization, which is efficient for data retrieval on AS/400 and IBM i systems.
- The interactive nature of the program mimics real-world applications used in inventory management on these systems.

## Compiling and Running the Program

To compile and run this program on an AS/400 or IBM i system:

1. Transfer the COBOL source file (INVENTORY.CBL) to the IFS (Integrated File System) or a source physical file member on the AS/400 or IBM i system.

2. Open a 5250 emulation session and sign in to the system.

3. To compile the program, use the CRTCBLPGM (Create COBOL Program) command:

   ```
   CRTCBLPGM PGM(MYLIB/INVENTORY) SRCFILE(MYLIB/QCBLLESRC) SRCMBR(INVENTORY)
   ```

   Replace MYLIB with your library name, and adjust the source file name if necessary.

4. To run the program, use the CALL command:

   ```
   CALL PGM(MYLIB/INVENTORY)
   ```

5. Follow the on-screen prompts to interact with the inventory management game.

## Features

- View current inventory
- Add stock to existing items
- Remove stock from existing items
- Update item prices
- Simple menu-driven interface
