       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTVENDR.
       DATE-WRITTEN. 4/16/2014.
       AUTHOR. MORGAN S
      ****************************************************************
      ****************************************************************
      *Purpose: To interactively manage our vendor master file with 
      *         input from the vendor. The program adds, edits,
      *         deletes, shows the details of a specific vendor and
      *         list all the vendors in the master file.
      *
      *Input:   The vendor master file contains the fields vendor
      *         id, vendor name, contact first name, contact last
      *         name, address street 1, street 2, city, county, zip
      *         code, phone number, balance, date last modified,
      *         and the active state of the acount. This is the file
      *         that the user can add edit and delete records from. 
      *
      *Output:  None
      *
      *Lead Developer: Sean Morgan
      ****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * The select statement that Add, Edit and Delete use.
           SELECT MASTER 
                 ASSIGN TO UT-SYS-MFILE
                 ORGANIZATION IS INDEXED
                 ACCESS IS DYNAMIC
                 RECORD KEY IS MAS-ACCOUNTNO
                 FILE STATUS IS WS-FILE-IS.
                 
       DATA DIVISION.
       FILE SECTION.
      * 
       FD  MASTER
         LABEL RECORDS ARE STANDARD
          RECORD CONTAINS 210 CHARACTERS.
       01  REC.
            05  MAS-ACCOUNTNO                PIC X(6).
            05  MAS-VENDOR-NAME              PIC X(30).
            05  MAS-CONTAC-PERSON.
                10   MAS-FNAME               PIC X(15).
                10   MAS-LNAME               PIC X(20).
            05  MAS-ADRESS.
                10   MAS-STREET              PIC X(30).
                10   MAS-STREET-TWO          PIC X(30).
                10   MAS-CITY                PIC X(20).
                10   MAS-COUNTRY             PIC X(20).
                10   MAS-ZIP                 PIC X(10).
            05  MAS-PHONENO                  PIC 9(12).
            05  MAS-BALANCE                  PIC 9(6)V99.
            05  MAS-DATE-BAL-CHANGE.
                10   MAS-YEAR                PIC 9(4).
                10   MAS-MONTH               PIC 9(2).
                10   MAS-DAY                 PIC 9(2).
            05  MAS-IS-ACTIVE                PIC X.
      *
       WORKING-STORAGE SECTION.
       
       01  WS-REC.
            05  WS-VENDOR-NAME           PIC X(30).
            05  WS-CONTAC-PERSON.
                10   WS-FNAME            PIC X(15).
                10   WS-LNAME            PIC X(20).
            05  WS-ADRESS.
                10   WS-STREET           PIC X(30).
                10   WS-STREET-TWO       PIC X(30).
                10   WS-CITY             PIC X(20).
                10   WS-COUNTRY          PIC X(20).
                10   WS-ZIP              PIC X(10).
            05  WS-PHONENO               PIC 9(12).
            05  WS-BALANCE               PIC 9(6)V99.
            05  WS-DATE-BAL-CHANGE.
                10   WS-YEAR             PIC 9(4).
                10   WS-MONTH            PIC 9(2).
                10   WS-DAY              PIC 9(2).             
            05  WS-IS-ACTIVE                 PIC X.
       
       01 WS-FILE-LO.
            05 UT-SYS-MFILE                  PIC X(50)
                VALUE "C:\Temp\INVOLDMASTER.DAT".
            05 UT-SYS-TEMP                   PIC X(50)
                VALUE "C:\Temp\INVOLDMASTER.DAT".
            05 UT-SYS-LINUX                  PIC X(50)
                VALUE "/tmp/INVOLDMASTER.DAT".
          
       01 WS-WORKING-AREA.
            05 WS-FILE-IS                    PIC 9(2).
                88 NOT-OPEN             VALUE 35.
                88 END-OF-FILE          VALUE 23.
            05 WS-PAUSE                      PIC X.
            05 MASTER-EOF                    PIC X     VALUE 'N'.
           
            05 WS-USER-SELECT                PIC 9.
                88 WS-ONE               VALUE 1.
                88 WS-TWO               VALUE 2.
                88 WS-THR               VALUE 3.
                88 WS-FOU               VALUE 4.
                88 WS-FIV               VALUE 5.
                88 WS-SIX               VALUE 6.

       01  WS-SWITCHES.
            05 WS-CREATE-FILE               PIC X
                                       VALUE 'N'.
                88 CREATE-FILE         VALUE 'Y'.
            05 WS-READY-TO-OPEN              PIC X
                                       VALUE 'N'.
                88 OPEN-THAT           VALUE 'Y'.
            05 WS-UPDATE-FLAG                PIC X
                                        VALUE 'N'.
                88 UPDATED              VALUE 'Y'.
            05 FILE-OPEN                     PIC X
                                        VALUE 'N'.
                88 OPEN-F               VALUE 'Y'.
            05 WS-STOP-PROGRAM               PIC X
                                        VALUE 'N'.
                88 OK-TO-STOP           VALUE 'Y'.
            05 WS-RECORD-NOT-FOUND           PIC X
                                        VALUE 'F'.
                88 VENDOR-NOT-FOUND     VALUE 'T'.
            05 WS-OK-TO-ADD                  PIC X
                                        VALUE 'N'.
                88 ASK-AGAIN            VALUE 'y'.
                88 OK-TO-ADD            VALUE 'Y'.
            05 WS-OK-TO-DELETE               PIC X
                                        VALUE 'N'.
                88 OK-TO-DELETE         VALUE 'Y'.
       01 WS-STRINGS.
            05 SEE-YA                               PIC X(18)
                VALUE "SEE YOU NEXT TIME.".
            05 DIVIDER                              PIC X(47)
                VALUE "===============================================".
            05 SELECT-FILE-LO                       PIC X(34)
                VALUE "Select Vendor Master File Location".
            05 START-B                              PIC X(3)
                VALUE "---".
            05 END-B                                PIC X(3)
                VALUE "---".
            05 WIN                                  PIC X(13)
                VALUE "Windows Users".
            05 LO-TEMP                              PIC X(6)
                VALUE "   1. ".
            05 LINUX                                PIC X(11)
                VALUE "Linux Users".
            05 LO-HOME                              PIC X(6)
                VALUE "   2. ".
            05 OTHER-ENTERY                         PIC X(20)
                VALUE "Enter Other Location".
            05 LO-OTHER                             PIC X(11)
                VALUE "   3. Other".
            05 LO-CREATE                            PIC X(35)
                VALUE "   4. Create New Vendor Master File".
            05 LO-EXIT                              PIC X(10)
                VALUE "   5. Exit".
            05 LO-ENTER                             PIC X(15)
                VALUE "Enter Location:".
            05 LO-ERR-NOT-FOUND                     PIC X(21)
                VALUE "ERROR FILE NOT FOUND.".
            05 LO-ERR-TRY-AGAIN                     PIC X(30)
                VALUE "PLEASE MAKE A VALID SELECTION".
            05 LO-FOUND                             PIC X(10)
                VALUE "FILE FOUND".
            05 MAIN-MENU                            PIC X(16)
                VALUE "Vendor Main Menu".
            05 ADD-VENDOR                           PIC X(16)
                VALUE "   1. ADD VENDOR".
            05 EDIT-VENDOR                          PIC X(17)
                VALUE "   2. EDIT VENDOR".
            05 DELETE-VENDOR                        PIC X(19)
                VALUE "   3. DELETE VENDOR".
            05 DETAIL-VENDOR                        PIC X(19)
                VALUE "   4. DETAIL VENDOR".
            05 LIST-VENDOR                          PIC X(18)
                VALUE "   5. LIST VENDORS".
            05 EXIT-PROG                            PIC X(10)
                VALUE "   6. EXIT".
            05 ENTER-SELECTION                      PIC X(16)
                VALUE "ENTER SELECTION:".
            05 ENTER-VALID                          PIC X(30)
                VALUE "PLEASE ENTER A VALID SELECTION".
            05 ENTER-ID                             PIC X(18)
                VALUE "Enter Vendor ID#: ".
            05 ADD-CHECK                            PIC X(28)
                VALUE "OK TO ADD RECORD? (Y TO ADD)".
            05 ADD-CHECK-TWO                        PIC X(37)
                VALUE "LAST CHANCE TO ADD RECORD? (Y TO ADD)".
            05 NOT-ADDED                            PIC X(16)
                VALUE "RECORD NOT ADDED".
            05 ADDED-CONF                           PIC X(12)
                VALUE "RECORD ADDED".
            05 ERR-REC-EXISTS                       PIC X(41)
                VALUE "RECORD ALREADY EXISTS - NO ADD ATTEMPTED ".
            05 ERR-WRITE-REC                        PIC X(26)
                VALUE "ERROR IN WRITING RECORD - ".
            05 UPDATE-CONF                          PIC X(14)
                VALUE "RECORD UPDATED".
            05 UPD-MSG-ONE                          PIC X(34)
                VALUE "LEAVE FIELD BLANK IF NO CHANGE or ".
            05 UPD-MSG-TWO                          PIC X(31)
                VALUE "'-' to erase for select fields.".
            05 ENTER-NAME                           PIC X(38)
                VALUE "Enter New Vendor Name (30 Characters):".
            05 ENTER-CON-F                          PIC X(36)
                VALUE "Enter Contact First (15 Characters):".
            05 ENTER-CON-L                          PIC X(40)
                VALUE "Enter Contact Last Name (20 Characters):".
            05 ENTER-STREET-ONE                     PIC X(37)
                VALUE "Enter Street Address (30 Characters):".
            05 ENTER-STREET-TWO                     PIC X(45)
                VALUE "Enter Second Street Address (30 Characters-):".
            05 ENTER-CITY                           PIC X(27)
                VALUE "Enter City (20 Characters):".
            05 ENTER-COUNTRY                        PIC X(30)
                VALUE "Enter Country (20 Characters):".
            05 ENTER-ZIP                            PIC X(31)
                VALUE "Enter Zip Code (10 Digets Max):".
            05 ENTER-PHONE                          PIC X(34)
                VALUE "Enter Phone Number (12 Diget Max):".
            05 ENTER-BALANCE                        PIC X(38)
                VALUE "Enter Vendor Balance ($999999.99 Max):".
            05 DELETE-CHECK                         PIC X(39)
                VALUE "Ok to delete this record? (Y to delete)".
            05 DELETE-CONF                          PIC X(14)
                VALUE "RECORD DELETED".
            05 NOT-DELETE                           PIC X(18)
                VALUE "RECORD NOT DELETED".
            05 DIS-ID                               PIC X(11)
                VALUE "VENDOR ID: ".
            05 DIS-NAME                             PIC X(13)
                VALUE "VENDOR NAME: ".
            05 DIS-CONT-NAME                        PIC X(14)
                VALUE "CONTACT NAME: ".
            05 DIS-ADDRESS                          PIC X(8)
                VALUE "ADDRESS:".
            05 DIS-SPACE                            PIC X(9)
                VALUE "         ".
            05 CITY-COUNTRY                         PIC X(2)
                VALUE ", ".
            05 COUNTRY-ZIP                          PIC X(1)
                VALUE " ".
            05 DIS-PHONE                            PIC X(14)
                VALUE "PHONE NUMBER: ".
            05 DIS-DATE                             PIC X(25)
                VALUE "DATE MODIFIDE(YYYYMMDD): ".
            05 DIS-STATE                            PIC X(14)
                VALUE "ACTIVE STATE: ".
            05 ERR-READING-REC                      PIC X(26)
                VALUE "ERROR IN READING RECORD - ".
            05 ERR-REC-NOT-FOUND                    PIC X(18)
                VALUE "RECORD NOT ON FILE".
            05 ERR-DELETING-REC                     PIC X(27)
                VALUE "ERROR IN DELETING RECORD - ".
            05 LIST-HEADER                          PIC X(28)
                VALUE "ACCOUNT ID       VENDOR NAME".
            05 LIST-DIV                             PIC X(47)
                VALUE "------------     ------------------------------".
            05 LIST-SPACE                           PIC X(11)
                VALUE  "           ".
            05 LIST-CONT                            PIC X(24)
                VALUE "PRESS ENTER TO CONTINUE:".
            05 ON-REC                               PIC X(29)
                VALUE "There are no Vendors in file".
            05 LO-CREATED                           PIC X(27)
                VALUE "Vendor Master File Created".
      ****************************************************************
      ****************************************************************
       PROCEDURE DIVISION.

      ****************************************************************
      *  Controls the direction of program logic.
      ****************************************************************
       100-MAIN.
           PERFORM 150-FIND-FILE THRU 150-EXIT
                   UNTIL OPEN-F.
           PERFORM 200-MAKE-SELECTION THRU 200-EXIT
                   UNTIL OK-TO-STOP.
           PERFORM 9910-END-PROGRAM-RTN.
           DISPLAY SEE-YA.
           STOP RUN.
      
      ****************************************************************
      *  A menu the allows the user to select where the maser file is
      *  located.
      ****************************************************************
       150-FIND-FILE.
           DISPLAY SPACES.
           DISPLAY DIVIDER.
           DISPLAY SPACES.
           DISPLAY SELECT-FILE-LO.
           DISPLAY SPACES.
           DISPLAY START-B, WIN, END-B.
           DISPLAY LO-TEMP, UT-SYS-TEMP.
           DISPLAY SPACES.
           DISPLAY START-B, LINUX, END-B.
           DISPLAY LO-HOME, UT-SYS-LINUX.
           DISPLAY SPACES.
           DISPLAY START-B, OTHER-ENTERY, END-B.
           DISPLAY LO-OTHER.
           DISPLAY SPACES.
           DISPLAY LO-CREATE.
           DISPLAY SPACES.
           DISPLAY LO-EXIT.
           DISPLAY SPACES.
           DISPLAY ENTER-SELECTION.
           ACCEPT WS-USER-SELECT.
           PERFORM 175-FILE-LOGIC THRU 175-EXIT.
           IF OPEN-THAT
               PERFORM 9900-INITIALIZATION THRU 9900-EXIT
           END-IF.
           DISPLAY DIVIDER.
       150-EXIT.
           EXIT.
           
      ****************************************************************
      *  This is the logic that drives the selection from the user and
      *  does the file selection.
      ****************************************************************
       175-FILE-LOGIC.
           MOVE 'Y' TO WS-READY-TO-OPEN.
           EVALUATE TRUE
               WHEN WS-ONE  MOVE UT-SYS-TEMP  TO UT-SYS-MFILE
               WHEN WS-TWO  MOVE UT-SYS-LINUX TO UT-SYS-MFILE
               WHEN WS-THR  PERFORM 177-OTHER THRU 177-EXIT
               WHEN WS-FOU  PERFORM 176-CREATE THRU 176-EXIT
               WHEN WS-FIV  PERFORM 178-EXIT-PROG THRU 178-EXIT
               WHEN OTHER   DISPLAY ENTER-VALID
           END-EVALUATE.
       175-EXIT.
           EXIT.
           
      ****************************************************************
      *  This allows the user to enter a location of where the new
      *  vendor master file is located.
      **************************************************************** 
       176-CREATE.
           MOVE 'Y' TO WS-CREATE-FILE.
           PERFORM 177-OTHER THRU 177-EXIT.
       176-EXIT.
           EXIT.
           
      ****************************************************************
      *  This allows the user to enter a location that is not a 
      *  default location.
      ****************************************************************
       177-OTHER.
           DISPLAY LO-ENTER.
           ACCEPT UT-SYS-MFILE.
       177-EXIT.
           EXIT.
           
      ****************************************************************
      *  This changes all the swiches so that the program closes.
      ****************************************************************  
       178-EXIT-PROG.
           MOVE 'Y' TO WS-STOP-PROGRAM.
           MOVE 'N' TO WS-READY-TO-OPEN.
           MOVE 'Y' TO FILE-OPEN.
           MOVE 'Y' TO WS-STOP-PROGRAM.
       178-EXIT.
           EXIT.
           
      ****************************************************************
      *  The selection menu that the user interacts with to move
      *  around the program.
      ****************************************************************
       200-MAKE-SELECTION.
           DISPLAY SPACES.
           DISPLAY DIVIDER.
           DISPLAY SPACES.
           DISPLAY MAIN-MENU.
           DISPLAY SPACES.
           DISPLAY ADD-VENDOR.
           DISPLAY EDIT-VENDOR.
           DISPLAY DELETE-VENDOR.
           DISPLAY DETAIL-VENDOR.
           DISPLAY LIST-VENDOR.
           DISPLAY EXIT-PROG.
           DISPLAY SPACES.
           DISPLAY ENTER-SELECTION.
           ACCEPT WS-USER-SELECT.
           DISPLAY SPACES.
           PERFORM 250-PROCEDE THRU 250-EXIT.          
           DISPLAY DIVIDER.
       200-EXIT.
           EXIT.
      
      ****************************************************************
      *  The menu's selection control logic.
      ****************************************************************
       250-PROCEDE.
           EVALUATE TRUE 
               WHEN WS-ONE  PERFORM 1000-ADD-RECORD THRU 1000-EXIT
               WHEN WS-TWO  PERFORM 2000-EDT-RECORD THRU 2000-EXIT
               WHEN WS-THR  PERFORM 3000-RM-RECORD  THRU 3000-EXIT
               WHEN WS-FOU  PERFORM 4000-VW-RECORD  THRU 4000-EXIT
               WHEN WS-FIV  PERFORM 5000-LIST       THRU 5000-EXIT
               WHEN WS-SIX  MOVE 'Y' TO WS-STOP-PROGRAM
               WHEN OTHER   DISPLAY ENTER-VALID
           END-EVALUATE.
       250-EXIT.
           EXIT.
            
      ****************************************************************
      ****************************************************************
      *  Add Record to master file.                     
      ****************************************************************
       1000-ADD-RECORD.
            DISPLAY ENTER-ID.
            ACCEPT MAS-ACCOUNTNO.
            PERFORM 1100-READ-V-MASTER THRU 1100-EXIT.
            IF VENDOR-NOT-FOUND
                PERFORM 2320-UPDATE-REC THRU 2320-EXIT
                PERFORM 1200-MOVE-DATA THRU 1200-EXIT
                DISPLAY ADD-CHECK
                ACCEPT WS-OK-TO-ADD
                IF OK-TO-ADD
                    PERFORM 1001-WRITE-REC THRU 1001-EXIT
                ELSE IF ASK-AGAIN
                    DISPLAY ADD-CHECK-TWO
                    ACCEPT WS-OK-TO-ADD
                    IF OK-TO-ADD
                        PERFORM 1001-WRITE-REC THRU 1001-EXIT
                    ELSE
                        DISPLAY NOT-ADDED
                    END-IF
                ELSE
                    DISPLAY NOT-ADDED
                END-IF
            END-IF.
            MOVE 'T' TO WS-RECORD-NOT-FOUND.
            MOVE 'N' TO WS-OK-TO-ADD.
       1000-EXIT.
             EXIT.
       
      ***************************************************************
      *   I have the task of writing the record to the master file.
      ***************************************************************      
       1001-WRITE-REC.
       		WRITE REC
            	INVALID KEY 
                	PERFORM 2610-BAD-WRITE THRU 2610-EXIT
            END-WRITE
            DISPLAY ADDED-CONF.
       1001-EXIT.
            EXIT.
            
      ***************************************************************
      *   Read a record from the employee master file.
      ***************************************************************
       1100-READ-V-MASTER.
            READ MASTER
               INVALID KEY MOVE 'T' TO WS-RECORD-NOT-FOUND
               NOT INVALID KEY PERFORM 1300-FOUND THRU 1300-EXIT
            END-READ.
       1100-EXIT.
            EXIT.
           
      ***************************************************************
      *   Moves the Record from working storage th master file
      *   record.
      ***************************************************************
       1200-MOVE-DATA.
            MOVE WS-VENDOR-NAME TO MAS-VENDOR-NAME
            MOVE WS-FNAME TO MAS-FNAME
            MOVE WS-LNAME TO MAS-LNAME
            MOVE WS-STREET TO MAS-STREET
            MOVE WS-STREET-TWO TO MAS-STREET-TWO
            MOVE WS-CITY TO MAS-CITY
            MOVE WS-COUNTRY TO MAS-COUNTRY
            MOVE WS-ZIP TO MAS-ZIP
            MOVE WS-PHONENO TO MAS-PHONENO
            MOVE WS-BALANCE TO MAS-BALANCE.
            MOVE FUNCTION CURRENT-DATE (1:8) TO MAS-DATE-BAL-CHANGE.
            MOVE 'Y' TO MAS-IS-ACTIVE.
       1200-EXIT.
            EXIT.

      ***************************************************************
      *   Display error message when record not found in file.
      ***************************************************************
       1300-FOUND.
            DISPLAY ERR-REC-EXISTS, REC
            MOVE 'F' TO WS-RECORD-NOT-FOUND.
       1300-EXIT.
            EXIT.

      ****************************************************************
      ****************************************************************
      *  Edit the selected record in master file.
      ****************************************************************
       2000-EDT-RECORD.
            DISPLAY ENTER-ID.
            ACCEPT MAS-ACCOUNTNO.
            PERFORM 4100-READ-V-MASTER THRU 4100-EXIT.
            IF NOT VENDOR-NOT-FOUND
                PERFORM 2300-GET-DATA THRU 2300-EXIT
                REWRITE REC
                     INVALID KEY PERFORM 2610-BAD-WRITE THRU 2610-EXIT
                DISPLAY UPDATE-CONF
            END-IF.
       2000-EXIT.
            EXIT.
            
      ***************************************************************
      *   The main update sequence.
      ***************************************************************
       2300-GET-DATA.
       		PERFORM 2310-CLEAR-WS-REC THRU 2310-EXIT.
            PERFORM 4300-GET-DATA THRU 4300-EXIT.
            DISPLAY SPACES.
            DISPLAY SPACES.
            DISPLAY UPD-MSG-ONE.
            DISPLAY UPD-MSG-TWO.
            DISPLAY SPACES.
            PERFORM 2320-UPDATE-REC THRU 2320-EXIT.
            PERFORM 2330-REC-CHECK THRU 2330-EXIT.
       2300-EXIT.
            EXIT.
       
      ***************************************************************
      *   This clears out the working storage of the record.
      ***************************************************************  
       2310-CLEAR-WS-REC.
            MOVE SPACES TO WS-VENDOR-NAME
            MOVE SPACES TO WS-FNAME
            MOVE SPACES TO WS-LNAME
            MOVE SPACES TO WS-STREET
            MOVE SPACES TO WS-STREET-TWO
            MOVE SPACES TO WS-CITY
            MOVE SPACES TO WS-COUNTRY
            MOVE 0 TO WS-ZIP
            MOVE 0 TO WS-PHONENO
            MOVE 'N' TO WS-UPDATE-FLAG.
       2310-EXIT.
       		EXIT.
       	
      ***************************************************************
      *   The interactive promps to read in a records fields.
      ***************************************************************	
       2320-UPDATE-REC.
            DISPLAY ENTER-NAME.
            ACCEPT WS-VENDOR-NAME.
            DISPLAY ENTER-CON-F.
            ACCEPT WS-FNAME.
            DISPLAY ENTER-CON-L.
            ACCEPT WS-LNAME.
            DISPLAY ENTER-STREET-ONE.
            ACCEPT WS-STREET.
            DISPLAY ENTER-STREET-TWO.
            ACCEPT WS-STREET-TWO.
            DISPLAY ENTER-CITY.
            ACCEPT WS-CITY.
            DISPLAY ENTER-COUNTRY.
            ACCEPT WS-COUNTRY.
            DISPLAY ENTER-ZIP.
            ACCEPT WS-ZIP.
            DISPLAY ENTER-PHONE.
            ACCEPT WS-PHONENO.
            DISPLAY ENTER-BALANCE.
            ACCEPT WS-BALANCE.
       2320-EXIT.
            EXIT.
       
      ***************************************************************
      *   I check to see if there are any changes to the record.
      ***************************************************************	
       2330-REC-CHECK.
            IF WS-VENDOR-NAME NOT = SPACES
              MOVE WS-VENDOR-NAME TO MAS-VENDOR-NAME
              MOVE 'Y' TO WS-UPDATE-FLAG
            END-IF.
            IF WS-FNAME NOT = SPACES
              MOVE WS-FNAME TO MAS-FNAME
              MOVE 'Y' TO WS-UPDATE-FLAG
            END-IF.
            IF WS-LNAME NOT = SPACES
              MOVE WS-LNAME TO MAS-LNAME
              MOVE 'Y' TO WS-UPDATE-FLAG
            END-IF.
            IF WS-STREET NOT = SPACES
              MOVE WS-STREET TO MAS-STREET
              MOVE 'Y' TO WS-UPDATE-FLAG
            END-IF.
            IF WS-STREET-TWO = "-"
                MOVE SPACES TO MAS-STREET-TWO
                MOVE 'Y' TO WS-UPDATE-FLAG
            ELSE IF WS-STREET-TWO NOT = SPACES
              MOVE WS-STREET-TWO TO MAS-STREET-TWO
              MOVE 'Y' TO WS-UPDATE-FLAG
            END-IF.
            IF WS-CITY NOT = SPACES
              MOVE WS-CITY TO MAS-CITY
              MOVE 'Y' TO WS-UPDATE-FLAG
            END-IF.
            IF WS-COUNTRY NOT = SPACES
              MOVE WS-COUNTRY TO MAS-COUNTRY
              MOVE 'Y' TO WS-UPDATE-FLAG
            END-IF.
            IF WS-ZIP > 0
               MOVE WS-ZIP TO MAS-ZIP
              MOVE 'Y' TO WS-UPDATE-FLAG
            END-IF.
            IF WS-PHONENO > 0
               MOVE WS-PHONENO TO MAS-PHONENO
              MOVE 'Y' TO WS-UPDATE-FLAG
            END-IF.
            IF NOT WS-BALANCE = MAS-BALANCE
               MOVE WS-BALANCE TO MAS-BALANCE
              MOVE 'Y' TO WS-UPDATE-FLAG
            END-IF.
            IF UPDATED
               MOVE FUNCTION CURRENT-DATE TO MAS-DATE-BAL-CHANGE
            END-IF.
       2330-EXIT.
            EXIT.

      ***************************************************************
      *   Display error message and the record in error.
      ***************************************************************
       2610-BAD-WRITE.
            DISPLAY ERR-WRITE-REC, REC.
       2610-EXIT.
            EXIT.

      ****************************************************************
      ****************************************************************
      *  Delete the selected record from master file.                    
      ****************************************************************
       3000-RM-RECORD.
            DISPLAY Enter-ID.
            ACCEPT MAS-ACCOUNTNO.
            PERFORM 4100-READ-V-MASTER THRU 4100-EXIT.
            IF NOT VENDOR-NOT-FOUND
                PERFORM 4300-GET-DATA THRU 4300-EXIT
                DISPLAY DELETE-CHECK
                ACCEPT WS-OK-TO-DELETE
                IF OK-TO-DELETE
                    DELETE MASTER RECORD
                        INVALID KEY
                            PERFORM 3610-NOT-FOUND THRU 3610-EXIT
                    END-DELETE
                    DISPLAY DELETE-CONF
                ELSE
                    DISPLAY NOT-DELETE
                END-IF
            END-IF.
       3000-EXIT.
            EXIT.

      ***************************************************************
      *   Display error message and the record in error.
      ***************************************************************
       3610-NOT-FOUND.
            DISPLAY ERR-DELETING-REC, REC.
       3610-EXIT.
            EXIT.
            
      ****************************************************************
      ****************************************************************
      *   I am an added feature that shows a selected vendor's
      *   information.
      ****************************************************************
       4000-VW-RECORD.
            DISPLAY Enter-ID.
            ACCEPT MAS-ACCOUNTNO.
            PERFORM 4100-READ-V-MASTER THRU 4100-EXIT.
            IF NOT VENDOR-NOT-FOUND
                DISPLAY SPACES
                PERFORM 4300-GET-DATA THRU 4300-EXIT
                DISPLAY SPACES
                DISPLAY LIST-CONT
                ACCEPT WS-PAUSE
            END-IF.
       4000-EXIT.
            EXIT.
       
      ***************************************************************
      *   Read a record from the employee master file.
      ***************************************************************
       4100-READ-V-MASTER.
            MOVE 'T' TO WS-RECORD-NOT-FOUND
            READ MASTER
               INVALID KEY PERFORM 4600-NOT-FOUND THRU 4600-EXIT
               NOT INVALID KEY MOVE 'F' TO WS-RECORD-NOT-FOUND
            END-READ.
       4100-EXIT.
            EXIT.
            
      ***************************************************************
      *   I display the record the the user.
      ***************************************************************
       4300-GET-DATA.
            DISPLAY SPACES.
            DISPLAY DIS-ID, MAS-ACCOUNTNO.
            DISPLAY DIS-NAME, MAS-VENDOR-NAME.
            DISPLAY DIS-CONT-NAME, MAS-FNAME, MAS-LNAME.
            DISPLAY DIS-ADDRESS.
            DISPLAY DIS-SPACE, MAS-STREET.
            DISPLAY DIS-SPACE, MAS-STREET-TWO.
            DISPLAY DIS-SPACE, MAS-CITY, CITY-COUNTRY, MAS-COUNTRY,
                        COUNTRY-ZIP, MAS-ZIP.
            DISPLAY DIS-PHONE, MAS-PHONENO.
            DISPLAY DIS-DATE, MAS-DATE-BAL-CHANGE.
            DISPLAY DIS-STATE, MAS-IS-ACTIVE.
            DISPLAY SPACES.
       4300-EXIT.
            EXIT.
            
      ***************************************************************
      *   Display error message when record not found in file.
      ***************************************************************
       4600-NOT-FOUND.
            DISPLAY ERR-READING-REC, MAS-ACCOUNTNO.
            DISPLAY ERR-REC-NOT-FOUND.
            MOVE 'T' TO WS-RECORD-NOT-FOUND.
       4600-EXIT.
            EXIT.
      
      ****************************************************************
      ****************************************************************
      *   I am an added feature that lists out all the records in the
      *   master file.
      ****************************************************************
       5000-LIST.
            MOVE 'N' TO MASTER-EOF.
            MOVE SPACES TO MAS-ACCOUNTNO.
            START MASTER KEY > MAS-ACCOUNTNO.
            IF NOT END-OF-FILE
                DISPLAY LIST-HEADER
                DISPLAY LIST-DIV
                READ MASTER NEXT RECORD
                    AT END MOVE 'Y' TO MASTER-EOF
                END-READ
      *
                PERFORM UNTIL MASTER-EOF = 'Y'
                    DISPLAY MAS-ACCOUNTNO, LIST-SPACE, MAS-VENDOR-NAME
                    READ MASTER NEXT RECORD
                        AT END MOVE 'Y' TO MASTER-EOF
                    END-READ
                END-PERFORM
      *
                DISPLAY SPACES
            ELSE
                DISPLAY ON-REC
            END-IF.
            DISPLAY LIST-CONT.
            ACCEPT WS-PAUSE.
       5000-EXIT.
            EXIT.
            
      ****************************************************************
      ****************************************************************
      *  Open the vendor master. If check to see if it is going to 
      *  create a new file and creates it in the user defined 
      *  location. Or it open the file is input mode becauce this way
      *  no new file will be created. Then it checks the file status
      *  and if 00 then is closes it and re-opens if in I-O. else it 
      *  gives a error code of 23 and then it didplays a message to 
      *  the user and the file menu is displayed again for a valid 
      *  file location.
      ****************************************************************
       9900-INITIALIZATION.
            IF CREATE-FILE
                OPEN I-O MASTER
                DISPLAY LO-CREATED 
                MOVE 'Y' TO FILE-OPEN
            ELSE
                OPEN INPUT MASTER
                IF NOT-OPEN
                    DISPLAY LO-ERR-NOT-FOUND
                    DISPLAY LO-ERR-TRY-AGAIN
                ELSE
                    CLOSE MASTER
                    OPEN I-O MASTER
                    DISPLAY LO-FOUND 
                    MOVE 'Y' TO FILE-OPEN
                END-IF
            END-IF.
       9900-EXIT.
            EXIT.

      ****************************************************************
      *   Close the vendor master file.
      ****************************************************************
       9910-END-PROGRAM-RTN.
            CLOSE MASTER.
       9910-EXIT.
           EXIT.
