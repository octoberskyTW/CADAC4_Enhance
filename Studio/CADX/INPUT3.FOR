      PROGRAM INPUT3
C
C----------------------------------------------------------------------
C
C     INPUT Version 3.0                                  June      2000
C     INPUT Version 2.4                                  February  2000
C     INPUT Version 2.3                                  August    1998
C     INPUT Version 2.2                                  August    1998
C     INPUT Version 2.1                                  June      1998
C     INPUT Version 2.0                                  September 1996
C     INPUT Version 1.0                                  May       1996
C
C----------------------------------------------------------------------
C
C     This program translates a user created fixed format cadac input
C     file (CADIN.ASC) into a file with the free form input style
C     (INPUT.ASC).
C     - PC, Version May 1996
C
C-----------------------------------------------------------------------
C
C     Created by:  TYBRIN Corporation
C                  1030 Titan Court
C                  Ft. Walton Beach, Florida
C                                  32547
C
C                  Voice: (850) 337-2500
C                  Fax:   (850) 337-2534
C
C     Created for: AFRL/MNG
C                  Eglin Air Force Base, Florida
C                                       32542-6817
C
C                  Voice: (850) 882-3722
C                  Fax:   (850) 882-9049
C
C--Program History------------------------------------------------------
C
C     INPUT3                                                  June  2000
C         - Version number increase with new release of new CADAC Studio
C
C     INPUT24                                              February 2000
C         - Modified CARD04 to allow comments records to be full length.
C         - Error messages written to ERROR.ASC will also be displayed
C           on the screen.
C
C     INPUT23                                              August  1998
C         - Changed the definition of "TIME" at Dr. Zipfel's request.
C		- Modified how "NUM" is handled in SWEEP block.  "NUM" is printed
C		  to "Input.asc" file regardless.
C     INPUT22                                              August  1998
C         - Removed screen scroll at Dr. Zipfel's request.
C     INPUT21                                              June  1998
C         - Modified how "NUM" variable in Sweep Block is
C           handled.  Cases 0-3 "NUM" will not be printed
C           to "Input.asc" because "CADIN" calculates it.
C           For cases 4&5, "NUM" will be printed because
C           "NUM" used in "CADIN" will be provided by the user.
C
C--File Usage ----------------------------------------------------------
C
C   Unit   Ref      Name                        Description
C
C    21   JERROR   JERROR.ASC - Error output file
C    31   INPUT    CADIN.ASC  - fixed-form file
C    33   IOUTPUT  INPUT.ASC  - free-form file
C    40   IHEAD    HEADER.ASC - contains variable locations and definations
C    41   NUHEAD   NUHEAD.ASC - expanded temp file 
C  
C--Module definition----------------------------------------------------
C
C   This module is the MAIN controlling module for the INPUT 
C   program.
C
C--Alphabetical Modules List--------------------------------------------
C
C  CARD01        - Controls Type 01 SAVE card
C  CARD02        - Controls Type 02 MODULE Card
C  CARD03        - Controls Type 03 ASSIGNMENT card
C  CARD04        - Controls Type 04 COMMENT card
C  CARD05        - Controls Type 05 MULTI-RUN card
C  CARD06        - Controls Type 06 RUN card
C  CARD07        - Controls Type 07 VECTOR card
C  CARD08        - Controls Type 08 WX card
C  CARD09        - Controls Type 09 HEADER card
C  CARD10        - Controls Type 10 STAGE card
C  CARD11        - Controls Type 11 FUNCTION card
C  CARD12        - Controls Type 12 LOAD card
C  CARD13        - Controls Type 13 STOP card
C  CARD16        - Controls Type 16 SWEEP card
C  CARD19        - Controls Type 19 SWEEP card
C  CARD20        - Controls Type 20 SWEEP card
C  CARD21        - Controls Type 21 SWEEP card
C  CARD90        - Controls Type 90 SAVE card
C  CLEAR_SCREEN  - Clear the screen
C  CLOSE_FILES   - File control
C  CODE_LINES    - Controls card type assignment
C  COPY_HEAD     - Copies Header file into working file
C  END_MSG_STOP  - End of data message and exit
C  FILE_COPY     - Copy a file
C  FIND_VARIABLE - Locate a variable on a file
C  GET_FILE      - File selection and verivication
C  GET_WK_STRING - Reads the card
C  ISSUE_ERR_MSG - Controls error messages
C  LAST_CHAR     - Finds the last character of first word
C  LENSTR        - Determine the lenght of a strin
C  NXT_CHAR      - Find the next character in a string
C  PREPARE_FILES - I/O for files
C  PUT_TITLE     - Controls title placement
C  READ_INTERNAL - PC version of Vax internal read for reals
C  READ_INTEGER  - Pc version of Vax internal read for integers
C  SET_FILE_IDS  - Set up file unit numbers
C  STR_UPCASE    - Convert to uppercase
C
C-----------------------------------------------------------------------
C
      WRITE(*,'(5X, A)')'INPUT - Version 3.0'
C
C   Prompt for and control file
C
      CALL PREPARE_FILES
C
C   Copy HEADER.ASC data to working file
C
      CALL COPY_HEAD 
C
      WRITE(*,'(/ 5X, A /)' ) 
     1  'Now Processing Cards of Input file!'  
C
C   Put title from CADIN.ASC on Free Form output file
C
      CALL PUT_TITLE
C
C  Codes the Fixed form records into Free Form records
C      
      CALL CODE_LINES
C
C  Shut down and rename files
C
      CALL CLOSE_FILES
C
      STOP '  ---- INPUT Program Completed ----'
      END
      SUBROUTINE CARD01
C
C----------------------------------------------------------------------
C
C    Card type 01 - Subroutine Selection
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132
      CHARACTER INLINE*132, CODE*2
      LOGICAL END_OF_FILE
C
C  Not used at this time
C
      CODE = INLINE(1:2)
      END_OF_FILE = .FALSE.
C
      RETURN
      END
      SUBROUTINE CARD02 ( CODE )
C
C----------------------------------------------------------------------
C
C    Card type 02 - a "MODULE" card
C
C----------------------------------------------------------------------
C
C     This module determines the modules that have been entered and 
C     includes them in the freeform file.
C
C----------------------------------------------------------------------
C
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      COMMON /FILE_IDS/ INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
C
      CHARACTER INLINE*132, MOD_NAME*15, OUTLINE*132, 
     1          VALID_MODULES*3, MODULE_NAMES*15, CODE*2
      LOGICAL MOD_FOUND, END_OF_FILE
      DIMENSION VALID_MODULES(2:35), MODULE_NAMES(2:35)
C
      DATA VALID_MODULES / 'A1', 'A2', 'A3', 'A4', 'A5',
     1                     'C1', 'C2', 'C3', 'C4', 'C5',
     2                     'zz', 'zz', 'zz', 'zz', 'zz',
     3                     'D1', 'D2', 'D3', 'D4', 'D5',
     4                     'G1', 'G2', 'G3', 'G4', 'G5',
     5                     'zz',
     6                     'S1', 'S2', 'S3', 'S4', 'S5',
     7                     'zz', 'zz', 'zz' /
C
      DATA MODULE_NAMES / 'AERO COEF', 'PROPULSION', 'FORCES', ' ', ' ',
     1                    'GUIDANCE', 'AUTOPILOT', ' ', 'ACTUATOR', ' ',
     2                    ' ', ' ', ' ', ' ', ' ',
     3                    'TRANSLATION','ROTATION','SWEEP','DEBUG',' ',
     4                    'TARGET', 'AIR DATA', ' ', 'TERMINAL', ' ',
     5                    'KINEMATICS',
     6                    'SEEKER', 'SEEKER', 'NAV FILTER', 'INS', ' ',
     7                    ' ', ' ', ' ' /
C
C---  Initialize the output line.
C
      OUTLINE = ' '
C
C  Start writing output data
C
      WRITE(IOUTPUT,'(A)') "MODULES"
C
C  Search for module names
C
      DO WHILE( INLINE(1:2) .EQ. '02' )
C
        I = 2
        MOD_FOUND = .FALSE.
        DO WHILE (.NOT. MOD_FOUND .AND. I .LE.  35)   ! there are 35 MODULEs
          IF( INDEX(INLINE, VALID_MODULES(I)) .EQ. 4 ) THEN
            MOD_FOUND = .TRUE.
          ELSE
            I = I + 1
          ENDIF
        ENDDO
C
        IF (.NOT. MOD_FOUND) THEN
          CALL ISSUE_ERR_MSG( 'INVALID MODULE NAME' )          
C
        ELSE
C         
          IF (INLINE(8:19) .EQ. " ") THEN
C---      Use the default module name.
            MOD_NAME = MODULE_NAMES(I)
          ELSE
            MOD_NAME = INLINE(8:19)
          ENDIF
          OUTLINE(2:4) = INLINE(4:5)
          OUTLINE(7:18) = MOD_NAME
          WRITE(IOUTPUT,'(A)') OUTLINE(1:LENSTR(OUTLINE))
C
        ENDIF          
C
        CALL GET_WK_STRING(INLINE, END_OF_FILE )
        IF(END_OF_FILE) CALL END_MSG_STOP
     1      ('PREMATURE END OF FILE REACHED')
C
      ENDDO     !   end of do while loop
C     
      WRITE(IOUTPUT,'(A)') "END"
C
C   Set the code type to 00 so that the calling routine knows
C   that a value has been preloaded for INLINE and not to read 
C   additional data until this data point has been processed
C
      CODE = '00'
C
      RETURN
      END      
      SUBROUTINE CARD03
C
C----------------------------------------------------------------------
C
C    Card type 03 - a "VARIABLE ASSIGNMENT" card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132
      CHARACTER INLINE*132, CHKEQ*6, STAGE*2, VAR*13, OPT*3, VAR2*13
      CHARACTER TLOC_IN_C*4, TLOC_IN_C2*4, SBSR*6
      CHARACTER VALU*14, PARM1*14, PARM2*14, DEFINIT*100, VARDEFI*100
      LOGICAL FOUND
C
C  Process the record
C
      OUTLINE = ' '
      READ(INLINE(4:11),100) VAR
  100 FORMAT(A8)
C
      READ(INLINE(22:25),125) TLOC_IN_C
  125 FORMAT(A4)
C  
C  
      READ(INLINE(61:62),150) STAGE
  150 FORMAT(A2)
C  
      IF (VAR .EQ. 'RANSEED') THEN
C---  The line read in was the RANDOM function
         OUTLINE(1:7) = "RANDOM("          
         READ(INLINE(32:45),200,ERR=900) VALU
  200    FORMAT(A14)
         IFVAL = NXT_CHAR(1, VALU)
         ILVAL = LAST_CHAR(IFVAL, VALU)
         VALU = VALU(IFVAL:ILVAL)
         IEND = 8 + ILVAL-1
         OUTLINE(8:IEND) = VALU
         READ(INLINE(47:49),250) OPT
  250    FORMAT(A3)
         IF(OPT .EQ. '0.0') THEN
           IF(STAGE .EQ. ' ') THEN
              OUTLINE(IEND+1:) = ",ONCE)"
              WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
           ELSE
              OUTLINE(IEND+1:IEND+14) = ",ONCE) STAGE "
              OUTLINE(IEND+15:) = STAGE
              WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
           ENDIF
         ELSEIF(OPT .EQ. '1.0') THEN
           IF(STAGE .EQ. ' ') THEN
             OUTLINE(IEND+1:) = ",EVERY)"
             WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
           ELSE
             OUTLINE(IEND+1:IEND+15) = ",EVERY) STAGE "
             OUTLINE(IEND+16:) = STAGE
             WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
           ENDIF
         ELSEIF(OPT .EQ. '2.0') THEN
           IF(STAGE .EQ. ' ') THEN
             OUTLINE(IEND+1:) = ",RUNGROUP)"
             WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
           ELSE
             OUTLINE(IEND+1:IEND+18) = ",RUNGROUP) STAGE "
             OUTLINE(IEND+19:) = STAGE
             WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
           ENDIF
         ELSEIF(OPT .EQ. '3.0') THEN      
           IF(STAGE .EQ. ' ') THEN
             OUTLINE(IEND+1:) = ",RUN)"
             WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
           ELSE
             OUTLINE(IEND+1:IEND+13) = ",RUN) STAGE "
             OUTLINE(IEND+14:) = STAGE
             WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
           ENDIF                           
         ENDIF
      ELSE
C
C  Not Randon function
C      
        READ(INLINE(15:20),500) CHKEQ
  500   FORMAT(A6)
C  
        CALL FIND_VARIABLE ( TLOC_IN_C, VAR, FOUND, DEFINIT)
        VARDEFI = DEFINIT
        IFVAR = NXT_CHAR(1, VAR)
        ILVAR = LAST_CHAR(IFVAR, VAR)
        VAR = VAR(IFVAR:ILVAR)
        OUTLINE(1:ILVAR) = VAR
        OUTLINE(ILVAR+1:ILVAR+4) = " = "
        ISTART = ILVAR+4
C 
        IF (FOUND) THEN
          IF (CHKEQ .EQ. 'EQUALS') THEN
C---    The line was a variable assignment to another variable              
            READ(INLINE(32:35),125) TLOC_IN_C2
            CALL FIND_VARIABLE ( TLOC_IN_C2, VAR2, FOUND, DEFINIT)
            IFVAR2 = NXT_CHAR(1, VAR2)
            ILVAR2 = LAST_CHAR(IFVAR2, VAR2)
            VAR2 = VAR2(IFVAR2:ILVAR2)
            OUTLINE(ISTART:ISTART+ILVAR2-1) = VAR2
            ISTART = ISTART+ILVAR2+1
C            
            IF (FOUND) THEN
              IF (STAGE .EQ. " ") THEN
                 IF(VARDEFI .EQ. " ") THEN
                   WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                 ELSE
                   IEND = LENSTR(OUTLINE)
                   IF(IEND .GT. 17) THEN
                     ISTART = IEND +3
                     OUTLINE(ISTART:ISTART) = "!"
                     OUTLINE(ISTART+2:) = VARDEFI
                   ELSE
                     OUTLINE(20:20) = "!"
                     OUTLINE(22:) = VARDEFI
                   ENDIF
                   WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                 ENDIF
              ELSE
                 OUTLINE(ISTART:ISTART+7) = " STAGE "
                 ISTART = ISTART+8
                 OUTLINE(ISTART:ISTART+2) = STAGE
                 IF(VARDEFI .EQ. " ") THEN
                   WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                 ELSE
                   IEND = LENSTR(OUTLINE)
                   IF(IEND .GT. 17) THEN
                     ISTART = IEND +3
                     OUTLINE(ISTART:ISTART) = "!"
                     OUTLINE(ISTART+2:) = VARDEFI
                   ELSE
                     OUTLINE(20:20) = "!"
                     OUTLINE(22:) = VARDEFI
                   ENDIF
                   WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                 ENDIF
              ENDIF
            ELSE
              CALL ISSUE_ERR_MSG("INVALID VARIABLE")
            ENDIF
          ELSE           
            IF ((CHKEQ .EQ. 'GAUSS') .OR. 
     1      (CHKEQ .EQ. 'UNIF') .OR. (CHKEQ .EQ. 'EXPO')
     1      .OR. (CHKEQ .EQ. 'RAYLEI') .OR. (CHKEQ .EQ. 'SIGN')) THEN
C
             READ(INLINE(32:45),550,ERR=900) PARM1
             IFPAR1 = NXT_CHAR(1, PARM1)
             ILPAR1 = LAST_CHAR(IFPAR1, PARM1)
             PARM1 = PARM1(IFPAR1:ILPAR1)
C             
  550        FORMAT(A14)
C             
             LEN_STRING = LENSTR(CHKEQ)
             OUTLINE(ISTART:ISTART+LEN_STRING-1) = CHKEQ
             ISTART = ISTART+LEN_STRING
C  
             IF ((CHKEQ .EQ. 'GAUSS') .OR.
     1        (CHKEQ .EQ. 'UNIF')) THEN
                 READ(INLINE(47:60),550,ERR=900) PARM2
                 IFPAR2 = NXT_CHAR(1, PARM2)
                 ILPAR2 = LAST_CHAR(IFPAR2, PARM2)
                 PARM2 = PARM2(IFPAR2:ILPAR2)
                 IF (STAGE .EQ. " ") THEN
                  OUTLINE(ISTART:ISTART)= "("
                  OUTLINE(ISTART+1:ISTART+ILPAR1) = PARM1
                  ISTART = ISTART+ILPAR1+1
                  OUTLINE(ISTART:ISTART) = ","
                  OUTLINE(ISTART+1:ISTART+ILPAR2) = PARM2
                  ISTART = ISTART+ILPAR2+1
                  OUTLINE(ISTART:ISTART) = ")"
                  IF(VARDEFI .EQ. " ") THEN
                    WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                  ELSE
                    IEND = LENSTR(OUTLINE)
                    IF(IEND .GT. 17) THEN
                      ISTART = IEND +3
                      OUTLINE(ISTART:ISTART) = "!"
                      OUTLINE(ISTART+2:) = VARDEFI
                    ELSE
                      OUTLINE(20:20) = "!"
                      OUTLINE(22:) = VARDEFI
                    ENDIF
                    WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                  ENDIF
                ELSE
                  OUTLINE(ISTART:ISTART)= "("
                  OUTLINE(ISTART+1:ISTART+ILPAR1) = PARM1
                  ISTART = ISTART+ILPAR1+1
                  OUTLINE(ISTART:ISTART) = ","
                  OUTLINE(ISTART+1:ISTART+ILPAR2) = PARM2
                  ISTART = ISTART+ILPAR2+1
                  OUTLINE(ISTART:ISTART+8) = ") STAGE "
                  OUTLINE(ISTART+9:ISTART+11) = STAGE
                  ISTART = ISTART+12
                  IF(VARDEFI .EQ. " ") THEN
                    WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                  ELSE
                    IEND = LENSTR(OUTLINE)
                    IF(IEND .GT. 17) THEN
                      ISTART = IEND +3
                      OUTLINE(ISTART:ISTART) = "!"
                      OUTLINE(ISTART+2:) = VARDEFI
                    ELSE
                      OUTLINE(20:20) = "!"
                      OUTLINE(22:) = VARDEFI
                    ENDIF
                    WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                  ENDIF
                ENDIF
             ELSE
              IF (STAGE .EQ. " ") THEN
                OUTLINE(ISTART:ISTART)= "("
                OUTLINE(ISTART+1:ISTART+ILPAR1) = PARM1
                ISTART = ISTART+ILPAR1+1
                OUTLINE(ISTART:ISTART) = ")"
                IF(VARDEFI .EQ. " ") THEN
                  WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                ELSE
                  IEND = LENSTR(OUTLINE)
                  IF(IEND .GT. 17) THEN
                    ISTART = IEND +3
                    OUTLINE(ISTART:ISTART) = "!"
                    OUTLINE(ISTART+2:) = VARDEFI
                  ELSE
                    OUTLINE(20:20) = "!"
                    OUTLINE(22:) = VARDEFI
                  ENDIF
                  WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                ENDIF
              ELSE
                OUTLINE(ISTART:ISTART)= "("
                OUTLINE(ISTART+1:ISTART+ILPAR1) = PARM1
                ISTART = ISTART+ILPAR1+1
                OUTLINE(ISTART:ISTART) = ","
                ISTART = ISTART+1
                OUTLINE(ISTART:ISTART+8) = ") STAGE "
                OUTLINE(ISTART+9:ISTART+11) = STAGE
                ISTART = ISTART+11
                IF(VARDEFI .EQ. " ") THEN
                  WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                ELSE
                  IEND = LENSTR(OUTLINE)
                  IF(IEND .GT. 17) THEN
                    ISTART = IEND +3
                    OUTLINE(ISTART:ISTART) = "!"
                    OUTLINE(ISTART+2:) = VARDEFI
                  ELSE
                    OUTLINE(20:20) = "!"
                    OUTLINE(22:) = VARDEFI
                  ENDIF
                  WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                ENDIF
              ENDIF
             ENDIF
            ELSE
             READ(INLINE(32:45),200,ERR=900) VALU
             IFVAL = NXT_CHAR(1, VALU)
             ILVAL = LAST_CHAR(IFVAL, VALU)
             VALU = VALU(IFVAL:ILVAL)
C             
             IF (INLINE(30:30) .NE. ' ') THEN
               READ(INLINE(32:37),660,ERR=900) SBSR
  660          FORMAT(A6)
               JCHEK = INDEX(SBSR,'.')
               IF(JCHEK .GT. 0) SBSR = SBSR(1:JCHEK-1)
                 IF(SBSR .EQ. " ") SBSR(1:1) = "0"
                 IFSBSR = NXT_CHAR(1,SBSR)
                 ILSBSR = LAST_CHAR(IFSBSR,SBSR)
                 SBSR = SBSR(IFSBSR:ILSBSR)
               IF (STAGE .EQ. " ") THEN
                  OUTLINE(ISTART:ISTART+4) = "INT("
                  OUTLINE(ISTART+4:ISTART+ILSBSR+4) = SBSR
                  ISTART = ISTART+ILSBSR+4
                  OUTLINE(ISTART:ISTART) = ")"
                  IF(VARDEFI .EQ. " ") THEN
                    WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                  ELSE
                    IEND = LENSTR(OUTLINE)
                    IF(IEND .GT. 17) THEN
                      ISTART = IEND +3
                      OUTLINE(ISTART:ISTART) = "!"
                      OUTLINE(ISTART+2:) = VARDEFI
                    ELSE
                      OUTLINE(20:20) = "!"
                      OUTLINE(22:) = VARDEFI
                    ENDIF
                    WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                  ENDIF
               ELSE
                  OUTLINE(ISTART:ISTART+4) = "INT("
                  OUTLINE(ISTART+4:ISTART+ILSBSR+4) = SBSR
                  ISTART = ISTART+ILSBSR+4
                  OUTLINE(ISTART:ISTART+8) = ") STAGE "
                  OUTLINE(ISTART+9:ISTART+11) = STAGE
                  ISTART = ISTART+11
                  IF(VARDEFI .EQ. " ") THEN
                    WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                  ELSE
                    IEND = LENSTR(OUTLINE)
                    IF(IEND .GT. 17) THEN
                      ISTART = IEND +3
                      OUTLINE(ISTART:ISTART) = "!"
                      OUTLINE(ISTART+2:) = VARDEFI
                    ELSE
                      OUTLINE(20:20) = "!"
                      OUTLINE(22:) = VARDEFI
                    ENDIF
                    WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                  ENDIF
               ENDIF
             ELSE                                
               IF (STAGE .EQ. " ") THEN
                  OUTLINE(ISTART:ISTART+ILVAL-1) = VALU
                  ISTART = ISTART+ILVAL
                  IF(VARDEFI .EQ. " ") THEN
                    WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                  ELSE
                    IEND = LENSTR(OUTLINE)
                    IF(IEND .GT. 17) THEN
                      ISTART = IEND +3
                      OUTLINE(ISTART:ISTART) = "!"
                      OUTLINE(ISTART+2:) = VARDEFI
                    ELSE
                      OUTLINE(20:20) = "!"
                      OUTLINE(22:) = VARDEFI
                    ENDIF
                    WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                  ENDIF
               ELSE
                  OUTLINE(ISTART:ISTART+ILVAL-1) = VALU
                  ISTART = ISTART+ILVAL
                  OUTLINE(ISTART:ISTART+7) = " STAGE "
                  OUTLINE(ISTART+8:ISTART+10) = STAGE
                  ISTART = ISTART+10
                  IF(VARDEFI .EQ. " ") THEN
                    WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                  ELSE
                    IEND = LENSTR(OUTLINE)
                    IF(IEND .GT. 17) THEN
                      ISTART = IEND +3
                      OUTLINE(ISTART:ISTART) = "!"
                      OUTLINE(ISTART+2:) = VARDEFI
                    ELSE
                      OUTLINE(20:20) = "!"
                      OUTLINE(22:) = VARDEFI
                    ENDIF
                    WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                  ENDIF
               ENDIF
             ENDIF
            ENDIF
          ENDIF              
        ELSE
          CALL ISSUE_ERR_MSG("INVALID VARIABLE DURING READ")
        ENDIF
      ENDIF
C
  900 CONTINUE
      GOTO 999
C
  910 CALL ISSUE_ERR_MSG("ERROR WRITING LINE TO FILE")
C
  999 RETURN
      END
      SUBROUTINE CARD04
C
C----------------------------------------------------------------------
C
C    Card type 04 - A "COMMENT" card
C
C----------------------------------------------------------------------
C
C     This module places the comment read from the input file in the
C     CADIN.ASC file as a type 4 card.
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132
      CHARACTER INLINE*132
C
      LEN_STRING = LENSTR( INLINE )
CXXbc      IF( LEN_STRING .GT. 18 ) LEN_STRING = 18
      IF( LEN_STRING .GT. 1 ) WRITE(IOUTPUT,100)
     1                        INLINE(NXT_CHAR(3,INLINE):LEN_STRING)
  100 FORMAT( '! ', A )
C
      RETURN
      END
      SUBROUTINE CARD05
C
C----------------------------------------------------------------------
C
C    Card type 05 - a "MULTI-RUN" card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132
      CHARACTER INLINE*132
C
      READ(INLINE(26:30),100,ERR=900) NUMRUNS
  100 FORMAT(I5)
C  
      WRITE(IOUTPUT,110, ERR=910) "MONTE ", NUMRUNS
  110 FORMAT(A6,I5)
      GOTO 999
C
  900 CALL ISSUE_ERR_MSG("ERROR READING NUMBER OF MONTE CARLO SAMPLES")
      GOTO 999
C
  910 CALL ISSUE_ERR_MSG("ERROR WRITING CARD 05 - MUNTI-RUN RECORD")
C
  999 RETURN
      END
      SUBROUTINE CARD06
C
C----------------------------------------------------------------------
C
C    Card type 06 - A "RUN" card 
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
C
      WRITE(IOUTPUT, 100) "RUN"
  100 FORMAT(A3)
C
      RETURN
      END
      SUBROUTINE CARD07
C
C----------------------------------------------------------------------
C
C    Card type 07 - a "VECTOR" card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER INLINE*132, OUTLINE*132, TIDIM*4
      CHARACTER VAR*15, VALU*10, DEFINIT*100, VARDEFI*100
      CHARACTER TLOC_IN_C*4
      LOGICAL FOUND
C      
      DIMENSION VALUE(400)
C
      OUTLINE = " "
C      
      READ(INLINE(22:25), '(A4)') TLOC_IN_C
      CALL FIND_VARIABLE ( TLOC_IN_C, VAR, FOUND, DEFINIT)
        IF( .NOT. FOUND) THEN
          CALL ISSUE_ERR_MSG ( 'INVALID VARIABLE' )
          RETURN
        ENDIF
      VARDEFI = DEFINIT
      IFVAR = NXT_CHAR(1, VAR)
      ILVAR = LAST_CHAR(IFVAR, VAR)
      VAR = VAR(IFVAR:ILVAR)
      IRPAR = INDEX(VAR, "(")
      IF(IRPAR .GT. 0) THEN
        VAR = VAR(1:IRPAR-1)
      ENDIF
C      
      READ(INLINE(32:40), '(A9)') VALU
      IFVALU = NXT_CHAR(1, VALU)
      ILVALU = LAST_CHAR(IFVALU, VALU)
      VALU = VALU(IFVALU:ILVALU)
C
      JCHEK = INDEX(INLINE(27:30), '-')
      IF (JCHEK .GT. 0) THEN
C        
C   VECTOR statement
C
        IF (INLINE(61:62) .EQ. '  ') THEN
C--- No stage                            
           OUTLINE(1:7) = "VECTOR "
           OUTLINE(8:ILVAR+7) = VAR
           ISTART = ILVAR+9
           OUTLINE(ISTART:ISTART+ILVALU-1) = VALU
           ISTART = ISTART+ILVALU
        ELSE
           OUTLINE(1:7) = "VECTOR "
           OUTLINE(8:ILVAR+7) = VAR
           ISTART = ILVAR+9
           OUTLINE(ISTART:ISTART+ILVALU-1) = VALU
           ISTART = ISTART+ILVALU
           OUTLINE(ISTART:ISTART+6) = " STAGE "
           ISTART = ISTART+7
           OUTLINE(ISTART:ISTART+2) = INLINE(61:62)
           ISTART = ISTART+3
        ENDIF
C
                 IF(VARDEFI .EQ. " ") THEN
                   WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                 ELSE
                   IEND = LENSTR(OUTLINE)
                   IF(IEND .GT. 17) THEN
                     ISTART = IEND +3
                     OUTLINE(ISTART:ISTART) = "!"
                     OUTLINE(ISTART+2:) = VARDEFI
                   ELSE
                     OUTLINE(20:20) = "!"
                     OUTLINE(22:) = VARDEFI
                   ENDIF
                   WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                 ENDIF
      ELSE
C
C   VECTORV statement
C
         OUTLINE(1:7) = "VECTORV "
         OUTLINE(9:ILVAR+8) = VAR
         ISTART = ILVAR+10
         IF (INLINE(61:62) .NE. '  ') THEN
           OUTLINE(ISTART:ISTART+5) = "STAGE "
           ISTART = ISTART+6
           OUTLINE(ISTART:ISTART+2) = INLINE(61:62)
           ISTART = ISTART+3
         ENDIF
C
                 IF(VARDEFI .EQ. " ") THEN
                   WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                 ELSE
                   IEND = LENSTR(OUTLINE)
                   IF(IEND .GT. 17) THEN
                     ISTART = IEND +3
                     OUTLINE(ISTART:ISTART) = "!"
                     OUTLINE(ISTART+2:) = VARDEFI
                   ELSE
                     OUTLINE(20:20) = "!"
                     OUTLINE(22:) = VARDEFI
                   ENDIF
                   WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                 ENDIF
C
         TIDIM=INLINE(27:30)           
         READ(TIDIM,300,ERR=900) IDIM
  300    FORMAT(I4)
         READ(INPUT,400,ERR=900) (VALUE(I),I=1,IDIM)
  400    FORMAT(2X,5(G13.7,1X))       
C
C---  Write the data to the INPUT.ASC file
C
         WRITE(IOUTPUT,400,ERR=910) (VALUE(I),I=1,IDIM)           
      ENDIF
      GOTO 999
C
  900 CALL ISSUE_ERR_MSG("ERROR READING CARD TYPE 7")
      GOTO 999
C
  910 CALL ISSUE_ERR_MSG("ERROR WRITING CARD TYPE 7 - VECTOR")
C  
  999 RETURN
      END
      SUBROUTINE CARD08
C
C----------------------------------------------------------------------
C
C    Card type 08 - A "WEATHER" "WINDS" etc card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132
      CHARACTER INLINE*132, VARB*15
      LOGICAL END_OF_FILE
C
      READ(INLINE(3:15),'(A)') VARB
      IF(VARB .EQ. " ") THEN
        WRITE(IOUTPUT,499) "WEATHER"
  499   FORMAT(A7)
      ELSE
        WRITE(IOUTPUT,500) "WEATHER", " ", "!", VARB
  500   FORMAT(A7,A1,A1,A)
      ENDIF
C      
      ICHKEND = 0
      DO WHILE (ICHKEND .LT. 1)
         CALL GET_WK_STRING(INLINE, END_OF_FILE)
         ICHKEND = INDEX(INLINE(1:10), '-1.0')
         IF(ICHKEND .GT. 0) THEN
           WRITE(IOUTPUT,'(A)') "END"
         ELSE
           WRITE(IOUTPUT,'(A)',ERR=910) INLINE
         ENDIF
      ENDDO
      GOTO 999
C
  910 CALL ISSUE_ERR_MSG("ERROR IN WEATHER DATA")    
C
  999 RETURN
      END
      SUBROUTINE CARD09
C
C----------------------------------------------------------------------
C
C    Card type 09 - a "HEADER" card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132
      CHARACTER INLINE*132
      LOGICAL END_OF_FILE
C
      KOUNT = 0      
C
      READ(INLINE(25:25),50,ERR=900) NUMHDRS
   50 FORMAT(I6)
      WRITE(IOUTPUT,100) "HEADER"
  100 FORMAT(A6)
C
      DO WHILE(KOUNT .LT. NUMHDRS)
        CALL GET_WK_STRING(INLINE, END_OF_FILE)
        WRITE(IOUTPUT,200,ERR=910) INLINE
  200   FORMAT(A)
        KOUNT = KOUNT + 1
      ENDDO
C
      WRITE(IOUTPUT,300) "END"
  300 FORMAT(A3)
      GOTO 999
C
  900 CALL ISSUE_ERR_MSG("ERROR READING # OF HEADER RECORDS")
      GOTO 999
C
  910 CALL ISSUE_ERR_MSG("ERROR WRITING HEADER RECORDS")
C
  999 RETURN
      END
      SUBROUTINE CARD10
C
C----------------------------------------------------------------------
C
C    Card type 10 - a "STAGE" card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132, VAL*15, DEFINIT*100, VARDEFI*100
      CHARACTER INLINE*132, TLOC_IN_C*4, CONDTYP*1, VAR*15, VAR2*15,
     1              VARI*15, OUTLINE0*35, OUTLINE1*35, OUTLINE2*35
      LOGICAL END_OF_FILE, FOUND
C
C  Check to see if there is one or two conditions
C
C          This is the single condition.
      OUTLINE = ' '
C      
      IF(INLINE(25:25) .EQ. '1') THEN
        OUTLINE(1:3) = "IF "
C      
C  Get the next card for the criteria
        CALL GET_WK_STRING(INLINE, END_OF_FILE)
C  Check to see if it is a comparison to a value,
C  a variable or an addition condition
C
C         This is the value condition.
        IF(INLINE(49:50) .EQ. '  ' .OR. INLINE(49:50) .EQ. ' 0') THEN        
           READ(INLINE(8:11),100,ERR=900) TLOC_IN_C
  100      FORMAT(A4)
           CALL FIND_VARIABLE ( TLOC_IN_C, VAR, FOUND, DEFINIT)
           VARDEFI = DEFINIT
           IFVAR = NXT_CHAR(1, VAR)
           ILVAR = LAST_CHAR(IFVAR, VAR)
           VAR = VAR(IFVAR:ILVAR)
           OUTLINE(4:ILVAR+4) = VAR
           ISTART = ILVAR+4
C           
           IF (FOUND) THEN
              READ(INLINE(16:16),200,ERR=900) CONDTYP
  200         FORMAT(A1)       
              READ(INLINE(24:38),250,ERR=900) VAL
  250         FORMAT(A15)
              IFVAL = NXT_CHAR(1, VAL)
              ILVAL = LAST_CHAR(IFVAL, VAL)
              VAL = VAL(IFVAL:ILVAL)
C              
              IF (CONDTYP .EQ. '1') THEN
                 OUTLINE(ISTART:ISTART+2) = " > "
                 OUTLINE(ISTART+3:ISTART+3+ILVAL-1) = VAL
                 ISTART = ISTART+3+ILVAL
                 IF(VARDEFI .EQ. " ") THEN
                   WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                 ELSE
                   IEND = LENSTR(OUTLINE)
                   IF(IEND .GT. 17) THEN
                     ISTART = IEND +3
                     OUTLINE(ISTART:ISTART) = "!"
                     OUTLINE(ISTART+2:) = VARDEFI
                   ELSE
                     OUTLINE(20:20) = "!"
                     OUTLINE(22:) = VARDEFI
                   ENDIF
                   WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                 ENDIF
              ELSE
                 OUTLINE(ISTART:ISTART+2) = " < "
                 OUTLINE(ISTART+3:ISTART+3+ILVAL-1) = VAL
                 ISTART = ISTART+3+ILVAL
                 IF(VARDEFI .EQ. " ") THEN
                   WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                 ELSE
                   IEND = LENSTR(OUTLINE)
                   IF(IEND .GT. 17) THEN
                     ISTART = IEND +3
                     OUTLINE(ISTART:ISTART) = "!"
                     OUTLINE(ISTART+2:) = VARDEFI
                   ELSE
                     OUTLINE(20:20) = "!"
                     OUTLINE(22:) = VARDEFI
                   ENDIF
                   WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                 ENDIF
              ENDIF      
           ELSE
              CALL ISSUE_ERR_MSG("INVALID VARIABLE")
              RETURN
           ENDIF
C         This is the variable condition.            
        ELSEIF(INLINE(49:50) .EQ. ' 1') THEN
           READ(INLINE(8:11),101,ERR=900) TLOC_IN_C
  101      FORMAT(A4)
           CALL FIND_VARIABLE ( TLOC_IN_C, VAR, FOUND, DEFINIT)
           VARDEFI = DEFINIT
           IFVAR = NXT_CHAR(1, VAR)
           ILVAR = LAST_CHAR(IFVAR, VAR)
           VAR = VAR(IFVAR:ILVAR)
           OUTLINE(4:ILVAR+4) = VAR
           ISTART = ILVAR+4
C
           IF (FOUND) THEN
              READ(INLINE(24:38),251,ERR=900) VARI
  251         FORMAT(A15)
              JCHEK = INDEX(VARI, '.')
              TLOC_IN_C = VARI(JCHEK-4:JCHEK-1)
              CALL FIND_VARIABLE ( TLOC_IN_C, VAR2, FOUND, DEFINIT)
              IFVAR2 = NXT_CHAR(1, VAR2)
              ILVAR2 = LAST_CHAR(IFVAR2, VAR2)
              VAR2 = VAR2(IFVAR2:ILVAR2)
C
                IF (FOUND) THEN
                  READ(INLINE(16:16),201,ERR=900) CONDTYP
  201             FORMAT(A1)       
                  IF (CONDTYP .EQ. '1') THEN
                     OUTLINE(ISTART:ISTART+2) = " > "
                     OUTLINE(ISTART+3:ISTART+3+ILVAR2-1) = VAR2
                     ISTART = ISTART+3+ILVAR2
                     IF(VARDEFI .EQ. " ") THEN
                       WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                     ELSE
                       IEND = LENSTR(OUTLINE)
                       IF(IEND .GT. 17) THEN
                         ISTART = IEND +3
                         OUTLINE(ISTART:ISTART) = "!"
                         OUTLINE(ISTART+2:) = VARDEFI
                       ELSE
                         OUTLINE(20:20) = "!"
                         OUTLINE(22:) = VARDEFI
                       ENDIF
                       WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                     ENDIF
                  ELSE
                     OUTLINE(ISTART:ISTART+2) = " < "
                     OUTLINE(ISTART+3:ISTART+3+ILVAR2-1) = VAR2
                     ISTART = ISTART+3+ILVAR2
                     IF(VARDEFI .EQ. " ") THEN
                       WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                     ELSE
                       IEND = LENSTR(OUTLINE)
                       IF(IEND .GT. 17) THEN
                         ISTART = IEND +3
                         OUTLINE(ISTART:ISTART) = "!"
                         OUTLINE(ISTART+2:) = VARDEFI
                       ELSE
                         OUTLINE(20:20) = "!"
                         OUTLINE(22:) = VARDEFI
                       ENDIF
                       WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                     ENDIF
                  ENDIF      
                ELSE
                  CALL ISSUE_ERR_MSG("INVALID VARIABLE")
                  RETURN
                ENDIF    
           ELSE
              CALL ISSUE_ERR_MSG("INVALID VARIABLE")
              RETURN
           ENDIF    
C         This is the addition condition.            
        ELSEIF(INLINE(49:50) .EQ. '-1') THEN
           READ(INLINE(8:11),102,ERR=900) TLOC_IN_C
  102      FORMAT(A4)
           CALL FIND_VARIABLE ( TLOC_IN_C, VAR, FOUND, DEFINIT)
           VARDEFI = DEFINIT
           IFVAR = NXT_CHAR(1, VAR)
           ILVAR = LAST_CHAR(IFVAR, VAR)
           VAR = VAR(IFVAR:ILVAR)
           OUTLINE(4:ILVAR+4) = VAR
           ISTART = ILVAR+4
C           
           IF (FOUND) THEN
              READ(INLINE(24:38),252,ERR=900) VARI
  252         FORMAT(A15)
              IFVARI = NXT_CHAR(1,VARI)      ! find first position of number
              ILVARI = LAST_CHAR(IFVARI,VARI) ! find last position of number
              JCHEK = INDEX(VARI(IFVARI:ILVARI), '-')
              READ(INLINE(16:16),202,ERR=900) CONDTYP
  202         FORMAT(A1)       
                IF (CONDTYP .EQ. '1') THEN
                   IF(JCHEK .GT. 0) THEN
                     OUTLINE(ISTART:ISTART+2) = " > "
                     OUTLINE(ISTART+3:ISTART+ILVAL+2) = VAR
                     ISTART = ISTART+ILVAL
                     OUTLINE(ISTART+1:ISTART+2) = " - "
                     OUTLINE(ISTART+4:ISTART+4+ILVARI-1) = 
     +                                            VARI(IFVARI+1:ILVARI)
                     ISTART = ISTART+4+ILVARI
                     IF(VARDEFI .EQ. " ") THEN
                       WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                     ELSE
                       IEND = LENSTR(OUTLINE)
                       IF(IEND .GT. 17) THEN
                         ISTART = IEND +3
                         OUTLINE(ISTART:ISTART) = "!"
                         OUTLINE(ISTART+2:) = VARDEFI
                       ELSE
                         OUTLINE(20:20) = "!"
                         OUTLINE(22:) = VARDEFI
                       ENDIF
                       WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                     ENDIF
                   ELSE                   
                     OUTLINE(ISTART:ISTART+2) = " > "
                     OUTLINE(ISTART+3:ISTART+ILVAL+2) = VAR
                     ISTART = ISTART+ILVAL
                     OUTLINE(ISTART+1:ISTART+2) = " + "
                     OUTLINE(ISTART+4:ISTART+4+ILVARI-1) = 
     +                                            VARI(IFVARI:ILVARI)
                     ISTART = ISTART+4+ILVARI
                     IF(VARDEFI .EQ. " ") THEN
                       WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                     ELSE
                       IEND = LENSTR(OUTLINE)
                       IF(IEND .GT. 17) THEN
                         ISTART = IEND +3
                         OUTLINE(ISTART:ISTART) = "!"
                         OUTLINE(ISTART+2:) = VARDEFI
                       ELSE
                         OUTLINE(20:20) = "!"
                         OUTLINE(22:) = VARDEFI
                       ENDIF
                       WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                     ENDIF
                   ENDIF
                ELSE
                   IF(JCHEK .GT. 0) THEN
                     OUTLINE(ISTART:ISTART+2) = " < "
                     OUTLINE(ISTART+3:ISTART+ILVAL+2) = VAR
                     ISTART = ISTART+ILVAL
                     OUTLINE(ISTART+1:ISTART+2) = " - "
                     OUTLINE(ISTART+4:ISTART+4+ILVARI-1) = 
     +                                            VARI(IFVARI+1:ILVARI)
                     ISTART = ISTART+4+ILVARI
                     IF(VARDEFI .EQ. " ") THEN
                       WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                     ELSE
                       IEND = LENSTR(OUTLINE)
                       IF(IEND .GT. 17) THEN
                         ISTART = IEND +3
                         OUTLINE(ISTART:ISTART) = "!"
                         OUTLINE(ISTART+2:) = VARDEFI
                       ELSE
                         OUTLINE(20:20) = "!"
                         OUTLINE(22:) = VARDEFI
                       ENDIF
                       WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                     ENDIF
                   ELSE                   
                     OUTLINE(ISTART:ISTART+2) = " < "
                     OUTLINE(ISTART+3:ISTART+ILVAL+2) = VAR
                     ISTART = ISTART+ILVAL
                     OUTLINE(ISTART+1:ISTART+2) = " + "
                     OUTLINE(ISTART+4:ISTART+4+ILVARI-1) = 
     +                                            VARI(IFVARI:ILVARI)
                     ISTART = ISTART+4+ILVARI
                     IF(VARDEFI .EQ. " ") THEN
                       WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                     ELSE
                       IEND = LENSTR(OUTLINE)
                       IF(IEND .GT. 17) THEN
                         ISTART = IEND +3
                         OUTLINE(ISTART:ISTART) = "!"
                         OUTLINE(ISTART+2:) = VARDEFI
                       ELSE
                         OUTLINE(20:20) = "!"
                         OUTLINE(22:) = VARDEFI
                       ENDIF
                       WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
                     ENDIF
                   ENDIF
                ENDIF      
           ELSE
              CALL ISSUE_ERR_MSG("INVALID VARIABLE")
              RETURN
           ENDIF
C       If cannot find value, variable or addition condition, issue error
C       message.                         
        ELSE
           CALL ISSUE_ERR_MSG("ERROR READING STAGE CRITERIA")
           RETURN
        ENDIF
C
C          This is the double condition.
      ELSE
        IF(INLINE(25:25) .EQ. '2') THEN
C        
          DO J = 1, 2
C      
C  Get the next card for the criteria
          CALL GET_WK_STRING(INLINE, END_OF_FILE)
C  Check to see if it is a comparison to a value,
C  a variable or an addition condition
C
C         This is the value condition.
          IF(INLINE(49:50) .EQ. '  ' .OR. INLINE(49:50) .EQ. ' 0') THEN
C        
            READ(INLINE(8:11),103,ERR=900) TLOC_IN_C
  103       FORMAT(A4)
            CALL FIND_VARIABLE ( TLOC_IN_C, VAR, FOUND, DEFINIT)
            VARDEFI = DEFINIT
            IFVAR = NXT_CHAR(1, VAR)
            ILVAR = LAST_CHAR(IFVAR, VAR)
            VAR = VAR(IFVAR:ILVAR)
            OUTLINE0(1:ILVAR) = VAR
            ISTART = ILVAR+1
C            
            IF (FOUND) THEN
              READ(INLINE(16:16),203,ERR=900) CONDTYP
  203         FORMAT(A1)       
              READ(INLINE(24:38),253,ERR=900) VAL
  253         FORMAT(A15)
              IFVAL = NXT_CHAR(1, VAL)
              ILVAL = LAST_CHAR(IFVAL, VAL)
              VAL = VAL(IFVAL:ILVAL)
C              
              IF (CONDTYP .EQ. '1') THEN
                 OUTLINE0(ISTART:ISTART+2) = " > "
                 OUTLINE0(ISTART+3:) = VAL
              ELSE
                 OUTLINE0(ISTART:ISTART+2) = " < "
                 OUTLINE0(ISTART+3:) = VAL
              ENDIF      
            ELSE
              CALL ISSUE_ERR_MSG("INVALID VARIABLE")
              RETURN
            ENDIF
C         This is the variable condition.            
          ELSEIF(INLINE(49:50) .EQ. ' 1') THEN
C
            READ(INLINE(8:11),104,ERR=900) TLOC_IN_C
  104       FORMAT(A4)
            CALL FIND_VARIABLE ( TLOC_IN_C, VAR, FOUND, DEFINIT)
            VARDEFI = DEFINIT
            IFVAR = NXT_CHAR(1, VAR)
            ILVAR = LAST_CHAR(IFVAR, VAR)
            VAR = VAR(IFVAR:ILVAR)
            OUTLINE0(1:ILVAR) = VAR
            ISTART = ILVAR+1
C            
            IF (FOUND) THEN
              READ(INLINE(24:38),254,ERR=900) VARI
  254         FORMAT(A15)
              JCHEK = INDEX(VARI, '.')
              TLOC_IN_C = VARI(JCHEK-4:JCHEK-1)
              CALL FIND_VARIABLE ( TLOC_IN_C, VAR2, FOUND, DEFINIT)
              IFVAR2 = NXT_CHAR(1, VAR2)
              ILVAR2 = LAST_CHAR(IFVAR2, VAR2)
              VAR2 = VAR2(IFVAR2:ILVAR2)
C                
                IF (FOUND) THEN
                  READ(INLINE(16:16),204,ERR=900) CONDTYP
  204             FORMAT(A1)       
                  IF (CONDTYP .EQ. '1') THEN
                     OUTLINE0(ISTART:ISTART+2) = " > "
                     OUTLINE0(ISTART+3:) = VAR2
                  ELSE
                     OUTLINE0(ISTART:ISTART+2) = " < "
                     OUTLINE0(ISTART+3:) = VAR2
                  ENDIF      
                ELSE
                  CALL ISSUE_ERR_MSG("INVALID VARIABLE")
                  RETURN
                ENDIF    
            ELSE
              CALL ISSUE_ERR_MSG("INVALID VARIABLE")
              RETURN
            ENDIF    
C         This is the addition condition.            
          ELSEIF(INLINE(49:50) .EQ. '-1') THEN
C
            READ(INLINE(8:11),105,ERR=900) TLOC_IN_C
  105       FORMAT(A4)
            CALL FIND_VARIABLE ( TLOC_IN_C, VAR, FOUND, DEFINIT)
            VARDEFI = DEFINIT
            IFVAR = NXT_CHAR(1, VAR)
            ILVAR = LAST_CHAR(IFVAR, VAR)
            VAR = VAR(IFVAR:ILVAR)
            OUTLINE0(1:ILVAR) = VAR
            ISTART = ILVAR+1
C            
            IF (FOUND) THEN
              READ(INLINE(24:38),255,ERR=900) VARI
  255         FORMAT(A15)
              IFVARI = NXT_CHAR(1,VARI)      ! find first position of number
              ILVARI = LAST_CHAR(IFVARI,VARI) ! find last position of number
              JCHEK = INDEX(VARI(IFVARI:ILVARI), '-')
              READ(INLINE(16:16),205,ERR=900) CONDTYP
  205         FORMAT(A1)       
                IF (CONDTYP .EQ. '1') THEN
                   IF(JCHEK .GT. 0) THEN
                     OUTLINE0(ISTART:ISTART+2) = " > "
                     OUTLINE0(ISTART+3:ISTART+ILVAL+2) = VAR
                     ISTART = ISTART+ILVAL
                     OUTLINE0(ISTART+1:ISTART+2) = " - "
                     OUTLINE0(ISTART+4:) = VARI(IFVARI+1:ILVARI)
                   ELSE                   
                     OUTLINE0(ISTART:ISTART+2) = " > "
                     OUTLINE0(ISTART+3:ISTART+ILVAL+2) = VAR
                     ISTART = ISTART+ILVAL
                     OUTLINE0(ISTART+1:ISTART+2) = " + "
                     OUTLINE0(ISTART+4:) = VARI(IFVARI+1:ILVARI)
                   ENDIF
                ELSE
                   IF(JCHEK .GT. 0) THEN
                     OUTLINE0(ISTART:ISTART+2) = " < "
                     OUTLINE0(ISTART+3:ISTART+ILVAL+2) = VAR
                     ISTART = ISTART+ILVAL
                     OUTLINE0(ISTART+1:ISTART+2) = " - "
                     OUTLINE0(ISTART+4:) = VARI(IFVARI+1:ILVARI)
                   ELSE                   
                     OUTLINE0(ISTART:ISTART+2) = " < "
                     OUTLINE0(ISTART+3:ISTART+ILVAL+2) = VAR
                     ISTART = ISTART+ILVAL
                     OUTLINE0(ISTART+1:ISTART+2) = " + "
                     OUTLINE0(ISTART+4:) = VARI(IFVARI+1:ILVARI)
                   ENDIF
                ENDIF      
            ELSE
              CALL ISSUE_ERR_MSG("INVALID VARIABLE")
              RETURN
            ENDIF
C       If cannot find value, variable or addition condition, issue error
C       message.                         
          ELSE
            CALL ISSUE_ERR_MSG("ERROR READING STAGE CRITERIA")
            RETURN
          ENDIF
          IF(J .LT. 2) THEN
            OUTLINE1 = OUTLINE0
          ELSE
            OUTLINE2 = OUTLINE0
          ENDIF
C        
          ENDDO
C        
        OUTLINE(1:3) = "IF "
        ISTART = 3
        ILOUT1 = LENSTR(OUTLINE1)
        OUTLINE(ISTART+1:ISTART+ILOUT1) = OUTLINE1
        ISTART = ISTART+ILOUT1
        OUTLINE(ISTART+1:ISTART+4) = " OR "
        ISTART = ISTART+4
        OUTLINE(ISTART+1:) = OUTLINE2
        WRITE(IOUTPUT,'(A)',ERR=910) OUTLINE
C      
        ELSE
          CALL ISSUE_ERR_MSG("ERR WITH REGUARD TO SINGLE OR DOUBLE
     1                   STAGE CONDITION")
          RETURN
        ENDIF
      ENDIF
      GOTO 999
C      
  900 CALL ISSUE_ERR_MSG("ERR READING INPUTFILE")
      GOTO 999
C
  910 CALL ISSUE_ERR_MSG("ERR WRITING 10 CARD")
C 
  999 RETURN
      END
      SUBROUTINE CARD11
C
C----------------------------------------------------------------------
C
C    Card type 11 - A "FUNCTION" card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132, DEFINIT*100, VARDEFI*100
      CHARACTER INLINE*132, COMBCODE*1, FUNCNAME*8, TLOC_IN_C*4
      CHARACTER VAR*13, VAR1*13, VAR2*13, STAGE*2
      CHARACTER TPARM1*4, TPARM2*4,  PARM1*14, PARM2*14, PARM3*5
      LOGICAL FOUND
C
      OUTLINE = ' '
C
      IF (INLINE(9:11) .EQ. 'END') THEN
         WRITE(IOUTPUT,100,ERR=9100) "CLEAR"
  100    FORMAT(A5)           
      ELSE
C
C         Parse line for other data
C        
        READ(INLINE(22:25),200,ERR=9000) TLOC_IN_C
  200   FORMAT(A4)
        CALL FIND_VARIABLE ( TLOC_IN_C, VAR, FOUND, DEFINIT)
        VARDEFI = DEFINIT
        IFVAR = NXT_CHAR(1, VAR)
        ILVAR = LAST_CHAR(IFVAR, VAR)
        VAR = VAR(IFVAR:ILVAR)
C          
        COMBCODE = INLINE(9:9)
        FUNCNAME = INLINE(10:14)
        IFFUN = NXT_CHAR(1, FUNCNAME)
        ILFUN = LAST_CHAR(IFFUN, FUNCNAME)
        FUNCNAME = FUNCNAME(IFFUN:ILFUN)        
C        
        IF (FOUND) THEN
          READ(INLINE(61:62),300,ERR=9000) STAGE
  300     FORMAT(A2)  
          IF ((FUNCNAME .NE. "SUM") .AND. (FUNCNAME .NE. "PROD") .AND.
     1      (FUNCNAME .NE. "DIFF") .AND. (FUNCNAME .NE. "EQUAL")) THEN
            READ(INLINE(32:45),320,ERR=9000) PARM1
  320       FORMAT(A14)
            IFPAR1 = NXT_CHAR(1, PARM1)
            ILPAR1 = LAST_CHAR(IFPAR1, PARM1)
            PARM1 = PARM1(IFPAR1:ILPAR1)
C            
            READ(INLINE(47:58),320,ERR=9000) PARM2
            READ(INLINE(26:30),350,ERR=9000) PARM3
  350       FORMAT(A5)
            IFPAR2 = NXT_CHAR(1, PARM2)
            ILPAR2 = LAST_CHAR(IFPAR2, PARM2)
            PARM2 = PARM2(IFPAR2:ILPAR2)
C
            IFPAR3 = NXT_CHAR(1, PARM3)
            ILPAR3 = LAST_CHAR(IFPAR3, PARM3)
            PARM3 = PARM3(IFPAR3:ILPAR3)
C     
            IF (STAGE .NE. '  ') THEN     
              IF (INLINE(26:30) .NE. '     ') THEN
                OUTLINE(1:5) = "FUNC "
                ISTART = 6
                OUTLINE(ISTART:ISTART+ILVAR-1) = VAR
                ISTART = ISTART+ILVAR
                OUTLINE(ISTART+1:ISTART+1) = COMBCODE
                OUTLINE(ISTART+3:ISTART+ILFUN+2) = FUNCNAME
                ISTART = ISTART+ILFUN+3
                OUTLINE(ISTART:ISTART) = "("
                OUTLINE(ISTART+1:ISTART+ILPAR1) = PARM1
                ISTART = ISTART+ILPAR1+1
                OUTLINE(ISTART:ISTART) = ","
                OUTLINE(ISTART+1:ISTART+ILPAR2) = PARM2
                ISTART = ISTART+ILPAR2+1
                OUTLINE(ISTART:ISTART) = ","
                OUTLINE(ISTART+1:ISTART+ILPAR3) = PARM3
                ISTART = ISTART+ILPAR3+1
                OUTLINE(ISTART:ISTART+7) = ") STAGE "
                OUTLINE(ISTART+8:ISTART+10) = STAGE
                ISTART = ISTART+11
                 IF(VARDEFI .EQ. " ") THEN
                   WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                 ELSE
                   IEND = LENSTR(OUTLINE)
                   IF(IEND .GT. 17) THEN
                     ISTART = IEND +3
                     OUTLINE(ISTART:ISTART) = "!"
                     OUTLINE(ISTART+2:) = VARDEFI
                   ELSE
                     OUTLINE(20:20) = "!"
                     OUTLINE(22:) = VARDEFI
                   ENDIF
                   WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                 ENDIF
              ELSE
                IF (INLINE(47:58) .EQ. '     ') THEN
                  OUTLINE(1:5) = "FUNC "
                  ISTART = 6
                  OUTLINE(ISTART:ISTART+ILVAR-1) = VAR
                  ISTART = ISTART+ILVAR
                  OUTLINE(ISTART+1:ISTART+1) = COMBCODE
                  OUTLINE(ISTART+3:ISTART+ILFUN+2) = FUNCNAME
                  ISTART = ISTART+ILFUN+3
                  OUTLINE(ISTART:ISTART) = "("
                  OUTLINE(ISTART+1:ISTART+ILPAR1) = PARM1
                  ISTART = ISTART+ILPAR1+1
                  OUTLINE(ISTART:ISTART+7) = ") STAGE "
                  OUTLINE(ISTART+8:ISTART+10) = STAGE
                  ISTART = ISTART +11
                  IF(VARDEFI .EQ. " ") THEN
                    WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                  ELSE
                    IEND = LENSTR(OUTLINE)
                    IF(IEND .GT. 17) THEN
                      ISTART = IEND +3
                      OUTLINE(ISTART:ISTART) = "!"
                      OUTLINE(ISTART+2:) = VARDEFI
                    ELSE
                      OUTLINE(20:20) = "!"
                      OUTLINE(22:) = VARDEFI
                    ENDIF
                    WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                  ENDIF
                ELSE
                  OUTLINE(1:5) = "FUNC "
                  ISTART = 6
                  OUTLINE(ISTART:ISTART+ILVAR-1) = VAR
                  ISTART = ISTART+ILVAR
                  OUTLINE(ISTART+1:ISTART+1) = COMBCODE
                  OUTLINE(ISTART+3:ISTART+ILFUN+2) = FUNCNAME
                  ISTART = ISTART+ILFUN+3
                  OUTLINE(ISTART:ISTART) = "("
                  OUTLINE(ISTART+1:ISTART+ILPAR1) = PARM1
                  ISTART = ISTART+ILPAR1+1
                  OUTLINE(ISTART:ISTART) = ","
                  OUTLINE(ISTART+1:ISTART+ILPAR2) = PARM2
                  ISTART = ISTART+ILPAR2+1
                  OUTLINE(ISTART:ISTART+7) = ") STAGE "
                  OUTLINE(ISTART+8:ISTART+10) = STAGE
                  ISTART = ISTART +11
                  IF(VARDEFI .EQ. " ") THEN
                    WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                  ELSE
                    IEND = LENSTR(OUTLINE)
                    IF(IEND .GT. 17) THEN
                      ISTART = IEND +3
                      OUTLINE(ISTART:ISTART) = "!"
                      OUTLINE(ISTART+2:) = VARDEFI
                    ELSE
                      OUTLINE(20:20) = "!"
                      OUTLINE(22:) = VARDEFI
                    ENDIF
                    WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              IF (INLINE(26:30) .NE. '     ') THEN
                OUTLINE(1:5) = "FUNC "
                ISTART = 6
                OUTLINE(ISTART:ISTART+ILVAR-1) = VAR
                ISTART = ISTART+ILVAR
                OUTLINE(ISTART+1:ISTART+1) = COMBCODE
                OUTLINE(ISTART+3:ISTART+ILFUN+2) = FUNCNAME
                ISTART = ISTART+ILFUN+3
                OUTLINE(ISTART:ISTART) = "("
                OUTLINE(ISTART+1:ISTART+ILPAR1) = PARM1
                ISTART = ISTART+ILPAR1+1
                OUTLINE(ISTART:ISTART) = ","
                OUTLINE(ISTART+1:ISTART+ILPAR2) = PARM2
                ISTART = ISTART+ILPAR2+1
                OUTLINE(ISTART:ISTART) = ","
                OUTLINE(ISTART+1:ISTART+ILPAR3) = PARM3
                OUTLINE(ISTART+ILPAR3+1:ISTART+ILPAR3+1) = ")"
                ISTART = ISTART+ILPAR3+2
                 IF(VARDEFI .EQ. " ") THEN
                   WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                 ELSE
                   IEND = LENSTR(OUTLINE)
                   IF(IEND .GT. 17) THEN
                     ISTART = IEND +3
                     OUTLINE(ISTART:ISTART) = "!"
                     OUTLINE(ISTART+2:) = VARDEFI
                   ELSE
                     OUTLINE(20:20) = "!"
                     OUTLINE(22:) = VARDEFI
                   ENDIF
                   WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                 ENDIF
              ELSE
                IF (INLINE(47:58) .EQ. '     ') THEN
                  OUTLINE(1:5) = "FUNC "
                  ISTART = 6
                  OUTLINE(ISTART:ISTART+ILVAR-1) = VAR
                  ISTART = ISTART+ILVAR
                  OUTLINE(ISTART+1:ISTART+1) = COMBCODE
                  OUTLINE(ISTART+3:ISTART+ILFUN+2) = FUNCNAME
                  ISTART = ISTART+ILFUN+3
                  OUTLINE(ISTART:ISTART) = "("
                  OUTLINE(ISTART+1:ISTART+ILPAR1) = PARM1
                  ISTART = ISTART+ILPAR1+1
                  OUTLINE(ISTART:ISTART) = ")"
                  IF(VARDEFI .EQ. " ") THEN
                    WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                  ELSE
                    IEND = LENSTR(OUTLINE)
                    IF(IEND .GT. 17) THEN
                      ISTART = IEND +3
                      OUTLINE(ISTART:ISTART) = "!"
                      OUTLINE(ISTART+2:) = VARDEFI
                    ELSE
                      OUTLINE(20:20) = "!"
                      OUTLINE(22:) = VARDEFI
                    ENDIF
                    WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                  ENDIF
                ELSE
                  OUTLINE(1:5) = "FUNC "
                  ISTART = 6
                  OUTLINE(ISTART:ISTART+ILVAR-1) = VAR
                  ISTART = ISTART+ILVAR
                  OUTLINE(ISTART+1:ISTART+1) = COMBCODE
                  OUTLINE(ISTART+3:ISTART+ILFUN+2) = FUNCNAME
                  ISTART = ISTART+ILFUN+3
                  OUTLINE(ISTART:ISTART) = "("
                  OUTLINE(ISTART+1:ISTART+ILPAR1) = PARM1
                  ISTART = ISTART+ILPAR1+1
                  OUTLINE(ISTART:ISTART) = ","
                  OUTLINE(ISTART+1:ISTART+ILPAR2) = PARM2
                  ISTART = ISTART+ILPAR2+1
                  OUTLINE(ISTART:ISTART) = ")"
                  IF(VARDEFI .EQ. " ") THEN
                    WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                  ELSE
                    IEND = LENSTR(OUTLINE)
                    IF(IEND .GT. 17) THEN
                      ISTART = IEND +3
                      OUTLINE(ISTART:ISTART) = "!"
                      OUTLINE(ISTART+2:) = VARDEFI
                    ELSE
                      OUTLINE(20:20) = "!"
                      OUTLINE(22:) = VARDEFI
                    ENDIF
                    WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                  ENDIF
                ENDIF
              ENDIF
            ENDIF  
          ELSE   
            READ(INLINE(32:35),1000,ERR=9000) TPARM1
 1000       FORMAT(A4)
            READ(INLINE(47:50),1000,ERR=9000) TPARM2
            TPARM11 = INDEX(TPARM1, '.')
            TPARM22 = INDEX(TPARM2, '.')
            IF (TPARM11 .GT. 0) TPARM1 = TPARM1(1:TPARM11-1)
            IF (TPARM22 .GT. 0) TPARM2 = TPARM2(1:TPARM22-1)
            CALL FIND_VARIABLE ( TPARM1, VAR1, FOUND, DEFINIT)
            IFVAR1 = NXT_CHAR(1, VAR1)
            ILVAR1 = LAST_CHAR(IFVAR1, VAR1)
            VAR1 = VAR1(IFVAR1:ILVAR1)
C                        
            IF (FOUND) THEN
              IF (TPARM2 .NE. " ") THEN
                 CALL FIND_VARIABLE ( TPARM2, VAR2, FOUND, DEFINIT)
                 IFVAR2 = NXT_CHAR(1, VAR2)
                 ILVAR2 = LAST_CHAR(IFVAR2, VAR2)
                 VAR2 = VAR2(IFVAR2:ILVAR2)                 
              ENDIF
C              
              IF (FOUND .OR. TPARM2 .EQ. " ") THEN
                IF (STAGE .NE. " ") THEN
                  IF (INLINE(47:50) .EQ. '     ') THEN
                    OUTLINE(1:5) = "FUNC "
                    ISTART = 6
                    OUTLINE(ISTART:ISTART+ILVAR-1) = VAR
                    ISTART = ISTART+ILVAR
                    OUTLINE(ISTART+1:ISTART+1) = COMBCODE
                    OUTLINE(ISTART+3:ISTART+ILFUN+2) = FUNCNAME
                    ISTART = ISTART+ILFUN+3
                    OUTLINE(ISTART:ISTART) = "("
                    OUTLINE(ISTART+1:ISTART+ILVAR1) = VAR1
                    ISTART = ISTART+ILVAR1+1
                    OUTLINE(ISTART:ISTART+7) = ") STAGE "
                    OUTLINE(ISTART+8:ISTART+10) = STAGE
                    ISTART = ISTART +11
                    IF(VARDEFI .EQ. " ") THEN
                      WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                    ELSE
                      IEND = LENSTR(OUTLINE)
                      IF(IEND .GT. 17) THEN
                        ISTART = IEND +3
                        OUTLINE(ISTART:ISTART) = "!"
                        OUTLINE(ISTART+2:) = VARDEFI
                      ELSE
                        OUTLINE(20:20) = "!"
                        OUTLINE(22:) = VARDEFI
                      ENDIF
                      WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                    ENDIF
                  ELSE
                    OUTLINE(1:5) = "FUNC "
                    ISTART = 6
                    OUTLINE(ISTART:ISTART+ILVAR-1) = VAR
                    ISTART = ISTART+ILVAR
                    OUTLINE(ISTART+1:ISTART+1) = COMBCODE
                    OUTLINE(ISTART+3:ISTART+ILFUN+2) = FUNCNAME
                    ISTART = ISTART+ILFUN+3
                    OUTLINE(ISTART:ISTART) = "("
                    OUTLINE(ISTART+1:ISTART+ILVAR1) = VAR1
                    ISTART = ISTART+ILVAR1+1
                    OUTLINE(ISTART:ISTART) = ","
                    OUTLINE(ISTART+1:ISTART+ILVAR2) = VAR2
                    ISTART = ISTART+ILVAR2+1
                    OUTLINE(ISTART:ISTART+7) = ") STAGE "
                    OUTLINE(ISTART+8:ISTART+10) = STAGE
                    ISTART = ISTART +11
                    IF(VARDEFI .EQ. " ") THEN
                      WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                    ELSE
                      IEND = LENSTR(OUTLINE)
                      IF(IEND .GT. 17) THEN
                        ISTART = IEND +3
                        OUTLINE(ISTART:ISTART) = "!"
                        OUTLINE(ISTART+2:) = VARDEFI
                      ELSE
                        OUTLINE(20:20) = "!"
                        OUTLINE(22:) = VARDEFI
                      ENDIF
                      WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                    ENDIF
                  ENDIF            
                ELSE
                  IF (INLINE(47:50) .EQ. '     ') THEN
                    OUTLINE(1:5) = "FUNC "
                    ISTART = 6
                    OUTLINE(ISTART:ISTART+ILVAR-1) = VAR
                    ISTART = ISTART+ILVAR
                    OUTLINE(ISTART+1:ISTART+1) = COMBCODE
                    OUTLINE(ISTART+3:ISTART+ILFUN+2) = FUNCNAME
                    ISTART = ISTART+ILFUN+3
                    OUTLINE(ISTART:ISTART) = "("
                    OUTLINE(ISTART+1:ISTART+ILVAR1) = VAR1
                    ISTART = ISTART+ILVAR1+1
                    OUTLINE(ISTART:ISTART) = ")"
                    IF(VARDEFI .EQ. " ") THEN
                      WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                    ELSE
                      IEND = LENSTR(OUTLINE)
                      IF(IEND .GT. 17) THEN
                        ISTART = IEND +3
                        OUTLINE(ISTART:ISTART) = "!"
                        OUTLINE(ISTART+2:) = VARDEFI
                      ELSE
                        OUTLINE(20:20) = "!"
                        OUTLINE(22:) = VARDEFI
                      ENDIF
                      WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                    ENDIF
                  ELSE
                    OUTLINE(1:5) = "FUNC "
                    ISTART = 6
                    OUTLINE(ISTART:ISTART+ILVAR-1) = VAR
                    ISTART = ISTART+ILVAR
                    OUTLINE(ISTART+1:ISTART+1) = COMBCODE
                    OUTLINE(ISTART+3:ISTART+ILFUN+2) = FUNCNAME
                    ISTART = ISTART+ILFUN+3
                    OUTLINE(ISTART:ISTART) = "("
                    OUTLINE(ISTART+1:ISTART+ILVAR1) = VAR1
                    ISTART = ISTART+ILVAR1+1
                    OUTLINE(ISTART:ISTART) = ","
                    OUTLINE(ISTART+1:ISTART+ILVAR2) = VAR2
                    ISTART = ISTART+ILVAR2+1
                    OUTLINE(ISTART:ISTART) = ")"
                    IF(VARDEFI .EQ. " ") THEN
                      WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                    ELSE
                      IEND = LENSTR(OUTLINE)
                      IF(IEND .GT. 17) THEN
                        ISTART = IEND +3
                        OUTLINE(ISTART:ISTART) = "!"
                        OUTLINE(ISTART+2:) = VARDEFI
                      ELSE
                        OUTLINE(20:20) = "!"
                        OUTLINE(22:) = VARDEFI
                      ENDIF
                      WRITE(IOUTPUT,'(A)',ERR=9100) OUTLINE
                    ENDIF
                  ENDIF
                ENDIF
              ELSE                     
                  CALL ISSUE_ERR_MSG("INVALID VARIABLE")
                ENDIF
              ELSE
                  CALL ISSUE_ERR_MSG("INVALID VARIABLE")
              ENDIF
            ENDIF
        ELSE
          CALL ISSUE_ERR_MSG("INVALID VARIABLE")
        ENDIF 
      ENDIF
      GOTO 9999
C
 9000 CALL ISSUE_ERR_MSG("ERROR READING FUNCTION RECORD")
      GOTO 9999
C
 9100 CALL ISSUE_ERR_MSG("ERROR WRITING FUNCTION RECORD")
C
 9999 RETURN
      END
      SUBROUTINE CARD12
C
C----------------------------------------------------------------------
C
C    Card type 12 - a "LOAD" card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
C
      WRITE(IOUTPUT,100) "LOAD"
  100 FORMAT(A4)
C
      RETURN
      END
      SUBROUTINE CARD13
C
C----------------------------------------------------------------------
C
C    Card type 13 - A "STOP" card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
C
      WRITE(IOUTPUT,100) "STOP"
  100 FORMAT(A4)
C
      RETURN
      END
      SUBROUTINE CARD16
C
C----------------------------------------------------------------------
C
C    Card type 16 - A "STAGE END" card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132
      CHARACTER INLINE*132 
C
  999 RETURN
      END
      SUBROUTINE CARD19
C
C----------------------------------------------------------------------
C
C    Card type 19 - A "SWEEP" card
C
C----------------------------------------------------------------------
C
C    Write "SWEEP" block header
C    NOTE: Assume that card 19 is the FIRST card of the sweep
C    definition block!
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER INLINE*132, OUTLINE*132, TLOC_IN_C*4, VAR1*13
      CHARACTER ANGLE*14, ANGLE2*15, DANGLE*4, DEFINIT*100, VAR1DEFI*100
      LOGICAL FOUND
C
C   Write card type
C
         WRITE(IOUTPUT,'(A)') "SWEEP"
C
C--- Initialize output line
C
         OUTLINE=" "
C
C--- Prepare output line one phrase at a time
C
         OUTLINE(1:11) = "ANGLE/OUTER"
         ISTART = 13
         READ(INLINE(32:45),'(A)') ANGLE
         IFANG = NXT_CHAR(1, ANGLE)
         ILANG = LAST_CHAR(IFANG, ANGLE)
         ANGLE = ANGLE(IFANG:ILANG)
C         
         READ(INLINE(46:60),'(A)') ANGLE2
         IFANG2 = NXT_CHAR(1, ANGLE2)
         ILANG2 = LAST_CHAR(IFANG2, ANGLE2)
         ANGLE2 = ANGLE2(IFANG2:ILANG2)
C         
         READ(INLINE(27:30),'(A)') DANGLE
         IFDANG = NXT_CHAR(1, DANGLE)
         ILDANG = LAST_CHAR(IFDANG, DANGLE)
         DANGLE = DANGLE(IFDANG:ILDANG)
C         
         OUTLINE(ISTART:ISTART+ILANG) = ANGLE
         ISTART = ISTART+ILANG+1
         OUTLINE(ISTART:ISTART)="<"
C         
         TLOC_IN_C=INLINE(22:25)
         CALL FIND_VARIABLE ( TLOC_IN_C, VAR1, FOUND, DEFINIT)
         VAR1DEFI = DEFINIT
         IFVAR1 = NXT_CHAR(1, VAR1)
         ILVAR1 = LAST_CHAR(IFVAR1, VAR1)
         VAR1 = VAR1(IFVAR1:ILVAR1)
C         
         IF (FOUND) THEN
           OUTLINE(ISTART+2:ISTART+ILVAR1+1) = VAR1
           ISTART = ISTART+ILVAR1+3
           OUTLINE(ISTART:ISTART)="<"
           OUTLINE(ISTART+2:ISTART+ILANG2+1) = ANGLE2
           ISTART = ISTART+ILANG2+2
           IF (INLINE(61:62) .EQ. " 0") THEN
             OUTLINE(ISTART:ISTART+2)="DEG"
             ISTART = ISTART+4
           ENDIF
           OUTLINE(ISTART:ISTART+6)="DELT = "
           ISTART = ISTART+7
           OUTLINE(ISTART:ISTART+ILDANG-1) = DANGLE
           ISTART = ISTART+ILDANG+1
C
C--- Write output line
C
                 IF(VAR1DEFI .EQ. " ") THEN
                   WRITE(IOUTPUT,'(A)') OUTLINE
                 ELSE
                   IEND = LENSTR(OUTLINE)
                   IF(IEND .GT. 17) THEN
                     ISTART = IEND +3
                     OUTLINE(ISTART:ISTART) = "!"
                     OUTLINE(ISTART+2:) = VAR1DEFI
                   ELSE
                     OUTLINE(20:20) = "!"
                     OUTLINE(22:) = VAR1DEFI
                   ENDIF
                   WRITE(IOUTPUT,'(A)') OUTLINE
                 ENDIF
         ELSE
           CALL ISSUE_ERR_MSG("INVALID VARIABLE")
         ENDIF
C
      CONTINUE
C
      RETURN
      END
      SUBROUTINE CARD20
C
C----------------------------------------------------------------------
C
C    Card type 20 - a "SWEEP" card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132
      CHARACTER INLINE*132, VAR1*13, TLOC_IN_C*4
      CHARACTER RANGE*14, RANGE2*15, DRANGE*4, DEFINIT*100, VAR1DEFI*100
      LOGICAL FOUND
C
C  Initialize output line
C
      OUTLINE = " "
C
C  Build output line one phrase at a time
C
      OUTLINE(1:11)  = "RANGE/INNER"
         ISTART = 13
         READ(INLINE(32:45),'(A)') RANGE
         IFRANG = NXT_CHAR(1, RANGE)
         ILRANG = LAST_CHAR(IFRANG, RANGE)
         RANGE = RANGE(IFRANG:ILRANG)
C         
         READ(INLINE(46:60),'(A)') RANGE2
         IFRANG2 = NXT_CHAR(1, RANGE2)
         ILRANG2 = LAST_CHAR(IFRANG2, RANGE2)
         RANGE2 = RANGE2(IFRANG2:ILRANG2)
C         
         READ(INLINE(27:30),'(A)') DRANGE
         IFDRANG = NXT_CHAR(1, DRANGE)
         ILDRANG = LAST_CHAR(IFDRANG, DRANGE)
         DRANGE = DRANGE(IFDRANG:ILDRANG)
C         
         OUTLINE(ISTART:ISTART+ILRANG) = RANGE
         ISTART = ISTART+ILRANG+1
         OUTLINE(ISTART:ISTART)="<"
C         
         TLOC_IN_C=INLINE(22:25)
         CALL FIND_VARIABLE ( TLOC_IN_C, VAR1, FOUND, DEFINIT)
         VAR1DEFI = DEFINIT
         IFVAR1 = NXT_CHAR(1, VAR1)
         ILVAR1 = LAST_CHAR(IFVAR1, VAR1)
         VAR1 = VAR1(IFVAR1:ILVAR1)
      IF (FOUND) THEN
           OUTLINE(ISTART+2:ISTART+ILVAR1+1) = VAR1
           ISTART = ISTART+ILVAR1+3
           OUTLINE(ISTART:ISTART)="<"
           OUTLINE(ISTART+2:ISTART+ILRANG2+1) = RANGE2
           ISTART = ISTART+ILRANG2+2
         IF (INLINE(27:30) .NE. "    ") THEN 
           OUTLINE(ISTART:ISTART+6)="DELT = "
           ISTART = ISTART+7
           OUTLINE(ISTART:ISTART+ILDRANG-1) = DRANGE
           ISTART = ISTART+ILDRANG+1
         ENDIF
C
C--- Write output line
C
                 IF(VAR1DEFI .EQ. " ") THEN
                   WRITE(IOUTPUT,'(A)') OUTLINE
                 ELSE
                   IEND = LENSTR(OUTLINE)
                   IF(IEND .GT. 17) THEN
                     ISTART = IEND +3
                     OUTLINE(ISTART:ISTART) = "!"
                     OUTLINE(ISTART+2:) = VAR1DEFI
                   ELSE
                     OUTLINE(20:20) = "!"
                     OUTLINE(22:) = VAR1DEFI
                   ENDIF
                   WRITE(IOUTPUT,'(A)') OUTLINE
                 ENDIF
      ELSE
         CALL ISSUE_ERR_MSG("INVALID VARIABLE")
      ENDIF
C
      RETURN
      END
      SUBROUTINE CARD21
C
C----------------------------------------------------------------------
C
C    Card type 21 - a "SWEEP" card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132
      CHARACTER INLINE*132, TLOC_IN_C*4, VAR1*13, NUM*15
      CHARACTER MNCVAL*15, MXCVAL*15, DEFINIT*100, VAR1DEFI*100
	CHARACTER C1805*4
      LOGICAL FOUND, END_OF_FILE
C
      OUTLINE=" "
C
C--- First, get mode from line
C
      OUTLINE(1:4)="MODE"
C
      IF (INLINE(30:30) .EQ. "0") THEN
        OUTLINE(6:10)="CONST"          
      ENDIF
C
      IF (INLINE(30:30) .EQ. "1") THEN
        OUTLINE(6:9)="INCR"
      ENDIF
C
      IF (INLINE(30:30) .EQ. "2") THEN
        OUTLINE(6:9)="DECR"
      ENDIF
C
      IF (INLINE(30:30) .EQ. "3") THEN
        OUTLINE(6:14)="INCR/DECR"
      ENDIF
C
      IF (INLINE(30:30) .EQ. "4") THEN
        OUTLINE(6:8)="OUT"
      ENDIF
C
      IF (INLINE(30:30) .EQ. "5") THEN
        OUTLINE(6:8)="ALL"
      ENDIF
C
      WRITE(IOUTPUT,'(A)') OUTLINE
C
C--- Next, set number of binaries/trajectories
C
      OUTLINE=" "
      OUTLINE(1:3)="NUM"
CJH      
CJH     INPUT21                                              June  1998
CJH         - Modified how "NUM" variable in Sweep Block is
CJH           handled.  Cases 0-3 "NUM" will not be printed
CJH           to "Input.asc" because "CADIN" calculates it.
CJH           For cases 4&5, "NUM" will be printed because
CJH           "NUM" used in "CADIN" will be provided by the user.
CJH
CJH     INPUT23                                              August  1998
CJH         - How "NUM" variable in Sweep Block is
CJH           handled modified again.  "NUM" will be present in 
CJH           "CADIN" for all cases.  The value of "NUM" will be
CJH           in a different location for cases 0-3 than for cases 4&5.
CJH
      IF ((INLINE(30:30) .EQ. "4") .OR. (INLINE(30:30) .EQ. "5")) THEN
		OUTLINE(5:6) = INLINE(61:62)
      ELSE
	    READ(INLINE(46:60), '(A)') NUM
	    IFNUM = NXT_CHAR(1, NUM)
	    ILNUM = LAST_CHAR(IFNUM, NUM)
	    NUM = NUM(IFNUM:ILNUM)
          OUTLINE(5:) = NUM
      ENDIF
C
      WRITE(IOUTPUT,'(A)') OUTLINE
C
C--- Now, set LIMIT for "OUT" mode
C
      OUTLINE=" "
      READ(INLINE(31:45), '(A)') MNCVAL
      IFMNCVAL = NXT_CHAR(1, MNCVAL)
      ILMNCVAL = LAST_CHAR(IFMNCVAL, MNCVAL)
      MNCVAL = MNCVAL(IFMNCVAL:ILMNCVAL)
C      
C     IF "MNCVAL" = NOTHING, THEN NO "LIMIT" GOES IN THE INPUT.ASC FILE
C
      IF (MNCVAL .NE. "               ") THEN
CJH          IF ((INLINE(30:30) .EQ. "4") .OR. (INLINE(30:30) .EQ. "5"))THEN
              OUTLINE(1:5)="LIMIT"
              ISTART = 7
              OUTLINE(ISTART:ISTART+ILMNCVAL-1) = MNCVAL             ! Minimum critical value
              ISTART = ISTART+ILMNCVAL+1
              OUTLINE(ISTART:ISTART)="<"
              ISTART = ISTART+2
C         
              TLOC_IN_C=INLINE(22:25)
              CALL FIND_VARIABLE( TLOC_IN_C, VAR1, FOUND, DEFINIT)
              VAR1DEFI = DEFINIT
              IFVAR1 = NXT_CHAR(1, VAR1)
              ILVAR1 = LAST_CHAR(IFVAR1, VAR1)
              VAR1 = VAR1(IFVAR1:ILVAR1)
C         
              IF (FOUND) THEN
                  OUTLINE(ISTART:ISTART+ILVAR1-1) = VAR1
                  ISTART = ISTART+ILVAR1+1
C
C--- Max critical value is read from NEXT card, which is type 03.
C--- If the C-location of the type 03 card is NOT 1805 then no
C--- maximum critical value was entered.
C
                  CALL GET_WK_STRING(INLINE, END_OF_FILE)
	            READ(INLINE(22:25), '(A)') C1805
	            IF( C1805 .EQ. '1805' ) THEN
C         THIS LINE CONTAINS MAX CRITICAL VALUE
                      OUTLINE(ISTART:ISTART)="<"
                      ISTART = ISTART+2
                      READ(INLINE(31:45), '(A)') MXCVAL
                      IFMXCVAL = NXT_CHAR(1, MXCVAL)
                      ILMXCVAL = LAST_CHAR(IFMXCVAL, MXCVAL)
                      MXCVAL = MXCVAL(IFMXCVAL:ILMXCVAL)
                      OUTLINE(ISTART:ISTART+ILMXCVAL-1) = MXCVAL
                      ISTART = ISTART+ILMXCVAL+1
	            ELSE
                      ISTART = 60
                  ENDIF
C
C--- Write outline
C       
                  IF(VAR1DEFI .EQ. " ") THEN
                      WRITE(IOUTPUT,'(A)') OUTLINE
                  ELSE
                      IEND = LENSTR(OUTLINE)
                      IF(IEND .GT. 17) THEN
                          ISTART = IEND +3
                          OUTLINE(ISTART:ISTART) = "!"
                          OUTLINE(ISTART+2:) = VAR1DEFI
                      ELSE
                          OUTLINE(20:20) = "!"
                          OUTLINE(22:) = VAR1DEFI
                      ENDIF
                      WRITE(IOUTPUT,'(A)') OUTLINE
                  ENDIF
              ELSE
                  CALL ISSUE_ERR_MSG("INVALID VARIABLE")
              ENDIF
CJH          ENDIF
	ENDIF
C
C--- Write end of sweep block line
C
      WRITE(IOUTPUT,'(A)') "END SWEEP"
C
      RETURN
      END
      SUBROUTINE CARD90
C
C----------------------------------------------------------------------
C
C    Card type 90 - a "SAVE" card
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
C
      WRITE(IOUTPUT,100) "SAVE"
  100 FORMAT(A4)
C
      RETURN
      END
      SUBROUTINE CLEAR_SCREEN
C
C-----------------------------------------------------------------------
C
C     This module clears the data from the terminal screen.
C
C-----------------------------------------------------------------------
C
CJH      PRINT*, CHAR(27) // '[2J'
C 
      RETURN
      END  
      SUBROUTINE CLOSE_FILES
C
C----------------------------------------------------------------------
C
C     This module closes the files accessed by this program and it
C     renames the input file INPUT.ASC to INPUT.BAK and renames the
C     the file containing the definitions to INPUT.ASC (where INPUT is
C     a user specified name).
C
C----------------------------------------------------------------------
C
      COMMON /ERROR_INFO/ JERR
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /FILE_NAMES/ INPUT_NAME, INPUT_BACKUP,
     1                    OUTPUT_NAME, OUTPUT_BACKUP, ERROR_FILE
C
      CHARACTER*60        INPUT_NAME, INPUT_BACKUP,
     1                    OUTPUT_NAME, OUTPUT_BACKUP, ERROR_FILE
      CHARACTER STRDUM*80
      INTEGER*4 FINDFILEQQ, DELFILESQQ
C          
C---  Close the data files.
C
      CLOSE( INPUT )
      CLOSE( IOUTPUT )
C
C---  See if a file with the backup name currently exists.  If it does,
C---  delete it.
C
      IFOUND = FINDFILEQQ( INPUT_BACKUP(1:LENSTR(INPUT_BACKUP)), 
     1         'PATH', STRDUM ) 
      IF(IFOUND .GT. 0)
     1  IDEL = DELFILESQQ( INPUT_BACKUP(1:LENSTR(INPUT_BACKUP)) )
C
CC      CALL FILE_COPY( OUTPUT_NAME, OUTPUT_BACKUP )
      CALL FILE_COPY( INPUT_NAME, INPUT_BACKUP )
C
C---  See if any errors occurred during execution.  If they did, inform
C---  the user.  If they didn't close the file with a delete status.
C
      IF( JERR .GT. 0 ) THEN
        CLOSE( JERROR )
        CALL CLEAR_SCREEN 
        IF( JERR .EQ. 1 ) THEN
          WRITE(*,'(/////15X, A, I4, A / 15X, A // 15X, A)' )
     1        '* * * ', JERR, ' ERROR WAS FOUND IN INPUT FILE * * *', 
     2        '   REFER TO ERROR.ASC FILE IN OUTPUT FILE DIRECTORY',
     3        '        PRESS <RETURN> TO CONTINUE'
        ELSE
          WRITE(*,'(/////15X, A, I4, A / 15X, A // 15X, A)' )
     1        '* * * ', JERR, ' ERRORS WERE FOUND IN INPUT FILE * * *', 
     2        '   REFER TO ERROR.ASC FILE IN OUTPUT FILE DIRECTORY',
     3        '        PRESS <RETURN> TO CONTINUE'
        ENDIF
        READ(*,'(A)') ANS  
      ELSE
        CLOSE( JERROR, STATUS='DELETE' )
      ENDIF  
C
C---  Delete the expanded head.asc file.
C
      IDEL = DELFILESQQ( 'NUHEAD.ASC' )
C                   
      RETURN
      END
      SUBROUTINE CODE_LINES
C
C----------------------------------------------------------------------
C
C     This module codes the module lines in the CADAC fixed form input 
C     file, CADIN.ASC, into the format used in the freeform input file
C     INPUT.ASC
C
C----------------------------------------------------------------------
C
C   The first time into this module the value of the input stiring
C   is comming from Routine PUT_TITLE, it has just read the first record
C   past the title and is now in variable INLINE
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/      INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132
C
      CHARACTER INLINE*132, CODE*2
      LOGICAL END_OF_FILE
C
C  Initialize data
C
      END_OF_FILE = .FALSE.
      CODE = '00'
C      
  100 CONTINUE
C
C   Skip this if INLINE already loaded
C
      IF(CODE.GT.'00') THEN
        CALL GET_WK_STRING ( INLINE, END_OF_FILE ) ! read the next line
        IF( END_OF_FILE ) GOTO 9900
      ENDIF
C
C  Process INLINE to get the Card type
C
      CODE = INLINE(1:2)                      !  Card type
C
C--- The line is a type 01 card, Subroutine selection
C    read past type 01 cards, but don't do anything else
C    with them 
C       
      IF (CODE .EQ. '01') THEN
          CALL CARD01
          GOTO 9900
      ENDIF
C
C---   The line is a type 02 card, MODULE name
C
      IF (CODE .EQ. '02') THEN
          CALL CARD02 ( CODE )
          GOTO 9900
      ENDIF
C
C---   The line read in was a type 03 card, a variable assignment
C
      IF (CODE .EQ. '03') THEN
          CALL CARD03
          GOTO 9900
      ENDIF                
C
C---    The line read in was a comment, put it in the freeform file 
C---    preceded by !
C
      IF (CODE .EQ. '04') THEN
          CALL CARD04
          GOTO 9900
      ENDIF
C
C---    The line read in was a type 05 card, which is a multi-run 
C       selection card
C                       
      IF (CODE .EQ. '05') THEN
          CALL CARD05
          GOTO 9900          
      ENDIF
C
C---    The line read in was a type 06 card, which is a RUN card.
C           
      IF (CODE .EQ. '06') THEN
          CALL CARD06
          GOTO 9900                   
      ENDIF
C
C---    The line read in was a type 07 card, which is a vector
C       initialization
C      
      IF (CODE .EQ. '07') THEN
        CALL CARD07
        GOTO 9900
      ENDIF
C
C---    The line read in was a type 08 card, which is atmospheric input
C      
      IF (CODE .EQ. '08') THEN
           CALL CARD08
           GOTO 9900
      ENDIF
C
C---    The line read in was a type 09 card, which is a header
C       card 
C      
      IF (CODE .EQ. '09') THEN                      
         CALL CARD09
         GOTO 9900
      ENDIF
C
C---    The line read in was a type 10 card, staging criteria
C      
      IF (CODE .EQ. '10') THEN
          CALL CARD10
          GOTO 9900
      ENDIF
C
C       The line read in was a type 11 card, function card
C      
      IF (CODE .EQ. '11') THEN
        CALL CARD11
        GOTO 9900
      ENDIF
C
C---    The line read in was a type 12 card, load file 90
C      
      IF (CODE .EQ. '12') THEN
          CALL CARD12
          GOTO 9900
      ENDIF
C
C---    The line read in was a type 13 card, stop card
C      
      IF (CODE .EQ. '13') THEN      
         CALL CARD13
         GOTO 9900
      ENDIF
C
C---    The line read in was a type 16 card, end of staging criteria
C
      IF (CODE .EQ. '16') THEN
         CALL CARD16
         GOTO 9900
      ENDIF
C      
C---    The line read in was a SWEEP outer variable definition
C
      IF (CODE .EQ. '19') THEN
         CALL CARD19
         GOTO 9900
      ENDIF
C      
C--- The line read in was a SWEEP inner variable definition
C
      IF (CODE .EQ. '20') THEN
         CALL CARD20
         GOTO 9900
      ENDIF
C      
C--- The line read in was a type 21 card, which sets the remainder
C    of the sweep data
C
      IF (CODE .EQ. '21') THEN
        CALL CARD21
        GOTO 9900
      ENDIF
C      
C---    The line read in was a save card
C
      IF (CODE .EQ. '90') THEN
         CALL CARD90
         GOTO 9900
      ENDIF
C
 9900 IF( END_OF_FILE ) RETURN
C      
      GOTO 100
      END
      SUBROUTINE COPY_HEAD
C
C----------------------------------------------------------------------
C
C     This module copies the variables and their C locations from the
C     HEAD.ASC file to a working file.  Expanding the arrays, and
C     completing arrays notation where necessary.
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
C      
      COMMON /PATH_DIR/ FDRIVE,   FDIR
      CHARACTER         FDRIVE*2, FDIR*60, FEXT*4, FNAME*8
      CHARACTER         PATHDIREC*80
C
      CHARACTER VAR_READ*13, FILENAME*80, TEMP*15
      CHARACTER ASTRING*80, O_ASTRING*80, COMMENT*80
      LOGICAL GOODFILE, EOFILE  
      INTEGER SPLITPATHQQ
C
C---  Give a status report for the user.
C
      CALL CLEAR_SCREEN
      WRITE(*,'(/////// 20X, A)' ) 
     1     '* * * Expanding HEAD.ASC file * * *'
C
C--- the  Copied HEAD file.
C
      OPEN( NUHEAD, FILE = 'NUHEAD.ASC', FORM = 'FORMATTED',
     1      STATUS = 'UNKNOWN')
C
C---  Open the original HEAD.ASC file.
C
      GOODFILE = .FALSE.
      FNAME = 'HEAD'
      FEXT = '.ASC'
      PATHDIREC = FDRIVE // FDIR(1:LENSTR(FDIR))
      FILENAME = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
     1             FNAME(1:LENSTR(FNAME)) // FEXT
   10 OPEN(IHEAD, FILE=FILENAME(1:LENSTR(FILENAME)), FORM='FORMATTED', 
     1           STATUS='OLD', ERR=20)
      GOODFILE = .TRUE.
C
   20 CONTINUE
C
CC      IF( .NOT. GOODFILE ) CALL END_MSG_STOP
CC     1    ( 'ERROR Opening HEAD.ASC'  )
      FILENAME = ' '
      IF( .NOT. GOODFILE ) THEN
          WRITE(*,'(//5X,A/5X,A//8X,A//5X,A/5X,A\)')
     1    'HEAD.ASC or user given header file name is not in present',
     2    'Directory: ', PATHDIREC,
     3    'Please enter name of HEADER file with',
     4    'path name if needed: '
          READ(*,'(A)') FILENAME
          ILEN = SPLITPATHQQ( FILENAME, FDRIVE, FDIR, FNAME, FEXT )
          FILENAME = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
     1             FNAME(1:LENSTR(FNAME)) // FEXT
          GOODFILE = .FALSE.
        GOTO 10
      ENDIF
C
C---  Read past the scroll variables.
C
      READ(IHEAD,'(A)') O_ASTRING
      DO WHILE( O_ASTRING(1:1) .NE. '*' )
         READ(IHEAD,'(A)') O_ASTRING
      ENDDO
C
      EOFILE = .FALSE.
   50 DO WHILE ( .NOT. EOFILE )
C
        EOFILE = .TRUE.
        READ(IHEAD,'(A)',END=100) O_ASTRING
        EOFILE = .FALSE.
C
  100   CONTINUE
C
        IF( .NOT. EOFILE .AND. O_ASTRING(1:1) .NE. '*' ) THEN
          COMMENT = O_ASTRING(26:80)               ! Save the comment.
          LEN_COMM = LENSTR(COMMENT)
          IF( LEN_COMM .EQ. 0 ) LEN_COMM = 1
          CALL STR_UPCASE( O_ASTRING, ASTRING )
          VAR_READ = ASTRING(13:25)
          LPAREN = INDEX( VAR_READ,'(' )
          IKSEE = INDEX(VAR_READ, 'KSEE')
          IF( LPAREN .GT. 0 ) THEN
C-----THE NEXT THREE LINES ARE TO KEEP FROM LISTING ALL 3510 C-LOCATIONS           
            IF(IKSEE .GT. 0) THEN
              GOTO 50
            ENDIF
C------------------------------------------------------------------------            
            TEMP = ' '
C
C---        Determine the C location.  Read the row and column and
C---        expand the array in the new file.
C
            READ(ASTRING(5:8),'(I4)' ) ILOC_IN_C
C
C---        Determine the end of the var_name.  The end can be signaled
C---        by the right paren or just the end of the string.

            LRPAREN =  INDEX(VAR_READ,')' )
            IF( LRPAREN .GT. 0 ) THEN
              IEND = LRPAREN-1
            ELSE
              IEND = LENSTR(VAR_READ)
            ENDIF
C
C---        Search for a comma.  If one is found, then the array is
C---        two dimensional. Read the row element.
C
            KOMMA = INDEX(VAR_READ,',' )
            IF( KOMMA .GT. 0 ) THEN
              CALL READ_INTEGER(IROW, IERR, VAR_READ(LPAREN+1:KOMMA-1))
              CALL READ_INTEGER(ICOL, IERR, VAR_READ(KOMMA+1:IEND))
C             READ(VAR_READ(LPAREN+1:KOMMA-1),'(I2)') IROW
C             READ(VAR_READ(KOMMA+1:IEND),'(I2)') ICOL
            ELSE
              CALL READ_INTEGER(IROW, IERR, VAR_READ(LPAREN+1:IEND))
C             READ(VAR_READ(LPAREN+1:IEND),'(I2)' ) IROW
              ICOL = 0
            ENDIF
C
C---        Now that we know the array size, we only need the array name.
            VAR_READ = VAR_READ(1:LPAREN-1)
C
            IF( ICOL .EQ. 0 ) THEN
C
C---          The array is one dimensional.  The array is written to the
C---          file in reverse order, so that when a INPUT.ASC record refers
C---          to an array without specifying the dimension, the first
C---          reference in the new HEAD file, gives the array dimension.
C
              DO I = IROW, 1, -1
                IF( I .LT. 10 ) THEN
                  WRITE(TEMP,200) VAR_READ(1:LENSTR(VAR_READ)), I
  200             FORMAT( A, '(', I1, ')' )
                ELSEIF( I .LT. 100 ) THEN
                  WRITE(TEMP,220) VAR_READ(1:LENSTR(VAR_READ)), I
  220             FORMAT( A, '(', I2, ')' )
                ELSE
                  WRITE(TEMP,230) VAR_READ(1:LENSTR(VAR_READ)), I
  230             FORMAT( A, '(', I3, ')' )
                ENDIF
C
                WRITE(NUHEAD, 240) ILOC_IN_C + I-1, TEMP(1:13),
     2                             COMMENT(1:LEN_COMM)
  240           FORMAT( T5, I4.4, T13, A, T26, A )
C
              ENDDO
C
            ELSE
C
C---          The array is two-dimensional.  The array is written to the
C---          new file in reverse for the same reason the one dimensional
C---          array is.
C
              ILOC = ILOC_IN_C + (IROW*ICOL)
              DO J = ICOL, 1, -1
                DO I = IROW, 1, -1
                  IF( I .LT. 10 ) THEN
                    WRITE(TEMP,300) VAR_READ(1:LENSTR(VAR_READ)), I
  300               FORMAT( A, '(', I1, ',' )
                  ELSEIF( I .LT. 100 ) THEN
                    WRITE(TEMP,320) VAR_READ(1:LENSTR(VAR_READ)), I
  320               FORMAT( A, '(', I2, ',' )
                 ELSE
                    WRITE(TEMP,330) VAR_READ(1:LENSTR(VAR_READ)), I
  330               FORMAT( A, '(', I3, ',' )
                  ENDIF
                  ISTART = LENSTR(TEMP)+1
                  IF( J .LT. 10 ) THEN
                    WRITE(TEMP(ISTART:ISTART+1),340) J
  340               FORMAT( I1, ')' )
                  ELSEIF( J .LT. 100 ) THEN
                    WRITE(TEMP(ISTART:ISTART+2),360) J
  360               FORMAT( I2, ')' )
                 ELSE
                    WRITE(TEMP(ISTART:ISTART+2),370) J
  370               FORMAT( I3, ')' )
                  ENDIF
C
                ILOC = ILOC - 1
                WRITE(NUHEAD,240) ILOC, TEMP(1:13), COMMENT(1:LEN_COMM)
                ENDDO
              ENDDO
C
            ENDIF
C
          ELSE
C
C---        The variable is not an array, copy line as is
C
            WRITE(NUHEAD,'(A)') O_ASTRING(1:LENSTR(ASTRING))
C
          ENDIF
C
        ENDIF
C
      ENDDO  
C
C---  Add the variables that are defined in the cadac executive to the
C---  new HEAD.ASC.  These variables are added here because they are
C---  not included in the HEAD.ASC file created when DFHEAD is executed 
C---  because they are in the executive and not the modules. 
C
C     C LOC VARIABLE DEFINITION
      WRITE(NUHEAD,240)
     1 0001,'ERRVAL',  'E Maximum Integration step error Value' 
C
      WRITE(NUHEAD,240)
     1 0002,'ERRN',    'E IPL location of variable causing ERRVAL' 
C
      WRITE(NUHEAD,240)
     1 0003,'AERR',    'E C location of variable causing ERRVAL' 
C
      WRITE(NUHEAD,240)
     1 0004,'PRELOC',  'E' 
C
      WRITE(NUHEAD,240)
     1 0051,'REARTH',  'E Radius of the Earth = 20902190.0 Feet' 
C
      WRITE(NUHEAD,240)
     1 0052,'CRAD',    'E Conversion factor = 57.29577951 (Deg/Rad)' 
C
      WRITE(NUHEAD,240)
     1 0053,'OPTMET',  'E Units of measure 1 = metric; 0 = English' 
C
      WRITE(NUHEAD,240)
     1 0054,'AGRAV',   
     2 'E Acceleration due to gravity @ sea level = 32.174 Feet/s**2' 
C
      WRITE(NUHEAD,240)
     1 0055,'CFTM',    'E Conversion factor = 0.3048006 (Meters/Feet)' 
C
      WRITE(NUHEAD,240)
     1 0056,'CKFPS',   'E Converstion factor = 1.6878 (Knots/(ft/s))' 
C
      WRITE(NUHEAD,240)
     1 0057,'AMU',     
     2 'E Gravitational parameter = 1.407654E+16 (Feet**3/s**2)' 
C
      WRITE(NUHEAD,240)
     1 0058,'WEII3',   'E Earth Angular rotation = 7.2921152E-5 (Rad/s)' 
C
      WRITE(NUHEAD,240)
     1 0059,'OPNORO',
     2 'E Option flag: 0=rotating earth model; 1=Non-rotating
     3 earthmodel' 
C
      WRITE(NUHEAD,240)
     1 0090,'RANSEED', 'E Random function generator initialization' 
C
      WRITE(NUHEAD,240)
     1 1600,'MINIT',   'E Flag: InitMode I1I2: 00=Flight Path Angle;
     2 01=Target Centered; 11=Look Angle; 10=Launch Point Centered' 
C
      WRITE(NUHEAD,240)
     1 1772,'TRCOND',  'E Terminate Condition Codes from right to left' 
C
      WRITE(NUHEAD,240)
     1 1800,'ISWEEP',  'E (Sweep) Sweep option flag (0 through 4)' 
C
      WRITE(NUHEAD,240)
     1 1801,'CRITNO',  'E (Sweep) Critical variable C location' 
C
      WRITE(NUHEAD,240)
     1 1802,'CRITVAL', 'E (Sweep) Minimum test for critical variable' 
C
      WRITE(NUHEAD,240)
     1 1803,'SEARNO',  'E (Sweep) Number of binary search runs (opt 4)' 
C
      WRITE(NUHEAD,240)
     1 1804,'NUMR',    'E (Sweep) The number of trajectory runs' 
C
      WRITE(NUHEAD,240)
     1 1805,'CRITMAX', 
     2 'E (Sweep) The maximum test for critical variable' 
C
      WRITE(NUHEAD,240)
     1 1811,'ANGLNO',
     2 'E (Sweep) The C location of the angluar variable' 
C
      WRITE(NUHEAD,240)
     1 1812,'ANGMIN',  'E (Sweep) The minimum angle value' 
C
      WRITE(NUHEAD,240)
     1 1813,'ANGMAX',  'E (Sweep) The maximum angle value' 
C
      WRITE(NUHEAD,240)
     1 1814,'ANGDEL',  'E (Sweep) The Delta angle' 
C
      WRITE(NUHEAD,240)
     1 1815,'ANGUNT',  
     2 'E (Sweep) The units of the input data: rad or deg' 
C
      WRITE(NUHEAD,240)
     1 1821,'RANGNO',  'E (Sweep) The C location of the range variable' 
C
      WRITE(NUHEAD,240)
     1 1822,'RANMIN',  'E (Sweep) The minimum range value' 
C
      WRITE(NUHEAD,240)
     1 1823,'RANMAX',  'E (Sweep) The maximum range value' 
C
      WRITE(NUHEAD,240)
     1 1824,'RANDEL',  'E (Sweep) The delta range value' 
C
      WRITE(NUHEAD,240)
     1 1837,'ANGX',    'E (Sweep) The polar angle from target' 
C
      WRITE(NUHEAD,240)
     1 1838,'RANG',    'E (Sweep) The range (distance) from target' 
C
      WRITE(NUHEAD,240)
     1 2000,'TIME',    'E Trajectory time - s' 
C
      WRITE(NUHEAD,240)
     1 2001,'TSTAGE',  'E Time in current stage - s'
C
      WRITE(NUHEAD,240)
     1 2003,'PCNT',    'E Time of the next print at TABOUT.'
C
      WRITE(NUHEAD,240)
     1 2004,'PPNT',    'E Time of next print to the plot files.'
C
      WRITE(NUHEAD,240)
     1 2005,'PPP',
     2 'E Time interval writing to TRAJ.BIN or TRAJ.ASC - sec'
C
      WRITE(NUHEAD,240)
     1 2006,'ITAP90',   'E Flag:  0= No CSAVE.ASC; 1= trajectory
     2 started from data saved to CSAVE.ASC (used by D3I)'
C
      WRITE(NUHEAD,240)
     1 2011,'KSTEP',    'E Controls flow after an integration step'
C
      WRITE(NUHEAD,240)
     1 2014,'ITCNT',    'E Flag'
C
      WRITE(NUHEAD,240)
     1 2015,'CPP',
     2 'E Time interval writing to Screen or TABOUT.ASC - sec'
C      
      WRITE(NUHEAD,240)
     1 2016,'PGCNT',    'E Flag'
C      
      WRITE(NUHEAD,240)
     1 2020,'LCONV',    'E Flag: 0= start of trajectory;
     2 2= stop trajectory calculations'
C      
      DO IJ = 2127,2196,1
      WRITE(NUHEAD,240) 
     1 IJ,'PMIN',      'E '
      ENDDO 
C
      WRITE(NUHEAD,240) 
     1 2280,'NV',      'E The number of variables in the plot list.' 
C
      WRITE(NUHEAD,240) 
     1 2285,'NJ',      'E Flag' 
C
      WRITE(NUHEAD,240) 
     1 2361,'NOMOD',   'E The number of modules to be called.' 
C
      DO IJ = 2362,2460,1
      WRITE(NUHEAD,240) 
     1 IJ,'XMODNO(99)','E The list of the module numbers to be called by
     2 the Exe, in the calling order.' 
	ENDDO
C
      WRITE(NUHEAD,240) 
     1 2461,'NOSUB',   'E The number of the ouput and auxiliary
     2 subroutines to be called by the Exe.' 
C
      DO IJ = 2462,2560,1
      WRITE(NUHEAD,240) 
     1 IJ,'SUBNO(99)', 'E The list of output and auxiliary subroutine
     2 numbers to be called by the Exe.' 
	ENDDO
C
      WRITE(NUHEAD,240) 
     1 2561,'NIP',     'E The number of variables being integrated.' 
C
      DO IJ = 2562,2661,1
      WRITE(NUHEAD,240) 
     1 IJ,'IPL(100)',  
     2 'E The locations of the derivative of the state variable.' 
	ENDDO
C
      WRITE(NUHEAD,240) 
     1 2662,'HMIN',    'E ' 
C
      WRITE(NUHEAD,240) 
     1 2663,'HMAX',    'E ' 
C
      DO IJ = 2664,2764,1
      WRITE(NUHEAD,240) 
     1 IJ,'DER',       'E Integration interval - sec' 
	ENDDO
C
      DO IJ = 2765,2865,1
      WRITE(NUHEAD,240) 
     1 IJ,'V(101)',    'E ' 
	ENDDO
C
      WRITE(NUHEAD,240) 
     1 2866,'ICOOR',    
     2 'E Flag:  -1= Executive is in initialization mode;
     3 0= Integration predictor cycle;
     4 1= Integration corrector cycle' 
C
      DO IJ = 2867,2967,1
      WRITE(NUHEAD,240) 
     1 IJ,'IPLV(100)', 'E The location of the state variable;
     2 corresponding to the derivative in the IPLV array' 
	ENDDO
C
      CLOSE( NUHEAD )
      CLOSE( IHEAD )
C
      RETURN
      END                               
      SUBROUTINE END_MSG_STOP( MSG )
C
C----------------------------------------------------------------------
C
C    This module issues an error message when a premature end is reached
C    and exits the program.
C
C----------------------------------------------------------------------
C      
      COMMON /FILE_IDS/ INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
C      
      CHARACTER*(*) MSG
C
C  Display message
C
      WRITE(*,100) MSG
  100 FORMAT( // ' ', A, // ' * * * PROGRAM TERMINATED * * * ' )
C
      WRITE(JERROR,110) MSG
  110 FORMAT( // ' ', A )
C
C  Shutdown Files
C
      CLOSE( JERROR )
      CLOSE( INPUT )
      CLOSE( IOUTPUT )
      CLOSE( NUHEAD ,STATUS='DELETE')
C
C  End Program
C
      STOP ' '
      END
      SUBROUTINE FILE_COPY( FILE1, FILE2 )
C
C----------------------------------------------------------------------
C
C     This module copies the file with the name FILE1 to the file with 
C     with the name IFILE2.  
C
C----------------------------------------------------------------------
C     
      CHARACTER*(*) FILE1, FILE2
      CHARACTER STRING_132*132
C
      IFILE1 = 10
      IFILE2 = 11
C      
C---  Open the original file.
C
      OPEN(IFILE1, FILE=FILE1(1:LENSTR(FILE1)), STATUS='OLD', ERR=900 )
C
C---  Open the new file with the status of old to see if it exists.  If
C---  it does close it with the delete option.
C
      OPEN(IFILE2, FILE=FILE2(1:LENSTR(FILE2)), STATUS='OLD',ERR=100 )
      CLOSE(IFILE2,STATUS='DELETE')
C
  100 CONTINUE
      OPEN(IFILE2, FILE=FILE2(1:LENSTR(FILE2)), STATUS='NEW', ERR=910 )                 
C
C  Read/Write Files
C
  200 READ(IFILE1, '(A)', END=300) STRING_132
      ILEN =  LENSTR( STRING_132) 
      IF( ILEN .EQ. 0 ) THEN
        WRITE(IFILE2,'(A)') ' '
      ELSE
        WRITE(IFILE2,'(A)') STRING_132(1:LENSTR(STRING_132))
      ENDIF
      GOTO 200
C
  300 CONTINUE
C
      RETURN
C
  900 CONTINUE
  910 CONTINUE
      END
      SUBROUTINE FIND_VARIABLE ( TLOC, LOCALVAR, FIND, DEFAN)
C
C---------------------------------------------------------------------
C
C  Locates variable in the Header file by its "C" location and returns the 
C   name.
C
C---------------------------------------------------------------------
C
C      
      COMMON /FILE_IDS/ INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
C 
CC      CHARACTER*(*) TLOC_IN_C, VAR2
      CHARACTER TLOC*4, LOCALVAR*13
      CHARACTER TLOC2*4,  LOCALVAR3*13, STRING*132, DEFAN*100
      INTEGER ILOC2, ILOC
      LOGICAL FIND, EOFILE, EOFILE2
C
C  Open File
C
      OPEN(NUHEAD, FILE = 'NUHEAD.ASC', FORM = 'FORMATTED',
     1      STATUS = 'UNKNOWN')
C              
      FIND = .FALSE.
C
C  Get Data
C
  100 READ(NUHEAD,'(A)',END=999) STRING
      READ(STRING,200)  TLOC2, LOCALVAR3, DEFAN
  200 FORMAT( T5, A, T13, A, T26, A )
C                                      
      READ(TLOC,110) ILOC
  110 FORMAT(I4)
      READ(TLOC2,110) ILOC2      
C
C  Process data
C
      IF ( ILOC2 .EQ. ILOC ) THEN
          IF(LENSTR(DEFAN) .EQ. 0 .AND. .NOT. EOFILE2 ) THEN
              EOFILE = .FALSE.
              DO WHILE ( .NOT. EOFILE )
                  EOFILE = .TRUE.          
	            READ(NUHEAD, '(A)', END=220) STRING
	            EOFILE = .FALSE.
	            READ(STRING,200) TLOC2,LOCALVAR3,DEFAN
	            READ(TLOC2,110) ILOC2
	            IF( ILOC2 .EQ. ILOC ) THEN
	                GOTO 210
	            ENDIF
	        ENDDO
	     ENDIF
  210	     CONTINUE
           LOCALVAR = LOCALVAR3          
           FIND = .TRUE.
	     EOFILE2 = .FALSE.
      ELSE 
          GOTO 100
      ENDIF
	GOTO 999
C    
  220 REWIND(NUHEAD)
      EOFILE2 = .TRUE.
      GOTO 100
C                                              
  999 CLOSE (NUHEAD)
      END
      SUBROUTINE GET_FILE( FILE_NAME, FILE_MSG, FILETYPE, EXIT_PROG )
C
C--------------------------------------------------------------------
C
C     This module prompts the user for the name of a file; If the file
C     is an input file, this module opens the file and verifies that it
C     exists.  If the file is an output file, the module verifies that 
C     it is a valid name.  If the filename is not valid, the user may 
C     enter a new file or exit the program.
C
C----------------------------------------------------------------------
C      
      COMMON /PATH_DIR/ FDRIVE,   FDIR
      CHARACTER FDRIVE*2, FDIR*60, FNAME*8, FEXT*4
C
      CHARACTER FILE_MSG*80, FILE_NAME*60, INFILE*60, DIRPATH*80, ANSW*2
C
      LOGICAL GOOD_FILE, EXIT_PROG
C
      INTEGER FILETYPE, OUTPUTFILE
      INTEGER*4 LENDIR, GETDRIVEDIRQQ
      INTEGER SPLITPATHQQ, CHANGEDRIVEQQ, CHANGEDIRQQ
C
      DATA OUTPUTFILE / 1 /      
C                        
C
    5 GOOD_FILE = .FALSE.    
      DO WHILE ( .NOT. GOOD_FILE )
C
C  Get file Name
	  CALL CLEAR_SCREEN
	  LENDIR = GETDRIVEDIRQQ( DIRPATH )  
C
CC        WRITE(*,'(////5X,A/5X,A//5X,A/5X,A//5X,A,/5X,A,A,A\)') 
CC     1  'This Program Inputs a Fixed-Formated File',
CC     2  'and Outputs a Free-Formed File.',
CC     3      'Current Directory is :', DIRPATH, 
CC     4      FILE_MSG(1:LENSTR(FILE_MSG)),
CC     5      'Default = ', FILE_NAME(1:LENSTR(FILE_NAME)), ' : '
        WRITE(*,'(//5X,A/5X,A//5X,A,/5X,A,A,A\)') 
     1        'Current Directory is :', DIRPATH, 
     2        FILE_MSG(1:LENSTR(FILE_MSG)),
     3        'Default = ', FILE_NAME(1:LENSTR(FILE_NAME)), ' : '
C     
	  READ(*,'(A)') INFILE
	  IF( LENSTR(INFILE) .EQ. 0 ) INFILE = FILE_NAME
C
        OPEN(3, FILE=INFILE(1:LENSTR(INFILE)), STATUS='OLD', ERR=10)
	  GOOD_FILE = .TRUE. 
	  CLOSE( 3 )
C
C***  IF GOOD_FILE = TRUE AND FILETYPE = 1 => OUTPUTFILE; THEN FILE 
C***  ALREADY EXISTS.  WARN USER
C
      ANSW = 'N'
      IF( GOOD_FILE .AND. FILETYPE .EQ. OUTPUTFILE ) THEN
         WRITE(*,'(//8X,A/5X,A/5X,A/5X,A,A,A\)')
     1    '*** WARNING ***',
     2    'OUTPUT FILE ALREADY EXISTS - DO YOU WISH TO CHOOSE', 
     3    'A DIFFERENT NAME (Y or N)',
     4    'Default = ', ANSW, ': '
         READ(*,'(A)') ANSW
         IF( LENSTR(ANSW) .EQ. 0) ANSW = 'N'
C
         IF( ANSW .EQ. 'Y' .OR. ANSW .EQ. 'y' ) GOTO 5
      ENDIF
C
        ILEN = SPLITPATHQQ( INFILE, FDRIVE, FDIR, FNAME, FEXT )
        ILEND = CHANGEDRIVEQQ( FDRIVE )
        ILENDIR = CHANGEDIRQQ( FDIR(1:LENSTR(FDIR)-1) )
        DIRPATH = FDRIVE // FDIR(1:LENSTR(FDIR)-1)
        INFILE = FDRIVE // FDIR(1:LENSTR(FDIR)) //
     1            FNAME(1:LENSTR(FNAME)) // FEXT 
   10   CONTINUE
C
C***  IF FILETYPE = 0 THEN FILE WAS SUPPOSED TO EXIST
      IF( FILETYPE .EQ. 0) GOTO 30
C             
      IF( .NOT. GOOD_FILE .AND. FILETYPE .EQ. OUTPUTFILE ) THEN
C
C                        Output File
C---      If the file is an output file then it is possible it doesn't 
C---      exist, which would create an error with the previous open 
C---      statement.  Therefore, try to open the file with a new status.
C---      If an error still occurs, then it is a bad file name.
C
	    OPEN(3, FILE=INFILE(1:LENSTR(INFILE)), STATUS='NEW', ERR=20)
	    GOOD_FILE = .TRUE. 
	    CLOSE( 3 )  
C
   20     CONTINUE
C   
      ENDIF
C                
   30 CONTINUE
C
      IF( .NOT. GOOD_FILE ) THEN 
C
C  Bad Filename
C
	    WRITE(*,'(/5X,A/5X,A/5X,A\)' )
     1         '    * * * Invalid Filename * * *',
     2         '       Do you wish to continue? (Y or N)',
     3         '       Default = Y : '
	    READ(*,'(A)') ANS
	    IF( ANS .EQ. 'N' .OR. ANS .EQ. 'n' ) THEN
	      EXIT_PROG = .TRUE.        
	      RETURN  
	    ELSE
	      EXIT_PROG = .FALSE.
	    ENDIF        
C                 
        ELSE   
C        
          FILE_NAME = INFILE
C          
	  ENDIF
C
      ENDDO   
C      
      RETURN
      END
      SUBROUTINE GET_WK_STRING(INLINE, END_OF_FILE )
C
C----------------------------------------------------------------------
C
C     This module determines the string to decode, removing and leading
C     blanks.
C
C----------------------------------------------------------------------
C
C    INLINE      - (O) Input record from file
C    END_OF_FILE - (O) EOF logical flag
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
C
      CHARACTER  INLINE*132
      LOGICAL END_OF_FILE
C
      END_OF_FILE = .FALSE.
C
  100 READ(INPUT,'(A)', END=900 ) INLINE
C
C---  Inform the user of the status of the program.
C
CJH      PRINT*, CHAR(27) // '[2J'
CJH      WRITE(*,'(/ 5X, A / 5X, A)' ) 
CJH     1  'Now processing line:',  
CJH     2   INLINE(1:LENSTR(INLINE))
C
      CALL STR_UPCASE( INLINE, INLINE )
      RETURN
C
C  Found an EOF on the file
C
  900 END_OF_FILE = .TRUE.  
C        
      RETURN
      END
      SUBROUTINE ISSUE_ERR_MSG( MSG )
C
C----------------------------------------------------------------------
C
C    This module issues an error message when a error occurs and prints
C    the line that contained the error.
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /INPUT_STRING/ INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      CHARACTER OUTLINE*132
      COMMON /ERROR_INFO/ JERR
C
      CHARACTER INLINE*132
      CHARACTER*(*) MSG
      DATA JERR /0/
C
C---  Increment the number of errors.
      JERR = JERR + 1
C
C  Display error to error file
C
      WRITE(JERROR,100) MSG, INLINE
  100 FORMAT( ' ', '* * * ', A / 5X, ':', A / )
C
C  Display error to .ASC file
C
      WRITE(IOUTPUT,100) MSG, INLINE
C
C  Display error message to screen
C
      WRITE(*,100) MSG, INLINE
C
      RETURN
      END
      FUNCTION LAST_CHAR( INPOS, STRING )
C
C----------------------------------------------------------------------
C
C     This module determines the postion of the last character of the
C     first word in the string passed in.
C
C----------------------------------------------------------------------
C
      CHARACTER*(*) STRING
      LOGICAL LAST_POS
C
      IPOS = INPOS
      LAST_POS = .FALSE.
      DO WHILE( IPOS .LE. LENSTR(STRING) .AND. .NOT. LAST_POS )
        IF( STRING(IPOS:IPOS) .EQ. ' ' ) THEN
          LAST_POS = .TRUE.
        ELSE
          IPOS = IPOS + 1
        ENDIF
      ENDDO
C
      LAST_CHAR = IPOS - 1
C
      RETURN
      END
      FUNCTION LENSTR( THESTRING )
C
C----------------------------------------------------------------------
C
C  This function searches the text contained in the string variable for
C  the end of the text.  The function returns the character location
C  of the last non-blank character location.  This is useful in
C  locating the end of text within a string.  The module will work
C  with any length input string.
C
C----------------------------------------------------------------------
C
C  THESTRING - (C) Input.  The character string to be searched for the
C                  end of the text string.
C  LENSTR - (I) The location of the last non-blank character in
C               THESTRING.  A 0 value is returned if the string is
C               completely blank
C
C----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
C
      LENGTH = LEN( THESTRING )
C
      DO WHILE ( LENGTH .GT. 0 .AND. THESTRING(LENGTH:LENGTH) .EQ. ' ' )
         LENGTH = LENGTH - 1
      ENDDO
C
      LENSTR = LENGTH
      RETURN
      END
      FUNCTION NXT_CHAR(IN_POS, THESTRING)
C
C--------------------------------------------------------------------
C
C  This function returns the location of the next non-blank character
C  in a string.  The module will work with any length input string.
C
C----------------------------------------------------------------------
C
C  THESTRING - (C) Input.  The character string to be searched for the
C                  next non-blank character.
C
C  LOC_FRST - (I) The location of the next non-blank character in
C                 THESTRING.
C
C----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
C                
      LEN_THESTRING = LEN(THESTRING)
      IPOSITION = IN_POS
      IF( THESTRING(IPOSITION:IPOSITION) .EQ. ' ' ) THEN
        DO WHILE ( THESTRING(IPOSITION:IPOSITION) .EQ. ' ')
          IF ( IPOSITION +1 .GT. LEN_THESTRING ) THEN 
            GOTO 100
          ENDIF
          IPOSITION = IPOSITION + 1                       
        ENDDO
      ENDIF
C                                                
  100 CONTINUE
      NXT_CHAR = IPOSITION

      RETURN
      END
      SUBROUTINE PREPARE_FILES
C
C----------------------------------------------------------------------
C
C     This module prompts for the input file and opens the input
C     and output files.
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
      COMMON /FILE_NAMES/ INPUT_NAME, INPUT_BACKUP, OUTPUT_NAME,
     1                    OUTPUT_BACKUP, ERROR_FILE   
      CHARACTER*60        INPUT_NAME, INPUT_BACKUP, OUTPUT_NAME,
     1                    OUTPUT_BACKUP, ERROR_FILE
      COMMON /PATH_DIR/ FDRIVE,   FDIR
      CHARACTER MSG*80, FDRIVE*2, FDIR*60, FNAME*8, FEXT*4
C
      LOGICAL EXIT_PROG
      INTEGER SPLITPATHQQ
C
C  Set up file unit numbers
C
      CALL SET_FILE_IDS
C     
	  CALL CLEAR_SCREEN
C
        WRITE(*,'(//5X,A/5X,A)') 
     1  'This Program Inputs a Fixed-Formated File',
     2  'and Outputs a Free-Formated File.'
C
C  Set the parameters needed for prompting for the input HEAD.ASC file.
C
      INPUT_NAME = 'CADIN.ASC'   
      MSG = 'Enter name of Fixed-Formated file to transform:'
C  
C  Prompt the user for the input fixed-form file.
C
      CALL GET_FILE( INPUT_NAME, MSG, 0, EXIT_PROG ) 
C
      IF( EXIT_PROG ) STOP ' '
C                                 
C---  Break up the path and file name of the input file INPUT_NAME.  The
C---  information will be used when establishing INPUT_BACKUP.
C
      ILEN = SPLITPATHQQ( INPUT_NAME, FDRIVE, FDIR, FNAME, FEXT ) 
C
C  Set the input file name to its full name including its path.
C
      INPUT_NAME = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
     1             FNAME(1:LENSTR(FNAME)) // FEXT
C      
C  Set the file name for the backup of the input file.
C
      INPUT_BACKUP = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
     1               FNAME(1:LENSTR(FNAME)) // '.TP5'
C
C  Open the INPUT file.
C
      OPEN(INPUT, FILE=INPUT_NAME(1:LENSTR(INPUT_NAME)),
     1     FORM='FORMATTED', STATUS='UNKNOWN', ERR=999)
C     
C  Set the parameters needed for prompting for the OUTPUT file.
C
      OUTPUT_NAME = 'INPUT.ASC'
      MSG = 'Enter name of Free-Formated file for output:'
C     
C  
C  Prompt the user for the input free-form file.
C
      CALL GET_FILE( OUTPUT_NAME, MSG, 1, EXIT_PROG ) 
C
      IF( EXIT_PROG ) STOP ' '
C                                 
C---  Break up the path and file name of the output file OUTPUT_NAME.  The
C---  information will be used when establishing OUTPUT_BACKUP.
C
      ILEN = SPLITPATHQQ( OUTPUT_NAME, FDRIVE, FDIR, FNAME, FEXT ) 
C
C  Set the output file name to its full name including its path.
C
      OUTPUT_NAME = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
     1             FNAME(1:LENSTR(FNAME)) // FEXT
CC      
CC  Set the file name for the backup of the output file.
CC
CC      OUTPUT_BACKUP = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
CC     1               FNAME(1:LENSTR(FNAME)) // '.BAK'
CC
CC     
CC---  The output file backup has the extenstion ".TP5"
CC---  This file will be located in the default directory.
CC
CC      OUTPUT_BACKUP = 'INPUT.TP5'
C
C  Open the OUTPUT and ERROR files.
C
      OPEN(IOUTPUT,FILE=OUTPUT_NAME(1:LENSTR(OUTPUT_NAME)),
     1     FORM='FORMATTED',STATUS='UNKNOWN', ERR=999 )
C      
C  Open the error file. But first build directory and path
C   from OUTPUT FILE data.
C
      FNAME = 'ERROR'
      FEXT = '.ASC'
      ERROR_FILE = FDRIVE // FDIR(1:LENSTR(FDIR)) //
     1             FNAME(1:LENSTR(FNAME)) // FEXT
      OPEN( JERROR, FILE=ERROR_FILE(1:LENSTR(ERROR_FILE)),
     1      STATUS='UNKNOWN' )
C
C
  999 RETURN
      END     
      SUBROUTINE PUT_TITLE
C
C----------------------------------------------------------------------
C
C     This module reads the title from the CADIN.ASC file and places it
C     on the free-form output file
C
C----------------------------------------------------------------------
C                         
      COMMON / INPUT_STRING / INLINE
      COMMON /OUTPUT_STRING/ OUTLINE
      COMMON /FILE_IDS/ INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
C
      CHARACTER INLINE*132, OUTLINE*132
      LOGICAL END_OF_FILE
C
C---  Initialize outline.
C
      OUTLINE = '                                                      '
C                
  100 CALL GET_WK_STRING(INLINE, END_OF_FILE )
      OUTLINE = 'TITLE ' // INLINE                   
C      
      WRITE(IOUTPUT,'(A)') OUTLINE(1:LENSTR(OUTLINE))
C     
      CALL GET_WK_STRING(INLINE, END_OF_FILE ) ! Read the 2nd line.                       
C      
      RETURN
      END            
      SUBROUTINE READ_INTERNAL( R1, IERR, STRING )
C
C----------------------------------------------------------------------
C
C  This module performs an interactive conversion of CHARACTER data
C  to a real value entered from the user. This module was written 
C  to provide ease of transfer to the PC and for modularity. 
C
C----------------------------------------------------------------------
C
C  R1     - (R) OUTPUT. The real numbers read from the string passed
C                       into this module.
C  IERR   - (I) OUTPUT. An error indicator flag.  IERR is set to 0 at
C                       the beginning of the module.  If an error occurs
C                       during the internal read, IERR is set to 1.
C  STRING - (C) INPUT.  The string that contains the real number to read.
C
C----------------------------------------------------------------------
C
      CHARACTER*(*) STRING
C
      IERR = 0
C
C---  Create a temp file
C
      KSCRATCH = 99
      OPEN(KSCRATCH, FORM = 'FORMATTED', STATUS = 'SCRATCH')
C
C---  Write the string to the temp file.
C
      WRITE(KSCRATCH,'(A)') STRING
C
C---  Rewind the file and read the string as a real number.
C
      REWIND( KSCRATCH )
      READ(KSCRATCH,*,ERR=900) R1
C
      GOTO 999
C
  900 IERR = 1
C
  999 CLOSE( KSCRATCH )
      RETURN
      END
      SUBROUTINE READ_INTEGER( I1, IERR, STRING )
C
C----------------------------------------------------------------------
C
C  This module performs an interactive converstion of a character
C  string to a integer value. This module was written to provide
C  ease of transfer to the PC and for modularity. 
C
C----------------------------------------------------------------------
C
C  I1     - (I) OUTPUT. The integer read from the string passed
C                       into this module.
C  IERR   - (I) OUTPUT. An error indicator flag.  IERR is set to 0 at
C                       the beginning of the module.  If an error occurs
C                       during the internal read, IERR is set to 1.
C  STRING - (C) INPUT.  The string that contains the real number to read.
C
C----------------------------------------------------------------------
C
      CHARACTER*(*) STRING
C
      IERR = 0
C
C---  Create a temp file
C
      KSCRATCH = 99
      OPEN(KSCRATCH, FORM = 'FORMATTED', STATUS = 'SCRATCH')
C
C---  Write the string to the temp file.
C
      WRITE(KSCRATCH,'(A)') STRING
C
C---  Rewind the file and read the string as an integer number.
C
      REWIND( KSCRATCH )
      READ(KSCRATCH,*,ERR=900) I1
C
      GOTO 999
C
  900 IERR = 1
C
  999 CLOSE( KSCRATCH )
      RETURN
      END  
      SUBROUTINE SET_FILE_IDS
C
C----------------------------------------------------------------------
C
C     This module assigns the unit numbers for the files accessed in
C     this program.
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, IOUTPUT, IHEAD, NUHEAD, JERROR
C
      INPUT  = 31
      IOUTPUT = 33
      IHEAD  = 40
      NUHEAD = 41
      JERROR = 21
C
      RETURN
      END   
      SUBROUTINE STR_UPCASE(LINEIN, LINEOUT)
C
C----------------------------------------------------------------------
C
C     This module converts a string to all uppercase letters.
C
C----------------------------------------------------------------------
C
      CHARACTER*(*) LINEIN, LINEOUT
      INTEGER ASC_VAL
C       
      LINEOUT = LINEIN
      LINE_LEN = LENSTR(LINEOUT)
C      
      DO I = 1, LINE_LEN
        ASC_VAL = ICHAR(LINEOUT(I:I))
        IF( ASC_VAL .GE. 97 .AND. ASC_VAL .LE. 122 ) THEN
          LINEOUT(I:I) = CHAR( ASC_VAL - 32 )
        ENDIF
      ENDDO
C
      RETURN
      END
