      PROGRAM CADIN3
C
C----------------------------------------------------------------------
C
C     CADIN VERSION 3.0               V 3.0              June      2000
C     CADIN VERSION 2.5               V 2.5              February  2000
C     CADIN VERSION 2.4               V 2.4              August    1998
C     CADIN VERSION 2.3               V 2.3              August    1998
C     CADIN VERSION 2.2               V 2.2              July      1998
C     CADIN VERSION 2.1               V 2.1              June      1998
C     CADIN VERSION 2.0               V 2.0              September 1996
C     CADIN Version 1.0               V 1.0              March     1995
C
C----------------------------------------------------------------------
C
C     This program translates a free form user created cadac input
C     file into a file with the fixed format for input into CADAC.
C
C     Executive module for program CADAC - PC, Version 1 March 1995
C
C
C----------------------------------------------------------------------
C
C     Created by:  TYBRIN Corporation
C                  1030 Titan Court
C                  Ft. Walton Beach, Florida 32547
C
C                  Voice: (850) 337-2500
C                  Fax:   (850) 337-2534
C
C     Created for: AFRL/MNG
C                  Eglin Air Force Base, Florida 32542-6817
C
C                  Voice: (850) 882-3722
C                  Fax:   (850) 882-9049
C
C--Program History------------------------------------------------------
C
C      CADIN3   2000 June
C        - Version increased with release of new CADAC Studio
C      CADIN25  2000 March
C        - Took out variable SAME_VAR2 in subroutine SEARCH_HEAD.  See
C          subroutine SEARCH_HEAD for detailed comment.
C      CADIN25  2000 February
C        - Redefined the type 04 card to allow full line comments
C      CADIN24  1998 August
C        - Redid Sweep Block.  Added code to properly handle input data.
C        - Changed the definition of "TIME" at Dr. Zipfel's request.
C      CADIN23  1998 August
C        - Removed screen scoll at Dr. Zipfel's request.
C      CADIN22  1998 July
C        - Made "Num" =4 for TEST & TEST2 cases if "Num" not in Sweep Block
C      CADIN21  1998 June
C        - Added auto calculation of "Num" in Sweep Block
C        - Corrected minor problems
C      CADIN20  1996 August
C        - Corrected minor problems
C               1996 June
C        - Corrected minor problems
C               1995 Mar
C        - program CADN was renamed CADIN
C        - the input and output file names were modified
C        - the prompts were modified to be consistent with all the
C          CADAC utility programs
C
C--Programming Notes ---------------------------------------------------
C
C
C
C--File Usage ----------------------------------------------------------
C
C  Unit    Ref          Name                Description
C   21     JERROR        ERROR.ASC   -  ERROR FILE
C   31     INPUT         INPUT.ASC   -  INPUT FILE
C   32     NEWINPUT                  -  
C   33     ICADIN        CADIN.ASC   -  OUTPUT FILE
C   40     IHEAD         HEAD.ASC    -  HEADER FILE
C   41     NUHEAD                       
C
C--Module definition----------------------------------------------------
C
C   This module is the MAIN controlling module for the CADIN 
C   program.
C
C--Alphabetical Modules List--------------------------------------------
C
C     CAL_NUM         -
C     CHECK_4_KEYS    -
C     CHECK_4_VARS    -
C     CLEAR_SCREEN    -
C     CLOSE_FILES     -
C     COPY_HEAD       -
C     CLEAN_VAR       -
C     CT3_ASSIGN      -
C     CT3_VALUE       -
C     CT3_VARIABLE    -
C     DCODE_LINES     -
C     END_MSG_STOP    -
C     FILE_COPY       -
C     GET_FUNC_PARS   -
C     GET_FILE        -
C     GET_FUNC_VARS   -
C     GET_TITLE       -
C     GET_WK_STRING   -
C     ISSUE_ERR_MSG   -
C     KEY_CLEAR       -
C     KEY_FUNC        -
C     KEY_HEADER      -
C     KEY_IF          -
C     KEY_LOAD        -
C     KEY_MODULE      -
C     KEY_MONTE       -
C     KEY_RAND        -
C     KEY_RUN         -
C     KEY_SAVE        -
C     KEY_STOP        -
C     KEY_SWEEP       -
C     KEY_VECT        -
C     KEY_VECTV       -
C     KEY_WEATH       -
C     LAST_CHAR       -
C     LENSTR          -
C     NXT_CHAR        -
C     PREPARE_FILES   -
C     READ_INTEGER    - 
C     READ_INTERNAL   -
C     READ_WEATH      -
C     SEARCH_HEAD     - 
C     SET_FILE_IDS    -
C     STR_UPCASE      -
C     SWEEP_DEGR_RADS -
C     SWEEP_DELT      -
C     SWEEP_LIMITS    -
C     SWEEP_MODE      -
C     SWEEP_NUMBER    -
C     WRITE_CARD1     -
C     WRITE_COMMENT   -
C     WRITE_INTERNAL  -
C     WRITE_NEWINPUT_WITH_COMMENT - 
C
C-----------------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      COMMON /INPUT_STRING/ INLINE,     WK_STRING
      CHARACTER             INLINE*132, WK_STRING*132
C
      LOGICAL END_OF_FILE, KEY_FOUND, SAVE_TYPE4
      INTEGER CHEKMONT
C
      WRITE(*,'(5X, A)')'CADIN - Verison 3.0'
C
      CALL PREPARE_FILES
C      
      CALL COPY_HEAD 
C
      CALL GET_TITLE
C 
      WRITE(*,'(// 5X, A /)' ) 
     1  'Now processing Commands in INPUT file!'
C
      SAVE_TYPE4 = .TRUE.
C
      CALL GET_WK_STRING( END_OF_FILE, SAVE_TYPE4)    ! read the next line.
      CHEKMONT = 0
      CHEKMONT = INDEX(INLINE(1:10),'MON')
      IF( CHEKMONT .LT. 1 ) THEN
          CALL WRITE_CARD1                            ! Monte Carlo's not there
          CHEKMONT = 0
      ENDIF
C      
C---  Check for keywords at the beginning of the line.
      KEY_FOUND = .FALSE.
      CALL CHECK_4_KEYS( KEY_FOUND )
C---  Chech to see if #1 cards have been written
C
      IF( CHEKMONT .GT. 0 ) THEN                      ! #1 Cards have NOT been written      
        CALL WRITE_CARD1
      ENDIF
C
      CALL DCODE_LINES       
C
      CALL CLOSE_FILES
C
      WRITE(*,'(// 5X, A /)' ) 
     1  'Processing Finished!'
C
      STOP
      END
      SUBROUTINE CAL_NUM(NUM,RMAX,RMIN,RDELTA)
C
C----------------------------------------------------------------------
C
C     This module calculates NUM_TRAJ_BIN for sweep cases 0-3.
C     Cases 4&5 use the NUM_TRAJ_BIN provided by the user.
C
C----------------------------------------------------------------------
C
C  NUM    - (I) Output.  The integer number of NUM_TRAJ_BIN.
C  RMAX   - (R) Input.   Max range value
C  RMIN   - (R) Input.   Min range value
C  RDELTA - (R) Input.   Delta range value
C
      NUM = ((RMAX-RMIN)/RDELTA) + 1
C
      RETURN
      END
      SUBROUTINE CHECK_4_KEYS(KEY_FOUND)
C
C----------------------------------------------------------------------
C
C     This module checks INLINE for keywords.
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      LOGICAL KEY_FOUND
C
      DIMENSION KEYS(15)
      CHARACTER KEYS*80
C
      DATA NUM_KEYS / 15 /
      DATA KEYS / 'CLEAR',  'FUNC',    'HEAD',   'IF',    'LOAD', 
     1            'MODULE', 'MONTE',   'RUN',    'SAVE',  'STOP',  
     2            'SWEEP',  'VECTORV', 'VECTOR', 'WEATH', 'RANDOM' /
C
      I = 1
      DO WHILE ( I .LE. NUM_KEYS .AND. .NOT. KEY_FOUND )
         KEY_LEN = LENSTR(KEYS(I))
         IF( KEYS(I)(1:KEY_LEN) .EQ. WK_STRING(1:KEY_LEN)) THEN
           KEY_FOUND=.TRUE.
         ELSE
           I = I + 1
         ENDIF
      ENDDO
C
      IF( .NOT. KEY_FOUND ) RETURN
C
      GOTO(110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 
     1     210, 220, 230, 240, 250 ) I
C
  110 CALL KEY_CLEAR
      RETURN
C
  120 CALL KEY_FUNC 
      RETURN
C
  130 CALL KEY_HEADER
      RETURN
C
  140 CALL KEY_IF
      RETURN
C
  150 CALL KEY_LOAD
      RETURN
C
  160 CALL KEY_MODULE
      RETURN
C
  170 CALL KEY_MONTE
      RETURN
C
  180 CALL KEY_RUN
      RETURN
C
  190 CALL KEY_SAVE
      RETURN
C  
  200 CALL KEY_STOP
      RETURN
C  
  210 CALL KEY_SWEEP
      RETURN
C  
  220 CALL KEY_VECTV
      RETURN
C  
  230 CALL KEY_VECT
      RETURN
C  
  240 CALL KEY_WEATH
      RETURN   
C      
  250 CALL KEY_RAND
      RETURN
C
      END
      SUBROUTINE CHECK_4_VARS(IERR,VAR_FOUND)
C
C----------------------------------------------------------------------
C
C     This module checks INLINE for variables at the first position.
C     If one is found, a type 3 card is created and written to the
C     output file.
C
C----------------------------------------------------------------------
C
      COMMON / ASSIGNMNT_COMM/ COMMENT
      CHARACTER                COMMENT*80   
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C      
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      CHARACTER TEMP*132, VAR_NAME*12, OUTLINE*132
C
      LOGICAL VAR_FOUND, SAVECOM
C
C---  Set the error flag.
      IERR = 0
C      
C---  Initialize outline.
      OUTLINE = ' '
C
C---  Determine the end position of the first word in WK_STRING.
      I = LAST_CHAR(1,WK_STRING)
      J = INDEX( WK_STRING, '=' )
C
C---  Verify that an '=' was included in the statement.
      IF(J .LT. 1) THEN
        CALL ISSUE_ERR_MSG('INVALID STATEMENT: CHECK SPELLING')           !**
        IERR = 1
        RETURN
      ENDIF
C      
      I = MIN( I, J-1 )
      VAR_NAME = WK_STRING(1:I)
      SAVECOM = .TRUE.
      CALL SEARCH_HEAD
     1   (IERR,VAR_NAME,LOC_IN_C,VAR_FOUND,IDIM,SAVECOM)
      IF( IERR .GT. 0 .OR. .NOT. VAR_FOUND ) RETURN
C
C---  Remove parens and comma if they exists in the var_name so that more
C---  of the characters can be placed in the CADIN.ASC
      CALL CLEAN_VAR( VAR_NAME )
C
C---  Write the type 3 card indicator and the variable name.
      OUTLINE(1:2) = '03'
      OUTLINE(4:11) = VAR_NAME(1:8)
C
C---  Remove the varible name and the = from the working string.
      TEMP = WK_STRING(NXT_CHAR(J+1,WK_STRING):LENSTR(WK_STRING))
      WK_STRING = TEMP
C
C---  Determine the assignment made and the stage the assignment is 
C---  made in.  Place the information in the string OUTLINE.
      CALL CT3_ASSIGN( IERR, OUTLINE )
      IF( IERR .GT. 0 ) RETURN
C
      DO I = 1, IDIM
        IF( IDIM .GT. 1 ) THEN
          IF( IDIM .LT. 10 ) THEN  
            WRITE(OUTLINE(8:8),'(I1)') I
          ELSE
            WRITE(OUTLINE(7:8),'(I2)') I
          ENDIF
        ENDIF
        WRITE(OUTLINE(22:25),'(I4.4)') LOC_IN_C + I - 1
        WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
      ENDDO
C
      CALL WRITE_NEWINPUT_WITH_COMMENT
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
      COMMON /FILE_IDS/   INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      COMMON /FILE_NAMES/ INPUT_NAME, NEWINPUT_NAME, INPUT_BACKUP,
     1                    CADIN_NAME,  CADIN_BACKUP, ERROR_FILE   
      CHARACTER*60        INPUT_NAME, NEWINPUT_NAME, INPUT_BACKUP,
     1                    CADIN_NAME, CADIN_BACKUP, ERROR_FILE
C
      COMMON /ERROR_INFO/ JERR
C
      INTEGER*4 RENAMEFILEQQ, FINDFILEQQ, DELFILESQQ
      CHARACTER STRDUM*80
C          
C---  Close the data files.
      CLOSE( INPUT )
      CLOSE( NEWINPUT )
      CLOSE( ICADIN )
C
C---  See if a file with the backup name currently exists.  If it does,
C---  delete it.
      IFOUND = FINDFILEQQ( INPUT_BACKUP(1:LENSTR(INPUT_BACKUP)), 
     1         'PATH', STRDUM ) 
      IF(IFOUND .GT. 0)
     1  IDEL = DELFILESQQ( INPUT_BACKUP(1:LENSTR(INPUT_BACKUP)) )
C     
C---  Rename the INPUT.ASC file with the definitions from the HEAD.ASC
C---  file to the INPUT.BAK.
      IERROR = RENAMEFILEQQ( INPUT_NAME(1:LENSTR(INPUT_NAME)),
     1                       INPUT_BACKUP(1:LENSTR(INPUT_BACKUP)) ) 
C
C---  Rename the new INPUT file (with the definitions from HEAD.ASC)
C---  to the INPUT.ASC.
      IERROR = RENAMEFILEQQ( NEWINPUT_NAME(1:LENSTR(NEWINPUT_NAME)),
     1                       INPUT_NAME(1:LENSTR(INPUT_NAME)) ) 
C      
      CALL FILE_COPY( CADIN_NAME, CADIN_BACKUP )
C
C---  See if any errors occurred during execution.  If they did, inform
C---  the user.  If they didn't close the file with a delete status.  
      IF( JERR .GT. 0 ) THEN
        CLOSE( JERROR )
        CALL CLEAR_SCREEN 
        IF( JERR .EQ. 1 ) THEN
          WRITE(*,'(/////15X, A, I4, A / 15X, A // 15X, A)' )
     1        '* * * ', JERR, ' ERROR WAS FOUND IN INPUT FILE * * *', 
     2        '   REFER TO ERROR.ASC FILE IN CURRENT DIRECTORY',
     3        '        PRESS <RETURN> TO CONTINUE'
        ELSE
          WRITE(*,'(/////15X, A, I4, A / 15X, A // 15X, A)' )
     1        '* * * ', JERR, ' ERRORS WERE FOUND IN INPUT FILE * * *', 
     2        '   REFER TO ERROR.ASC FILE IN CURRENT DIRECTORY',
     3        '        PRESS <RETURN> TO CONTINUE'
        ENDIF
        READ(*,'(A)') ANS  
      ELSE
        CLOSE( JERROR, STATUS='DELETE' )
      ENDIF  
C
C---  Delete the expanded head.asc file.
      IDEL = DELFILESQQ( 'NUHEAD.ASC' )
C                   
      RETURN
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
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C      
      COMMON / ASSIGNMNT_COMM/ COMMENT
C
      COMMON /PATH_DIR/ FDRIVE,   FDIR
      CHARACTER         FDRIVE*2, FDIR*60, FNAME*8, FEXT*4, PATHDIREC*80
C
      CHARACTER VAR_READ*12, FILENAME*80, TEMP*132
      CHARACTER ASTRING*80, O_ASTRING*80, COMMENT*80
C
      LOGICAL GOODFILE, EOFILE  
      INTEGER SPLITPATHQQ
C
C---  Give a status report for the user.
C
      CALL CLEAR_SCREEN
      WRITE(*,'(/////// 20X, A)' ) 
     1     '* * * Expanding HEAD.ASC file * * *'
C
C---  Open the  Copied HEAD file.
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
     1            FNAME(1:LENSTR(FNAME)) // FEXT
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
      READ(IHEAD,'(A)') O_ASTRING
      DO WHILE( O_ASTRING(1:1) .NE. '*' )
         READ(IHEAD,'(A)') O_ASTRING
      END DO
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
            READ(ASTRING(5:8),'(I4)' ) LOC_IN_C
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
                WRITE(NUHEAD, 240) LOC_IN_C + I-1, TEMP(1:13),
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
              LOC = LOC_IN_C + (IROW*ICOL)
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
                  LOC = LOC - 1
                  WRITE(NUHEAD,240)LOC, TEMP(1:13), COMMENT(1:LEN_COMM)
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
        END IF
C
      END DO  
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
     1 2000,'TIME',    'E  Trajectory time - s' 
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
      SUBROUTINE CLEAN_VAR( VAR_NAME )
C
C----------------------------------------------------------------------
C
C     This module removes parens and commas if they exists in the 
C     var_name so that more of the characters can be placed in the 
C     CADIN.ASC.
C
C----------------------------------------------------------------------
C
      CHARACTER VAR_NAME*(*)
C
      DIMENSION  REM(3)
      CHARACTER  REM*1   
C
      DATA REM / '(', ',', ')'  /
C
      IEND = LENSTR(VAR_NAME)
      DO LOOP = 1, 3
        IREM = INDEX(VAR_NAME,REM(LOOP))
        IF( IREM .GT. 0 ) THEN
           DO J = IREM, IEND
             VAR_NAME(J:J) = VAR_NAME(J+1:J+1)
           ENDDO
        ENDIF
      ENDDO
      RETURN
      END
      SUBROUTINE CT3_ASSIGN(IERR,OUTLINE)
C
C----------------------------------------------------------------------
C
C     This module determines what the variable (or vector elements) is 
C     being assigned to.  It is called by a type 3 card assignment or 
C     by a vector assigment in which the vector command will result
C     in multiple type 3 cards.
C
C----------------------------------------------------------------------
C
      DIMENSION KEYS(7), NUM_PARS(7)
      CHARACTER KEYS*80 
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C      
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      CHARACTER OUTLINE*132
C
      LOGICAL FUNC_FOUND
C
      DATA NUM_KEYS / 7 /
      DATA KEYS / 'RANDOM', 'GAUSS', 'UNIF',        ! have 2 parameters
     1            'EXPO', 'RAYLEI', 'SIGN', 'INT' /  ! have 1 parameter
      DATA NUM_PARS / 2, 2, 2, 1, 1, 1, 1 /
C
C---  Set the error flag.
      IERR = 0
C      
C---  Determine if the variable is being set to a function by searching
C---  for the predefined keywords.
      FUNC_FOUND = .TRUE.
      DO JFUNC = 1, NUM_KEYS
        LEN_FUNC = LENSTR(KEYS(JFUNC))
        IF( INDEX(WK_STRING, KEYS(JFUNC)(1:LEN_FUNC)) .EQ. 1 ) GOTO 200
      ENDDO
      FUNC_FOUND = .FALSE.
C
  200 CONTINUE
C
      IF( FUNC_FOUND ) THEN
C
C---    The assignment is to a function.  Get the parameters of the
C---    and remove them from the working string.
        CALL GET_FUNC_PARS( NUM_PARS(JFUNC), PAR1, PAR2, PAR3, IERR )
        IF(IERR .GT. 0 ) RETURN
C
C---    Place the function name in the output line.
        IF( KEYS(JFUNC) .EQ. 'INT' ) THEN
C---      An integer is denoted by a '1' in column 30 in the type 3 card.
          OUTLINE(30:30) = '1'
        ELSE IF( KEYS(JFUNC)  .NE. 'RANDOM' ) THEN
          OUTLINE(15:20) = KEYS(JFUNC)
        ENDIF
C
        CALL WRITE_INTERNAL2(PAR1, IERR, OUTLINE(31:45) )
CJH        CALL WRITE_INTERNAL(PAR1, IERR, OUTLINE(31:45) )
        IF( KEYS(JFUNC) .EQ. 'GAUSS' .OR. KEYS(JFUNC) .EQ. 'UNIF' .OR. 
     1      KEYS(JFUNC) .EQ. 'RANDOM'   )
     2      CALL WRITE_INTERNAL2(PAR2, IERR, OUTLINE(46:60) )
CJH     2      CALL WRITE_INTERNAL(PAR2, IERR, OUTLINE(46:60) )
C
      ELSE
C
C---    43 +  ; 45 -  ; 46 .  ; 48 - 57
        IASCII = ICHAR( WK_STRING(1:1) )
        IF( IASCII .EQ. 43 .OR.
     1      IASCII .EQ. 45 .OR.
     2      IASCII .EQ. 46 .OR.
     3      ( IASCII .GE. 48 .AND. IASCII .LE. 57 ) ) THEN
C
C---      The assignment is to a value.  Get the value and remove
C---      it from the working string.
          CALL CT3_VALUE( IERR, PAR1 )
          IF( IERR .GT. 0 ) RETURN
C          
          CALL WRITE_INTERNAL2(PAR1, IERR, OUTLINE(31:45) )
CJH          CALL WRITE_INTERNAL(PAR1, IERR, OUTLINE(31:45) )
C
        ELSE
C
C---      The assignment is to a variable.  NOTE: This is actually the 
C---      EQUAL function.  Get the variable name, determine the C
C---      location and remove the variable from the working string.
C---      NOTE: PAR1 = variable's C location.
          OUTLINE(15:20) = 'EQUALS' 
C
          CALL CT3_VARIABLE( IERR, PAR1 )
          IF( IERR .GT. 0 ) RETURN
C          
          CALL WRITE_INTERNAL2(PAR1, IERR, OUTLINE(31:45) )
CJH          CALL WRITE_INTERNAL(PAR1, IERR, OUTLINE(31:45) )
C
        ENDIF
C
      ENDIF
C
C---  Check to see if a stage was included.
      ISTAGE = INDEX( WK_STRING, 'STAGE' )
      IF( ISTAGE .GT. 0 ) THEN
         ISTAGE = ISTAGE + 6
         LEN_WKSTRING = LENSTR(WK_STRING)
         CALL READ_INTERNAL(STAGE,IERR,WK_STRING(ISTAGE:LEN_WKSTRING))
         IF ( IERR .NE. 0 ) THEN
           CALL ISSUE_ERR_MSG( 'ERROR READING STAGE VALUE' )              !**
           RETURN
         ENDIF
         JSTAGE = NINT(STAGE)
         WRITE(OUTLINE(61:62),'(I2.2)') JSTAGE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CT3_VALUE( IERR, PAR1 )
C
C----------------------------------------------------------------------
C
C     This module determines the value for a type 3 assignment.
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      CHARACTER TEMP*132  
C
C---  Set the error flag.
      IERR = 0      
C
      MULT = INDEX(WK_STRING,'*')
      IF( MULT .GT. 0 ) THEN
C
C---    A multiplication factor was entered.  find starting
C---    position of factor end position of factor
        IFACT = NXT_CHAR(MULT+1,WK_STRING)
        IFACT_LAST = LAST_CHAR(IFACT,WK_STRING)
        CALL READ_INTERNAL(FACT, IERR, WK_STRING(IFACT:IFACT_LAST))
        IF(IERR .GT. 0) THEN
          CALL ISSUE_ERR_MSG
     1   ('ERROR READING MULTIPLICATION FACTOR IN ASSIGMENT STATEMENT')  !**
          RETURN
        ENDIF
C
C---    Determine last position of value.
        IEND = MULT - 1
C
      ELSE  
C      
        FACT = 1.0
C---    Determine last position of value.
        IEND = LAST_CHAR( 1, WK_STRING ) 
C       
      ENDIF
C
      CALL READ_INTERNAL( PAR1, IERR, WK_STRING(1:IEND))
C
      IF(IERR .GT. 0) THEN
        CALL ISSUE_ERR_MSG
     1      ('ERROR READING VALUE IN ASSIGNMENT STATEMENT')               !**
        RETURN
      ENDIF
C      
      PAR1 = PAR1 * FACT
C
C---  Set the working string to the string without the parameter data.
      IF ( IEND .NE. LENSTR(WK_STRING)) THEN
         TEMP = WK_STRING(IEND+1:LENSTR(WK_STRING))
      ELSE
         TEMP = ' '
      END IF
      WK_STRING = TEMP
C
      RETURN
      END
      SUBROUTINE CT3_VARIABLE( IERR,PAR1 )
C
C----------------------------------------------------------------------
C
C     This module determines the c location for the variable that the
C     assigment variable is being set to in a type 3 assignment.
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      CHARACTER VAR_NAME*12, TEMP*132
      LOGICAL VAR_FOUND, SAVECOM
C
C---  Set the error flag.
      IERR = 0
C      
C---  Determine the end position of the variable word in WK_STRING.
      IEND = LAST_CHAR(1,WK_STRING)
C
      VAR_NAME = WK_STRING(1:IEND)
C
      SAVECOM = .FALSE.
      CALL SEARCH_HEAD
     1    (IERR,VAR_NAME,LOC_IN_C,VAR_FOUND,IDIM,SAVECOM)
      IF( IERR .GT. 0 ) RETURN     
C
      IF( .NOT. VAR_FOUND .OR. IDIM .GT. 1 ) THEN
        CALL ISSUE_ERR_MSG 
     1   ('INVALID ASSIGMENT STATEMENT: CHECK SPELLING' )                 !**
        IERR = 1 
        RETURN
      ENDIF
C
      PAR1 = FLOAT(LOC_IN_C)
C
C---  Set the working string to the string without the parameter data.
      IF ( IEND .NE. LENSTR(WK_STRING)) THEN
         TEMP = WK_STRING(IEND+1:LENSTR(WK_STRING))
      ELSE
         TEMP = ' '
      END IF
      WK_STRING = TEMP
C
      RETURN
      END
      SUBROUTINE DCODE_LINES
C
C----------------------------------------------------------------------
C
C     This module decodes the lines following the "modules to be executed"
C     section in the input file and places them in correct format for
C     the output file; the CADAC input file, CADIN.ASC.
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      LOGICAL END_OF_FILE, VAR_FOUND, KEY_FOUND, SAVE_TYPE4
C
C---  Set the error flag.
      IERR = 0
 
      SAVE_TYPE4 = .TRUE.
C
  100 CONTINUE
      CALL GET_WK_STRING( END_OF_FILE, SAVE_TYPE4)    ! read the next line.
C
      IF( END_OF_FILE ) RETURN
C
C---  Check for keywords at the beginning of the line.
      KEY_FOUND = .FALSE.
      CALL CHECK_4_KEYS( KEY_FOUND )
C
C---  Check for variable names at the beginning of the line.
      VAR_FOUND = .FALSE.
      IF ( .NOT. KEY_FOUND ) CALL CHECK_4_VARS(IERR,VAR_FOUND )
      IF( IERR .GT. 0 ) GOTO 100
C
      IF( .NOT. KEY_FOUND .AND. .NOT. VAR_FOUND )          
     1    CALL ISSUE_ERR_MSG( 'ERROR IN INPUT: CHECK SPELLING')           !**
C
      GOTO 100
C
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
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C      
      CHARACTER MSG*80
C
      WRITE(*,100) MSG
  100 FORMAT( // ' ', A, // ' * * * PROGRAM TERMINATED * * * ' )
C
      WRITE(JERROR,110) MSG
  110 FORMAT( // ' ', A )
C
      CLOSE( JERROR )
      CLOSE( INPUT )
      CLOSE( ICADIN )
      CLOSE( NEWINPUT, STATUS='DELETE')
      CLOSE( NUHEAD,STATUS='DELETE')
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
C      
      CHARACTER STRING_132*132
C
      IFILE1 = 10
      IFILE2 = 11
C      
C---  Open the original file.
      OPEN(IFILE1, FILE=FILE1(1:LENSTR(FILE1)), STATUS='OLD', ERR=900 )
C
C---  Open the new file with the status of old to see if it exists.  If
C---  it does close it with the delete option.
      OPEN(IFILE2, FILE=FILE2(1:LENSTR(FILE2)), STATUS='OLD',ERR=100 )
      CLOSE(IFILE2,STATUS='DELETE')
C
  100 CONTINUE
      OPEN(IFILE2, FILE=FILE2(1:LENSTR(FILE2)), STATUS='NEW', ERR=910 )      
            
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
C
      END
      SUBROUTINE GET_FUNC_PARS( NPARS, PARAM1, PARAM2, PARAM3, IERR )
C
C----------------------------------------------------------------------
C
C     This module determines the parameters for a type 3 card with
C     a function or a type 11 card. It determines if a factor was 
C     entered and multiplies by the factor if it was.
C
C----------------------------------------------------------------------
C
C     Input:  NPARS
C     Output: PARAM1, PARAM2, PARAM3
C
C----------------------------------------------------------------------
C
      DIMENSION FACT(3), PARAM(3), IEND(3), ISTART(3)
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      CHARACTER TEMP*132, ERR_MSG*80
C
C---  Set the error flag.
      IERR = 0 
C      
      DO I = 1, 3
        PARAM(I) = 0.0
      ENDDO
C
C---  Remove the function from the working string.  Note: a function
C---  must have its parameters contained within parens. Look for the
C---  left and right parens.  If not found give err msg and return.
      ISTART_PAR = INDEX( WK_STRING, '(' )
      IEND_PAR   = INDEX( WK_STRING, ')' ) 
C      
      IF( ISTART_PAR .EQ. 0 .OR. IEND_PAR .EQ. 0 )  THEN
C      
        IF( NPARS .EQ. 1 ) THEN
          ERR_MSG = 
     1        'ERROR READING FUNCTION PARAMETER'
        ELSE IF( NPARS .EQ. 2 ) THEN  
          ERR_MSG =  
     1        'ERROR READING FUNCTION PARAMETERS: FUNCTION REQUIRES 2'
        ELSE IF( NPARS .EQ. 3 ) THEN
          ERR_MSG = 
     1        'ERROR READING FUNCTION PARAMETERS: FUNCTION REQUIRES 3'
        ENDIF
C               
        IERR = 1
        CALL ISSUE_ERR_MSG( ERR_MSG )    
        RETURN                                                            !**
C        
      ENDIF
C
C---  Set temp to just the data between the parens.      
      TEMP = WK_STRING( ISTART_PAR+1: IEND_PAR -1 )
C---  Determine the start and end positions of the function parameter(s).
C---  Note:  ISTART(1) is always 1 and the last postion of the last 
C---  parameter is always the length of the string.
      ISTART(1) = 1
      IEND(NPARS) = LENSTR( TEMP )
C      
      IF( NPARS .GT. 1 ) THEN
c
C---    The function has more that one parameter.  Find the location of 
C---    the first (maybe only) comma.
        KOMMA = INDEX( TEMP, ',' )
        IF( KOMMA .EQ. 0 ) THEN
          IERR = 1   
          IF( NPARS .EQ. 2 ) THEN  
            ERR_MSG =  
     1        'ERROR READING FUNCTION PARAMETERS: FUNCTION REQUIRES 2'
          ELSE IF( NPARS .EQ. 3 ) THEN
            ERR_MSG = 
     1        'ERROR READING FUNCTION PARAMETERS: FUNCTION REQUIRES 3'
          ENDIF
          CALL ISSUE_ERR_MSG( ERR_MSG )    
          RETURN
        ENDIF
C---    Set the location of the parameters in the input line.
        IEND(1)   = KOMMA - 1
        ISTART(2) = KOMMA + 1
        IF( NPARS .GT. 2 ) THEN
          KOMMA = INDEX(TEMP(ISTART(2):LENSTR(TEMP)),',')
          IF( KOMMA .EQ. 0 ) THEN
            IERR = 1
            ERR_MSG = 
     1        'ERROR READING FUNCTION PARAMETERS: FUNCTION REQUIRES 3'
            CALL ISSUE_ERR_MSG( ERR_MSG )    
            RETURN
          ENDIF
          IEND(2)   = ISTART(2) + (KOMMA-1) - 1
          ISTART(3) = ISTART(2) + (KOMMA-1) + 1  
          IEND(3) = LENSTR( TEMP )
        ENDIF
      ENDIF
C
      DO J = 1, NPARS
C
C---    Check for a multiplication factor.
        MULT = INDEX( TEMP(ISTART(J):IEND(J)), '*' )
        IF( MULT .GT. 0 ) THEN
          CALL READ_INTERNAL(FACT(J),IERR, TEMP(ISTART(J)+MULT:IEND(J)))
          IF( IERR .GT. 0 ) THEN
            CALL ISSUE_ERR_MSG('ERROR READING MULTIPLICATION FACTOR')     !**
            RETURN
          ENDIF
C---      Remove the factor from the string.
          DO KLEAR = ISTART(J)+MULT-1, IEND(J)
            TEMP(KLEAR:KLEAR) = ' '
          ENDDO
        ELSE
          FACT(J) = 1.0
        ENDIF
        CALL READ_INTERNAL( PARAM(J), IERR, TEMP(ISTART(J):IEND(J)) )
        PARAM(J) = PARAM(J) * FACT(J)
      ENDDO
C
      IF( LENSTR(WK_STRING) .GT. IEND_PAR+1 ) THEN
        TEMP = 
     1      WK_STRING(NXT_CHAR(IEND_PAR+1,WK_STRING):LENSTR(WK_STRING))
C---    Check to see if a factor is included outside the parens.
        IF( TEMP(1:1) .EQ. '*' ) THEN
C---      Find the location of the beginning and the end of the factor.
          IFIRST = NXT_CHAR(2,TEMP)          ! find first position of number
          ILAST = LAST_CHAR(IFIRST, TEMP )   ! find last position of number
          CALL READ_INTERNAL( FACT(1), IERR, TEMP(IFIRST:ILAST) )
          DO I = 1, NPARS
            PARAM(I) = PARAM(I) * FACT(1)
          ENDDO
          TEMP = WK_STRING(ILAST+1:LENSTR(WK_STRING))
        ENDIF
        WK_STRING = TEMP     ! Set working string to string without parameters data.     
      ELSE
        WK_STRING =  ' '
      ENDIF
C
      PARAM1 = PARAM(1)
      PARAM2 = PARAM(2)
      PARAM3 = PARAM(3)
C
      RETURN
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
      CHARACTER  FDRIVE*2, FDIR*60, FNAME*8, FEXT*4
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
    5 GOOD_FILE = .FALSE.    
      DO WHILE ( .NOT. GOOD_FILE )
C
	  LENDIR = GETDRIVEDIRQQ( DIRPATH )  
C
CC        WRITE(*,'(////5X,A/5X,A//5X,A/5X,A//5X,A,/5X,A,A,A\)') 
CC     1  'This Program Inputs a Free-Formated File',
CC     2  'and Outputs a Fixed-Formed File Named CADIN.ASC.',
CC     3      'Current Directory is :', DIRPATH, 
CC     4      FILE_MSG(1:LENSTR(FILE_MSG)),
CC     5      'Default = ', FILE_NAME(1:LENSTR(FILE_NAME)), ' : '
C     
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
     1        '*** WARNING ***',
     2        'OUTPUT FILE ALREADY EXISTS - DO YOU WISH TO CHOOSE',
     3        'A DIFFERNET NAME (Y or N)',
     4        'Default = ', ANSW, ':'
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
        IF( FILETYPE .EQ. 0 ) GOTO 30
C
      IF( .NOT. GOOD_FILE .AND. FILETYPE .EQ. OUTPUTFILE ) THEN
C
C---      If the file is an output file then it is possible it doesn't 
C---      exist, which would create an error with the previous open 
C---      statement.  Therefore, try to open the file with a new status.
C---      If an error still occurs, then it is a bad file name.
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
	  END IF
C
      ENDDO   
C      
      RETURN
      END
      SUBROUTINE GET_FUNC_VARS( NUM_VARS, LOC1, LOC2, IERR )
C
C----------------------------------------------------------------------
C
C     This module determines the variables for a type 11 card.   
C
C----------------------------------------------------------------------
C
C     Input: NUM_VARS
C     Output: LOC1, LOC2
C
C----------------------------------------------------------------------
C
      DIMENSION LOC(2), IEND(2), ISTART(2)
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      CHARACTER TEMP*132, VAR_NAME*12
      LOGICAL VAR_FOUND, SAVECOM
C 
C---  Set the error flag.
      IERR = 0
C      
      DO I = 1, 2
        LOC(I) = 0
      ENDDO
C
C---  Remove the function from the working string.  Note: a function
C---  must have its parameters contained within parens. Look for the
C---  left and right parens.  If not found give err msg and exit.
      ISTART_PAR = INDEX( WK_STRING, '(' )
      IEND_PAR   = INDEX( WK_STRING, ')' )
      IF( ISTART_PAR .EQ. 0 .OR. IEND_PAR .EQ. 0 )  THEN
        CALL ISSUE_ERR_MSG( 'ERROR READING FUNCTION VARIABLES')           !**
        IERR = 1
        RETURN
      ENDIF
C
C---  Set temp to just the data between the parens.      
      TEMP = WK_STRING( NXT_CHAR(ISTART_PAR+1,WK_STRING): IEND_PAR -1 )
C
C---  Determine the start and end positions of the function variable(s).
C---  Note:  ISTART(1) is always 1 and the last postion of the last 
C---  character of the last variable is always the length of the string.
      ISTART(1) = 1
      IEND(NUM_VARS) = LENSTR( TEMP )
C      
      IF( NUM_VARS .GT. 1 ) THEN
c
C---    The function has more that one variable.  Find the location of 
C---    the comma.
        KOMMA = INDEX( TEMP, ',' )
        IF( KOMMA .EQ. 0 ) THEN 
          CALL ISSUE_ERR_MSG( 'ERROR IN FUNCTION STATEMENT VARIABLES')    !**
          IERR = 1
          RETURN
        ENDIF
C---    Set the location of the parameters in the input line.
        IEND(1)   = LAST_CHAR( ISTART(1), TEMP(1:KOMMA-1) )
        ISTART(2) = NXT_CHAR(KOMMA+1,TEMP)
      ENDIF
C
      SAVECOM = .FALSE.
      DO J = 1, NUM_VARS                                   
        VAR_NAME = TEMP(ISTART(J):IEND(J))
        CALL SEARCH_HEAD
     1      (IERR,VAR_NAME, LOC(J),VAR_FOUND,IDIM,SAVECOM)
       IF( IERR .GT. 0 ) RETURN
        IF( .NOT. VAR_FOUND .OR. IDIM .GT. 1 ) THEN
          CALL ISSUE_ERR_MSG ('INVALID VARIABLE NAME')                 !**\
          RETURN
        ENDIF
      ENDDO
C
      LOC1 = LOC(1)
      LOC2 = LOC(2)
C
      RETURN
      END
      SUBROUTINE GET_TITLE
C
C----------------------------------------------------------------------
C
C     This module reads the title from the input file and places it
C     on the output file CADIN.ASC.
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      CHARACTER OUTLINE*132
C
      LOGICAL END_OF_FILE, SAVE_TYPE4 
C
      SAVE_TYPE4 = .FALSE.
C
C---  Initialize outline.
      OUTLINE = '                                                      '
C
      CALL GET_WK_STRING( END_OF_FILE, SAVE_TYPE4 )
C
      IF( END_OF_FILE ) CALL END_MSG_STOP
     1    ( 'PREMATURE END OF INPUT FILE' )
C
      IF ( WK_STRING(1:5) .EQ. 'TITLE' ) THEN
C
C---    TITLE keyword entered.
        IF ( LENSTR(WK_STRING) .GT. 5 ) THEN
C---      A title was entered after the TITLE keyword.  Copy the title
C---      as is, omitting the keyword, TITLE.
          OUTLINE = INLINE(NXT_CHAR(6,INLINE):LENSTR(INLINE))
        ELSE
          OUTLINE = ' '
        ENDIF
      ELSE
        OUTLINE = INLINE(1:LENSTR(INLINE))
      ENDIF
C
      WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
C
      RETURN
      END
      SUBROUTINE GET_WK_STRING( END_OF_FILE, SAVE_TYPE4 )
C
C----------------------------------------------------------------------
C
C   This module determines the string to decode, removing and leading
C   blanks and  writing any comments to the output file as type 4 cards
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      LOGICAL END_OF_FILE, SAVE_TYPE4, FLAG13
C
      END_OF_FILE = .FALSE.
C
  100 READ(INPUT,'(A)', END=900 ) INLINE 
C
      IF( LENSTR(INLINE) .LT. 1 ) THEN
        WRITE(NEWINPUT,'(A)') ' '            ! put a blank line in NEWINPUT.
        GOTO 100
      ELSE
        WRITE(NEWINPUT,'(A)') INLINE(1:LENSTR(INLINE)) ! Copy line to NEWINPUT.
      ENDIF
C
      IF( INLINE(1:1) .EQ. '!' ) THEN
C---    The line read in was a comment, put it in the output file as type 4.
        IF( SAVE_TYPE4 ) CALL WRITE_COMMENT( INLINE )
        GOTO 100
      ENDIF
C
C---  Clear out the working string.
      WK_STRING = ' '
C
C---  Check for comments at the end of the line.  Don't include them in
C---  the working string.
      ICOMMENT = INDEX( INLINE, '!' )
      IF( ICOMMENT .GT. 0 ) THEN
        ILAST = LENSTR(INLINE(1:ICOMMENT-1))
      ELSE
        ILAST = LENSTR( INLINE )
      ENDIF
      IFIRST = NXT_CHAR(1,INLINE)
      WK_STRING(1:ILAST-IFIRST+1) = INLINE(IFIRST:ILAST)
C
C---  Inform the user of the status of the program.
CJH      PRINT*, CHAR(27) // '[2J'
CJH      WRITE(*,'(// 5X, A / 5X, A)' ) 
CJH     1  'Now processing line:',  
CJH     2   WK_STRING(1:LENSTR(WK_STRING))
C
      CALL STR_UPCASE( WK_STRING, WK_STRING )
C
C---  Determine if the input line is a type 13 card, ie a stop command.
C---  If it is, set FLAG13 to true so that when we reach the end of file 
C---  for INPUT.ASC we will know that the last valid command was STOP and
C---  that we do not need to put a 13 card at the end of the CADIN.ASC file.
      FLAG13 = ( INDEX( WK_STRING, 'STOP' ) .GT. 0 )
C
      RETURN
C
  900 END_OF_FILE = .TRUE.
C
      IF( .NOT. FLAG13 ) WRITE(ICADIN,910)
  910 FORMAT( '13' )
      RETURN
C
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
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C      
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      COMMON /ERROR_INFO/JERR
C      
      CHARACTER*(*) MSG
C
      DATA JERR /0/
C
C---  Increment the number of errors.
      JERR = JERR + 1
C            
      WRITE(*,110) MSG, INLINE
C  100 FORMAT( // ' ', A, // ' ', A )
C
      WRITE(JERROR,110) MSG, INLINE
  110 FORMAT( ' ', '* * * ', A / 5X, ':', A / )
C
      END
      SUBROUTINE KEY_CLEAR
C
C----------------------------------------------------------------------
C
C     This module places a card (type 11) in the CADIN.ASC file to tell
C     cadac to clear all the functions defined. 
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      WRITE(ICADIN,100)
  100 FORMAT( '11', T9, 'END' )
C
      RETURN
      END
      SUBROUTINE KEY_FUNC
C
C----------------------------------------------------------------------
C
C     This module decodes functions statements, determining the variable
C     the variable to be assigned the function and the function.
C
C----------------------------------------------------------------------
C
      DIMENSION FUNCS(16)
      CHARACTER FUNCS*5
C
      COMMON / ASSIGNMNT_COMM/ COMMENT
      CHARACTER                COMMENT*80
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C      
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      CHARACTER TEMP*132, VAR_NAME*12, OUTLINE*132
      LOGICAL VAR_FOUND, FUNC_FOUND, SAVECOM
C
C---  Note there are only 16 functions, but MARKOV and GAUSS are used 
C---  interchangeably, so they are both included.  Since the END function
C---  is coded separately, the MARKOV command is placed in the array
C---  as the 14 element.
C
      DATA NUM_FUNCS /16/
      DATA FUNCS / 'STEP', 'RAMP', 'PARAB', 'SIN', 'COS', 
     1             'TRI', 'GAUSS', 'UNIF', 'DECAY', 'SQR',
     2             'SUM', 'PROD', 'DIFF', 'MARKO', 'RAYLE', 
     3             'EQUAL' /
C         
C---  Initialize outline.
      OUTLINE = ' '
C
C---  Remove the keyword FUNC from the working string.
      IEND = LAST_CHAR(1,WK_STRING)
      TEMP = WK_STRING(NXT_CHAR(IEND+1,WK_STRING):LENSTR(WK_STRING))
      WK_STRING = TEMP
C
C---  Determine the end position of the first word in WK_STRING.  It
C---  is signaled by a blank space or a =,+,-, or * character.
      I = LAST_CHAR(1,WK_STRING)
      ISIGN = 99
      J = INDEX(WK_STRING,'=' )
      IF( J .NE. 0 ) ISIGN = MIN( J, ISIGN )
      J = INDEX(WK_STRING,'+' )
      IF( J .NE. 0 ) ISIGN = MIN( J, ISIGN )
      J = INDEX(WK_STRING,'-' )
      IF( J .NE. 0 ) ISIGN = MIN( J, ISIGN )
      J = INDEX(WK_STRING,'*' )
      IF( J .NE. 0 ) ISIGN = MIN( J, ISIGN )
C
C---  Verify that an '=', '+', '-' or '*' was included in the statement.
      IF( ISIGN .GE. 99 ) THEN 
        CALL ISSUE_ERR_MSG                                                !**
     1    ('INVALID FUNCTION STATEMENT: INCLUDE A =, +, - OR *')
        RETURN
      ENDIF
C
      OUTLINE(9:9) = WK_STRING(ISIGN:ISIGN)
C      
      I = MIN( I, ISIGN-1 )
      VAR_NAME = WK_STRING(1:I)
      SAVECOM = .TRUE.
      CALL SEARCH_HEAD
     1   (IERR,VAR_NAME,LOC_IN_C,VAR_FOUND,IDIM,SAVECOM)
      IF( IERR .GT. 0 ) RETURN
C
      IF( .NOT. VAR_FOUND ) THEN
        CALL ISSUE_ERR_MSG                                                !**
     1  ('INVALID FUNCTION STATEMENT: CHECK VARIABLE SPELLING')
        RETURN
      ENDIF
C
C---  Remove parens and comma if they exists in the var_name so that
C---  more of the characters can be placed in the CADIN.ASC.
      CALL CLEAN_VAR( VAR_NAME )

C---  Save the type 11 card indicator and the variable name.
      OUTLINE(1:2) = '11'
      OUTLINE(4:8) = VAR_NAME(1:5)
C
C---  Remove the varible name and math sign from the working string.
      TEMP = WK_STRING(NXT_CHAR(ISIGN+1,WK_STRING):LENSTR(WK_STRING))
      WK_STRING = TEMP
C
C---  Determine the function by searching for the predefined keywords.
      FUNC_FOUND = .TRUE.
      DO JFUNC = 1, NUM_FUNCS
        LEN_FUNC = LENSTR(FUNCS(JFUNC))
        IF(INDEX(WK_STRING,FUNCS(JFUNC)(1:LEN_FUNC)) .GT. 0)GOTO 200
      ENDDO
      FUNC_FOUND = .FALSE.
C
  200 CONTINUE
C
      IF( .NOT. FUNC_FOUND ) THEN
        CALL ISSUE_ERR_MSG                                                !**
     1    ('INVALID FUNCTION STATEMENT: CHECK SPELLING OF FUNCTION')
        RETURN
      ENDIF
C
      OUTLINE(10:14) = FUNCS(JFUNC)(1:LEN_FUNC)
C
C---  If MARKOV is the function, set it to the GAUSS function number.      
      IF( JFUNC .EQ. 14 ) JFUNC = 7
C      
C---  Determine if the stage is being set.
      ISTAGE = INDEX(WK_STRING, 'STAGE' )
      IF( ISTAGE .GT. 0 ) THEN
        IFIRST = ISTAGE + 6
        LEN_WKSTRING = LENSTR(WK_STRING)
        CALL READ_INTERNAL(STAGE, IERR, WK_STRING(IFIRST:LEN_WKSTRING))
        IF( IERR .GT. 0 ) THEN
          CALL ISSUE_ERR_MSG                                              !**
     1     ('INVALID STAGE ASSIGNMENT IN FUNCTION STATEMENT' )  
          RETURN
        ENDIF
        JSTAGE = NINT(STAGE)
        WRITE(OUTLINE(61:62),'(I2.2)') JSTAGE
C---    Remove the stage data from the working string.
        WK_STRING(ISTAGE:LEN_WKSTRING) = ' '
      ENDIF
C
C---  Determine the parameters for the functions.
C
      IF( JFUNC .EQ. 1 )THEN              ! STEP function.
C
        PARAM1 = 0.0
C
C---    Determine if the function contains a value.  It is possible 
C---    that it doesn't.
        ILEFT = INDEX( WK_STRING,'(' )
        IRITE = INDEX( WK_STRING,')' )
        IF( ( ILEFT .GT. 0 .AND. IRITE .EQ. 0 ) .OR.
     1      ( ILEFT .EQ. 0 .AND. IRITE .GT. 0 ) .OR.
     2      ( ILEFT .GT. IRITE ) ) THEN
          CALL ISSUE_ERR_MSG('INVALID FUNCTION STATEMENT: CHECK PARENS')  !**
          RETURN                                  
        ELSE IF( ILEFT .GT. 0 .AND. IRITE .GT. 0 ) THEN
C---      See if there is a value between the parens.
          IF( (IRITE - ILEFT) .GT. 1 ) THEN
C---        There is at least a space between the parens.
            IF( LENSTR(WK_STRING(ILEFT+1:IRITE-1)) .GT. 0 ) THEN 
              CALL GET_FUNC_PARS( 1, PARAM1, PARAM2, PARAM3, IERR )       !**
              IF( IERR .GT. 0 ) RETURN
            ENDIF
          ENDIF
        ENDIF
        CALL WRITE_INTERNAL2(PARAM1, IERR, OUTLINE(31:45) )
CJH        CALL WRITE_INTERNAL(PARAM1, IERR, OUTLINE(31:45) )
C
      ELSE IF( JFUNC .EQ. 2 .OR. JFUNC .EQ. 3 .OR. JFUNC .EQ. 4 .OR.   
     1         JFUNC .EQ. 5 .OR. JFUNC .EQ. 6 .OR. JFUNC .EQ. 7 .OR. 
     2         JFUNC .EQ. 8 .OR. JFUNC .EQ. 9 .OR. JFUNC .EQ. 10 ) THEN
C
C---    2 = RAMP, 3 = PARAB, 4 = SIN, 5 = COS, 6 = TRI, 7 = GAUSS/MARKOV
C---    8 = UNIF, 9 = DECAY, 10 = SQR
C---    All these functions contain 2 value parameters.
C     
        CALL GET_FUNC_PARS( 2, PARAM1, PARAM2, PARAM3, IERR )
        IF( IERR .GT. 0 ) RETURN
C
        IF( JFUNC .EQ. 2 .OR. JFUNC .EQ. 3 .OR. JFUNC .EQ. 8 ) THEN
C---      PARAM1 is V2 and PARAM2 is V1.          
          CALL WRITE_INTERNAL2( PARAM2, IERR, OUTLINE(31:45) )
CJH          CALL WRITE_INTERNAL( PARAM2, IERR, OUTLINE(31:45) )
          CALL WRITE_INTERNAL2( PARAM1, IERR, OUTLINE(46:60) )
CJH          CALL WRITE_INTERNAL( PARAM1, IERR, OUTLINE(46:60) )
        ELSE
C---      PARAM1 is V1 and PARAM2 is V2.          
          CALL WRITE_INTERNAL2( PARAM1, IERR, OUTLINE(31:45) )
CJH          CALL WRITE_INTERNAL( PARAM1, IERR, OUTLINE(31:45) )
          CALL WRITE_INTERNAL2( PARAM2, IERR, OUTLINE(46:60) )
CJH          CALL WRITE_INTERNAL( PARAM2, IERR, OUTLINE(46:60) )
        ENDIF
C
      ELSE IF( JFUNC .EQ. 15 ) THEN
C---    RAYLE function has three parameters.
        CALL GET_FUNC_PARS( 3, PARAM1, PARAM2, PARAM3, IERR )
        IF( IERR .GT. 0 ) THEN
          CALL ISSUE_ERR_MSG('INVALID FUNCTION STATEMENT: CHECK PARENS')  !**
          RETURN
        ENDIF
        CALL WRITE_INTERNAL2( PARAM1, IERR, OUTLINE(31:45))
CJH        CALL WRITE_INTERNAL( PARAM1, IERR, OUTLINE(31:45))
        CALL WRITE_INTERNAL2( PARAM2, IERR, OUTLINE(46:60))
CJH        CALL WRITE_INTERNAL( PARAM2, IERR, OUTLINE(46:60))
        WRITE(OUTLINE(26:30),'(I5.5)') NINT(PARAM3)
      ELSE IF( JFUNC .EQ. 16 ) THEN
C---    The EQUALS function has one variable for its parameter.
        CALL GET_FUNC_VARS(1, LOC1, LOC2, IERR )
        IF( IERR .GT. 0 ) THEN
          CALL ISSUE_ERR_MSG('INVALID FUNCTION STATEMENT: CHECK PARENS')  !**
          RETURN
        ENDIF
        CALL WRITE_INTERNAL2(FLOAT(LOC1),IERR,OUTLINE(31:45))
CJH        CALL WRITE_INTERNAL(FLOAT(LOC1),IERR,OUTLINE(31:45))
      ELSE
C---    The SUM, PROD & DIFF have two variables for their parameters.
        CALL GET_FUNC_VARS( 2, LOC1, LOC2, IERR )  
        IF( IERR .GT. 0 ) THEN
          CALL ISSUE_ERR_MSG('INVALID FUNCTION STATEMENT: CHECK PARENS')  !**
          RETURN
        ENDIF
        CALL WRITE_INTERNAL2(FLOAT(LOC1),IERR,OUTLINE(31:45))
CJH        CALL WRITE_INTERNAL(FLOAT(LOC1),IERR,OUTLINE(31:45))
        CALL WRITE_INTERNAL2(FLOAT(LOC2),IERR,OUTLINE(46:60))
CJH        CALL WRITE_INTERNAL(FLOAT(LOC2),IERR,OUTLINE(46:60))
      ENDIF
C
      DO I = 1, IDIM
        IF( IDIM .GT. 1 ) THEN
          IF( IDIM .LT. 10 ) THEN  
            WRITE(OUTLINE(8:8),'(I1)') I
          ELSE
            WRITE(OUTLINE(7:8),'(I2)') I
          ENDIF
        ENDIF
        WRITE(OUTLINE(22:25),'(I4.4)') LOC_IN_C + I - 1
        WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
      ENDDO
C
      CALL WRITE_NEWINPUT_WITH_COMMENT
C
      RETURN
      END
      SUBROUTINE KEY_HEADER
C
C----------------------------------------------------------------------
C
C     This module places the comments contained between the keyword HEAD
C     and end in the CADIN.ASC as a type 9 card. 
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      DIMENSION HEADER(5)
      CHARACTER HEADER*80
C
      LOGICAL EOHS, END_OF_FILE, SAVE_TYPE4
C
      NUM_HEADERS = 0
      EOHS = .FALSE.
      SAVE_TYPE4 = .FALSE.
      IFIRST = NXT_CHAR( 1, WK_STRING )
      DO WHILE( .NOT. EOHS )
        CALL GET_WK_STRING( END_OF_FILE, SAVE_TYPE4 )
        IF( END_OF_FILE) CALL END_MSG_STOP
     1        ('END OF FILE REACHED DURING HEADER BLOCK')  
C
        IF( WK_STRING(IFIRST:IFIRST+2) .NE. 'END'  ) THEN
          NUM_HEADERS = NUM_HEADERS + 1
          IF( NUM_HEADERS .GT. 5 ) THEN 
            CALL ISSUE_ERR_MSG ('TOO MANY TEXT LINES IN HEADER GROUP' )  !**
            RETURN
          ENDIF
          HEADER(NUM_HEADERS) = INLINE
        ELSE
          IF( NUM_HEADERS .LT. 1 ) THEN
            CALL ISSUE_ERR_MSG( 'NO TEXT RECORDS INCLUDED WITH HEADER' )  !**
            RETURN
          ENDIF
          EOHS = .TRUE.
        ENDIF
      ENDDO
C
C---  Write the type 9 card to the file005 file and the text records.
      WRITE(ICADIN,200) NUM_HEADERS
  200 FORMAT( '09', T25, I1 )
      DO I = 1, NUM_HEADERS
        ILEN = LENSTR(HEADER(I))
        IF( ILEN .LT. 1 ) ILEN = 1
        WRITE(ICADIN,'(A)') HEADER(I)(1:ILEN)
      ENDDO
C
      RETURN
      END
      SUBROUTINE KEY_IF
C
C----------------------------------------------------------------------
C
C     This module translates the IF statement into the format that
C     the CADIN.ASC requires.
C
C----------------------------------------------------------------------
C
      DIMENSION IEND(2), IFIRST(2)
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      CHARACTER LINE1*132, OUTLINE*132, TEMP*132
      CHARACTER CH_VAL*15, UNITS*10, VAR_NAME*12
      LOGICAL VAR_FOUND, SAVECOM
C      
C---  Initialize the output line.
      OUTLINE = ' '
C---  Determine if the IF statment is an or.
      IFIRST(1) = NXT_CHAR(3,WK_STRING)   ! The first non-blank after the IF
      IEND(1) = INDEX(WK_STRING,' OR ')
      IF( IEND(1) .GT. 0 ) THEN
C---    It is an OR.  The string is going to be broken into two seperate
C---    lines, since both part follow the same format.  The end of the
C---    first part of the statement is where the OR is and the end of the
C---    second part of the statement is the end of the string.
        NUM_CRITERIA = 2
        IEND(2) = LENSTR(WK_STRING)
        IFIRST(2) = NXT_CHAR(IEND(1)+3,WK_STRING) ! frst non-blank after OR
      ELSE
        NUM_CRITERIA = 1
        IEND(1) = LENSTR(WK_STRING)
      ENDIF
C
C---  Create and place the type 10 card in the CADIN.ASC file.
      OUTLINE(1:2) = '10'
      WRITE(OUTLINE(25:25),'(I1)') NUM_CRITERIA
      WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
C
      DO J = 1, NUM_CRITERIA
C
C---    Clear the output line.
        OUTLINE = '  '
C
        LINE1 = WK_STRING(IFIRST(J):IEND(J))
C
C---    Determine the name of the variable that is to satisfy the condition.
C---    First, determine the position of the < or > so the position of the 
C---    last character of the variable name can be found.
        ISIGN = INDEX(LINE1,'<')
        IF( ISIGN .LE. 1 ) ISIGN = INDEX(LINE1,'>')
        IF( ISIGN .LE. 1 ) THEN
          CALL ISSUE_ERR_MSG                                              !**
     1   ('INVALID IF STATEMENT: NO CONDITION SIGN (< or >) INCLUDED ')
          RETURN
        ENDIF
        I = LENSTR(LINE1(1:ISIGN-1))
C
        VAR_NAME = LINE1(1:I)
C
C---    Determine if the var_name is included in the HEAD.ASC
C---    and find its c location.
CJH        SAVECOM = .FALSE.
        SAVECOM = .TRUE.
        CALL SEARCH_HEAD
     1       (IERR,VAR_NAME,LOC_IN_C,VAR_FOUND,IDIM,SAVECOM)
        IF( IERR .GT. 0 ) RETURN
C
C---  Place the comment from the HEAD.ASC in the new INPUT.ASC file.
        CALL WRITE_NEWINPUT_WITH_COMMENT
C
        IF( .NOT. VAR_FOUND .OR. IDIM .GT. 1 ) THEN
          CALL ISSUE_ERR_MSG                                              !**
     1  ('INVALID VARIABLE '//VAR_NAME(1:LENSTR(VAR_NAME))//' IN LINE:')   
          RETURN
        ENDIF
C
C---    Place the variable name and its C location in the output line.
        CALL CLEAN_VAR( VAR_NAME )
        OUTLINE(1:6) = VAR_NAME(1:6)
        WRITE(OUTLINE(8:11),'(I4.4)') LOC_IN_C
C
C---    Remove the variable name (and any blanks following the variable
C---    name) from the working string and determine if < or > was entered.
        TEMP = LINE1(ISIGN:LENSTR(LINE1))
        LINE1 = TEMP(1:LENSTR(TEMP))
        IF( LINE1(1:1) .EQ. '>' ) THEN
          OUTLINE(16:16) = '1'
        ELSEIF ( LINE1(1:1) .EQ. '<' ) THEN
          OUTLINE(16:16) = '0'
        ENDIF
C
C---    Remove the < or > sign and determine if the variable is to be
C---    compared to a value or another variable. If a value, then the next
C---    non-blank character should be a numeric character.
        TEMP = LINE1(NXT_CHAR(2,LINE1):LENSTR(LINE1))
        LINE1 = TEMP(1:LENSTR(TEMP))
        ICODE = ICHAR(LINE1(1:1))
        IF( (ICODE .GE. 48 .AND. ICODE .LE. 57) .OR.     ! 0-9, +, -, .
     2       ICODE .EQ. 43 .OR.  ICODE .EQ. 45 .OR. ICODE .EQ. 46 ) THEN
C
C---      A numeric character was entered.  Find the end of the value. Make
C---      sure that statement isnt: 2+var_name with out spaces around "+"
C---      OR THAT IT ISN'T AN EXPONENT (EX: 0.9012E+05 OR E-05)
          I = LAST_CHAR(1,LINE1)
	    ISIGN = INDEX(LINE1,'E+')
	    IF( ISIGN .LE. 0 ) THEN
	        ISIGN = INDEX(LINE1,'E-')
	    ENDIF
	    IF( ISIGN .LE. 0 ) THEN
              ISIGN = INDEX(LINE1,'+')   
              IF( ISIGN .GT. 1 ) I = MIN(ISIGN-1,I)
	    ENDIF
C         
          CH_VAL = LINE1(1:I)
C---      Check to see if a valid value is entered by calling READ_INTERNAL
          CALL READ_INTERNAL( TEST_VAL, IERR, CH_VAL )
C
          IF(IERR .GT. 0)THEN
            CALL ISSUE_ERR_MSG                                            !**
     1       ('ERROR READING CONDITION VALUE IN IF STATEMENT')
            RETURN
          ENDIF
C
          CALL WRITE_INTERNAL2(TEST_VAL,IERR,OUTLINE(24:38) )
C
C---      Check to see if the criteria test value is the sum of a value and
C---      a C location or if units were entered.
          IF( I .LT. LENSTR(LINE1) ) THEN
C
C---        There is more in the working string.  Either a sum or units.
C---        Remove the value and determine whats left.
            TEMP = LINE1(NXT_CHAR(I+1,LINE1):LENSTR(LINE1))
            LINE1 = TEMP(1:LENSTR(TEMP))
            IF( LINE1(1:1) .EQ. '+' ) THEN         ! a sum was entered.
C
C---          Set KODE in type 10 card.
              OUTLINE(49:50) = '-1'
C
              TEMP = LINE1(NXT_CHAR(2,LINE1):LENSTR(LINE1))
              LINE1 = TEMP(1:LENSTR(TEMP))
C
              I = LAST_CHAR(1,LINE1)
              VAR_NAME = LINE1(1:I)
C
              SAVECOM = .FALSE.
              CALL SEARCH_HEAD
     1            (IERR,VAR_NAME,LOC2_IN_C,VAR_FOUND,IDIM,SAVECOM)
               IF( IERR .GT. 0 ) RETURN
C
C---          Verify that the second variable name is the same as the first.
              IF( .NOT. VAR_FOUND .OR. LOC2_IN_C .NE. LOC_IN_C .OR.
     1            IDIM .GT. 1 ) THEN
                CALL ISSUE_ERR_MSG                                        !**
     2            ('SECOND VARIABLE, '//VAR_NAME(1:LENSTR(VAR_NAME))//
     3             ' IN STAGE CRITERIA, DOES NOT MATCH FIRST IN LINE:')
                RETURN
              ENDIF
C
              IF( I .LT. LENSTR(LINE1) ) THEN           ! units were included
                UNITS = LINE1( NXT_CHAR(I+1,LINE1):LENSTR(LINE1) )
                OUTLINE(40:45) = UNITS(1:6)
              ENDIF
C
            ELSE                                      
C
C---          Set KODE in type 10 card.
              OUTLINE(49:50) = ' 0'
              UNITS = LINE1( NXT_CHAR(1,LINE1):LENSTR(LINE1) )
              OUTLINE(40:45) = UNITS(1:6)
            ENDIF
          ENDIF
C
        ELSE
C
C---      A variable name was entered.  Find the position of the last
C---      character in the variable name.
          I = LAST_CHAR(1,LINE1)
          ISIGN = INDEX( LINE1,'+')
          IF( ISIGN .LT. 1 ) ISIGN = INDEX(LINE1,'-')
          IF( ISIGN .GT. 0 ) I = MIN( ISIGN-1, I )
C
          VAR_NAME = LINE1(1:I)
C
C---      Check to see if the criteria is the sum/difference between the
C---      second variable and a value. If it is, the second name must be 
C---      the same as the first variable name.  
C
          IF( ISIGN .GT. 0 ) THEN
C
            SAVECOM = .FALSE.
            CALL SEARCH_HEAD
     1          (IERR,VAR_NAME,LOC2_IN_C,VAR_FOUND,IDIM,SAVECOM)
            IF( IERR .GT. 0 ) RETURN
C
C---        Verify that the second variable name is the same as the first.
            IF( .NOT. VAR_FOUND .OR. LOC2_IN_C .NE. LOC_IN_C .OR.
     1          IDIM .GT. 1 ) THEN
              CALL ISSUE_ERR_MSG                                          !**
     2          ('SECOND VARIABLE, '//VAR_NAME(1:LENSTR(VAR_NAME))//
     3           ' IN STAGE CRITERIA, DOES NOT MATCH FIRST IN LINE:') 
              RETURN
            ENDIF
C
C---        The test value is the sum/difference of the variable and a value.
            OUTLINE(49:50) = '-1'
C
C---        Remove the variable from wk_string.
            TEMP = LINE1(ISIGN:LENSTR(LINE1))
            LINE1 = TEMP(1:LENSTR(TEMP))
C
            IF( LINE1(1:1) .EQ. '+' ) THEN
              IMULT = 1.0
            ELSEIF( LINE1(1:1) .EQ. '-' ) THEN
              IMULT = -1.0
            ENDIF

C---        Determine the value to be added to the variable.
            TEMP = LINE1(NXT_CHAR(2,LINE1):LENSTR(LINE1))
            LINE1 = TEMP(1:LENSTR(TEMP))
            I = LAST_CHAR(1, LINE1)
C
            CH_VAL = LINE1(1:I)
C---        Check to see if a valid value is entered by calling READ_INTERNAL.
            CALL READ_INTERNAL( TEST_VAL, IERR, CH_VAL )
C
            IF( IERR .NE. 0 ) THEN
              CALL ISSUE_ERR_MSG( 'ERROR READING VALUE IN LINE: ' )       !**
              RETURN
            ENDIF
C
            CALL WRITE_INTERNAL2( TEST_VAL*IMULT,IERR,OUTLINE(24:38) )
            IF( I .LT. LENSTR(LINE1) ) THEN
              UNITS = LINE1( NXT_CHAR(I+1,LINE1):LENSTR(LINE1) )
              OUTLINE(40:45) = UNITS(1:6)
            ENDIF
C
          ELSE 
C
C---        The test value is another variable.
            OUTLINE(49:50) = ' 1'
C
C---        Determine if the var_name is included in the HEAD.ASC
C---        and find its c location.
            SAVECOM = .FALSE.
            CALL SEARCH_HEAD
     1       (IERR,VAR_NAME,LOC_IN_C,VAR_FOUND,IDIM,SAVECOM)
            IF( IERR .GT. 0 ) RETURN
C            
            IF( .NOT. VAR_FOUND .OR. IDIM .GT. 1 ) THEN
              CALL ISSUE_ERR_MSG ('INVALID VARIABLE :'                    !**
     2              //VAR_NAME(1:LENSTR(VAR_NAME))//' IN LINE:') 
              RETURN
            ENDIF
C
C---        Place the variable name and then C location in the outline string.
            OUTLINE(18:23) = VAR_NAME(1:6)
            FLOC_IN_C = LOC_IN_C
            WRITE(OUTLINE(35:38),'(F4.0)') FLOC_IN_C
            IF( I .LT. LENSTR(LINE1) ) THEN             ! Place units on outline.
              UNITS = LINE1( NXT_CHAR(I+1,LINE1):LENSTR(LINE1) )
              OUTLINE(40:45) = UNITS(1:6)
            ENDIF
C
          ENDIF
C
        ENDIF
C
        WRITE(ICADIN,800) OUTLINE(1:LENSTR(OUTLINE))
  800   FORMAT( A )
C
      ENDDO
C
      WRITE(ICADIN,810)
  810 FORMAT( '16' )
C
      RETURN
      END
      SUBROUTINE KEY_LOAD
C
C----------------------------------------------------------------------
C
C     This module places type 12 card in the CADIN.ASC file 
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      WRITE(ICADIN,100) 
  100 FORMAT( '12' )
C      
      RETURN
      END
      SUBROUTINE KEY_MODULE
C
C----------------------------------------------------------------------
C
C     This module determines the modules that have been entered and 
C     includes them in the output file CADIN.ASC.
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      LOGICAL MOD_FOUND, END_OF_FILE, SAVE_TYPE4
C
      CHARACTER MOD_NAME*15, OUTLINE*132
C
      DIMENSION VALID_MODULES(2:35), MODULE_NAMES(2:35)
      CHARACTER VALID_MODULES*3,     MODULE_NAMES*15

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
      OUTLINE = ' '
C  
      CALL GET_WK_STRING( END_OF_FILE, SAVE_TYPE4 )
      IF(END_OF_FILE)CALL END_MSG_STOP('PREMATURE END OF FILE REACHED')
C
      DOWHILE( WK_STRING(1:3) .NE. 'END' )
C
        I = 2
        MOD_FOUND = .FALSE.
        DO WHILE (.NOT. MOD_FOUND .AND. I .LE.  35)   ! there are 35 MODULEs
          IF( INDEX(WK_STRING, VALID_MODULES(I)) .EQ. 1 ) THEN
            MOD_FOUND = .TRUE.
          ELSE
            I = I + 1
          ENDIF
        ENDDO
C
        IF (.NOT. MOD_FOUND) THEN
          CALL ISSUE_ERR_MSG( 'INVALID MODULE NAME' )                     !**
C
        ELSE
C
          IF ( LENSTR(WK_STRING) .GT. LENSTR(VALID_MODULES(I)) ) THEN
C
C---        A module name was entered after the module.
            IFIRST = NXT_CHAR( LENSTR(VALID_MODULES(I))+1,WK_STRING)
            MOD_NAME = WK_STRING( IFIRST:LENSTR(WK_STRING) )

          ELSE
C
C---        Use the default module name.
            MOD_NAME = MODULE_NAMES(I)
C
          ENDIF
C
C---      Create the type 2 card and write it to the output file.
          OUTLINE(1:2) = '02'
          OUTLINE(4:6) = VALID_MODULES(I)
          OUTLINE(8:20) = MOD_NAME
          WRITE(OUTLINE(22:25),'(I4.4)') I
C      
          WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
C
        ENDIF          
C
        CALL GET_WK_STRING( END_OF_FILE, SAVE_TYPE4 )
        IF(END_OF_FILE)CALL END_MSG_STOP
     1      ('PREMATURE END OF FILE REACHED')
C
      ENDDO 
C
      RETURN
      END
      SUBROUTINE KEY_MONTE
C
C----------------------------------------------------------------------
C
C     This command is converted to a type 5 card if the number
C     of runs is > 1.  
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      I = LAST_CHAR(1,WK_STRING)                ! get past "MONTE" keyword
      I = NXT_CHAR(I+1,WK_STRING)               ! skip blank spaces
C      
      IF( WK_STRING(I:I+4) .EQ. 'CARLO' ) I = NXT_CHAR(I+5,WK_STRING)
      IF( I .GT. LENSTR(WK_STRING) ) THEN                                 !**
        CALL ISSUE_ERR_MSG( 'NUMBER OF RUNS NOT INLCUDED' )
        RETURN
      ENDIF
C
C---  I is first position of number, find last position of num - J          
      J =  LAST_CHAR(I,WK_STRING)
      CALL READ_INTERNAL( RVAL, IERR, WK_STRING(I:J) )
      IF(IERR .GT. 0) THEN
        CALL ISSUE_ERR_MSG('ERROR READING NUMBER OF RUNS')                !**
        RETURN
      ENDIF
      NUM_RUNS = NINT( RVAL )
C      
      IF( NUM_RUNS .GT. 1 ) WRITE(ICADIN,200) NUM_RUNS
  200 FORMAT( '05', T26, I5 )
C
      RETURN
      END  
      SUBROUTINE KEY_RAND
C
C----------------------------------------------------------------------
C
C     This module translates the RANDOM statement into the format that
C     the CADIN.ASC requires.
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      CHARACTER OUTLINE*132, RMETHOD*3, CH_VAL*15
C      
C---  Initialize the output line.
      OUTLINE = ' '  
C      
C---  The first non-blank after RANDOM 
      IFIRST = NXT_CHAR(7,WK_STRING)  
C
C---  Make sure the next character is a left paren.
      IF( WK_STRING(IFIRST:IFIRST) .NE. '(' ) THEN
         CALL ISSUE_ERR_MSG
     1   ('INVALID RANDOM STATEMENT: NO OPEN PAREN INCLUDED ')
         RETURN
      ENDIF 
      IFIRST = IFIRST + 1     ! Move past the paren.
C             
C---  Determine if the RANDOM statement includes a seed initialization method.
      ICOMMA = INDEX(WK_STRING,',')
      IF( ICOMMA .GT. 0 ) THEN
C---    An initialization method was included; determine which one.
        ISTRT = ICOMMA + 1
        IF
     1  (WK_STRING(ISTRT:ISTRT+LENSTR('ONCE')-1)    .EQ.'ONCE')     THEN
          RMETHOD = '0.0'
        ELSE IF
     1  (WK_STRING(ISTRT:ISTRT+LENSTR('EVERY')-1)   .EQ.'EVERY')    THEN
          RMETHOD = '1.0'
        ELSE IF
     1  (WK_STRING(ISTRT:ISTRT+LENSTR('RUNGROUP')-1).EQ.'RUNGROUP') THEN
          RMETHOD = '2.0'
        ELSE
     1  IF(WK_STRING(ISTRT:ISTRT+LENSTR('RUN')-1)   .EQ.'RUN')    THEN
          RMETHOD = '3.0'
        ELSE
          CALL ISSUE_ERR_MSG
     1    ('INVALID RANDOM STATEMENT: INVALID INITIALIZATION METHOD')
          RETURN   
        ENDIF 
C
C---    Find the location of the last character of the seed value.         
        IEND = ICOMMA - 1
C            
      ELSE          
C
C---    Use the default method.      
        RMETHOD = '0.0' 
C
C---    Look for the close paren  to find the location of the 
C---    last character of the seed value.   
        IPAREN = INDEX( WK_STRING, ')' )
        IF( IPAREN .EQ. 0 ) THEN
          CALL ISSUE_ERR_MSG
     1    ('INVALID RANDOM STATEMENT: NO CLOSE PAREN INCLUDED')
          RETURN   
        ENDIF
        IEND = IPAREN - 1     
      ENDIF
C
C---  Check to see if a valid value is entered by calling READ_INTERNAL
      CH_VAL = WK_STRING(IFIRST:IEND)
      CALL READ_INTERNAL( RSEED, IERR, CH_VAL )
C
      IF(IERR .GT. 0)THEN
        CALL ISSUE_ERR_MSG                                           
     1  ('INVALID RANDOM STATEMENT: ERROR READING RANDOM SEED')
        RETURN
      ENDIF
C
C---  Create and place the type 3 card in the CADIN.ASC file.
      OUTLINE(1:2) = '03' 
C      
C---  Place the variable name and C location in the output line.
      OUTLINE(3:10)   = ' RANSEED'   
      OUTLINE(22:25) = '0090'
C
C---  Place the seed value in the output line.   
      CALL WRITE_INTERNAL2(RSEED, IERR, OUTLINE(31:45) ) 
CJH      CALL WRITE_INTERNAL(RSEED, IERR, OUTLINE(31:45) ) 
C
C---  Place the seed initialization method in the output line.      
      OUTLINE(47:49) = RMETHOD 
C      
C---  Check to see if a stage was included.
      ISTAGE = INDEX( WK_STRING, 'STAGE' )
      IF( ISTAGE .GT. 0 ) THEN
         ISTAGE = ISTAGE + 6
         LEN_WKSTRING = LENSTR(WK_STRING)
         CALL READ_INTERNAL(STAGE,IERR,WK_STRING(ISTAGE:LEN_WKSTRING))
         IF ( IERR .NE. 0 ) THEN
           CALL ISSUE_ERR_MSG
     1      ( 'INVALID RANDOM STATEMENT: ERROR READING STAGE VALUE' )
           RETURN
         ENDIF
         JSTAGE = NINT(STAGE)
         WRITE(OUTLINE(61:62),'(I2.2)') JSTAGE
      ENDIF
C
      WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))  
C
      RETURN
      END
      SUBROUTINE KEY_RUN
C
C----------------------------------------------------------------------
C
C     This module places type 6 card in the CADIN.ASC file 
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
C---  Check to see if the previous card is a type 16, if it is, replace
C---  it with a type 06 card.
      BACKSPACE ( ICADIN )
      READ( ICADIN, *, ERR=100 ) ITYPE
C
      IF( ITYPE .EQ. 16 ) BACKSPACE( ICADIN )
C
  100 WRITE(ICADIN,200)
  200 FORMAT( '06' )
C      
      RETURN
      END
      SUBROUTINE KEY_SAVE
C
C----------------------------------------------------------------------
C
C     This module places type 90 card in the CADIN.ASC file 
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      LOGICAL FIRST_TIME 
      DATA FIRST_TIME / .TRUE. /
C
      IF( FIRST_TIME ) THEN
        WRITE(ICADIN,100) 
  100   FORMAT( '90' )
        FIRST_TIME = .FALSE.
      ELSE
        CALL ISSUE_ERR_MSG( 'ERROR - ONLY ONE SAVE COMMAND IS ALLOWED' )  !**        
      ENDIF
C      
      RETURN
      END
      SUBROUTINE KEY_STOP
C
C----------------------------------------------------------------------
C
C     This module places type 13 card in the CADIN.ASC file 
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      WRITE(ICADIN,100) 
  100 FORMAT( '13' )
C      
      RETURN
      END
      SUBROUTINE KEY_SWEEP
C
C----------------------------------------------------------------------
C
C     This module places the sweep cards (ie, 19,20,21) in the CADIN.ASC.
C
C----------------------------------------------------------------------
C
      COMMON /INPUT_STRING/ INLINE,     WK_STRING
      CHARACTER             INLINE*132, WK_STRING*132  
C      
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      LOGICAL SAVE_TYPE4, END_OF_FILE
C
      CHARACTER OUTLINE*132,     TEMP*132
C
C
C---	MODULE SPECIFIC INFORMATION!!!!!	
C      
C---	Data statements will be set at the beginning of the program NOT RESET EVERYTIME
C	THE PROGRAM ENTERS THIS MODULE.
C
C
      DIMENSION SWEEP_KEY(6), VALID_KEY(6)
C
      CHARACTER SWEEP_KEY*4,     DEGR*2
      CHARACTER ANGL_VAR*10,     RANG_VAR*10,     TEST_VAR*10
      CHARACTER RANG_DELT_STR*5, ANGL_DELT_STR*5 
C
      LOGICAL VALID_KEY
      LOGICAL A_DELT_FOUND, R_DELT_FOUND
      LOGICAL T_MAX_FOUND, A_MAX_FOUND,  R_MAX_FOUND
      LOGICAL A_VALID_KEY
	LOGICAL SWEEP_BLOCK_PROCESSED
C
      DATA NUM_KEYS /6/
	DATA VALID_KEY/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./
      DATA SWEEP_KEY/'MODE', 'LIMI', 'NUM', 'RANG', 'ANGL', 'END '/
      DATA SWEEP_BLOCK_PROCESSED / .FALSE. /
C
C---  Set the error flags.
C
      IERR = 0
	INVALID = 0
	RANG_DELT_STR = ''
C            
C---	Determines that only one sweep block is allowed per input.asc file.
C
      IF( SWEEP_BLOCK_PROCESSED ) CALL ISSUE_ERR_MSG
     1     ('ERROR - ONLY ONE SWEEP BLOCK ALLOWED IN INPUT')
C
C---	Sets TRUE that a sweep block has already been processed if this routine is called again.
C
      SWEEP_BLOCK_PROCESSED  = .TRUE.
C
C---	Sets a flag to be passed to "GET WORKING STRING" that input is a comment.
C---	Believed to be a place holder for CALL statement in this module.
C
      SAVE_TYPE4 = .TRUE.
C
C---	Gets the next line from the input file.
C
  100 CALL GET_WK_STRING( END_OF_FILE, SAVE_TYPE4 )
C
C--	If the end of file has been reached from "GET WORKING STRING" an error message will
C	be issued.
C
      IF( END_OF_FILE ) CALL END_MSG_STOP
     1     ('END OF FILE REACHED DURING SWEEP BLOCK')
C
C---	For each line returned from "GET WORKING STRING", got through the "SWEEP KEY WORDS"
C	and determine if this particular one is valid.  If it is valid, set VALID_KEY = TRUE.
C---	If not issue an error message.
C
	A_VALID_KEY = .FALSE.
      DO I = 1, NUM_KEYS
C
C---	Go through entire list of valid sweep keys.
C
		IF ( WK_STRING(1:4) .EQ. SWEEP_KEY(I) ) THEN
C
C---			This is a valid statement.  Save the number of the key.
C
			A_VALID_KEY = .TRUE.
			ICOMMAND = I
C
C--			Check to see if this is a duplicate statement.  If yes, issue error; get next line
C			from input file.  If no, set validkey = true.
C
			IF ( VALID_KEY(I) .EQ. .TRUE. ) THEN
				CALL ISSUE_ERR_MSG('DUPLICATE COMMAND IN SWEEP BLOCK')
				GOTO 100
			ENDIF
			VALID_KEY(I) = .TRUE.
		ENDIF
      ENDDO
C
C---	Did the previous loop find a statement that was NOT a sweep_key statement.  If yes, then
C	print error message; go get another statement.
C
      IF (.NOT. A_VALID_KEY ) THEN
        CALL ISSUE_ERR_MSG
     1     ('INVALID STATEMENT IN SWEEP BLOCK: CHECK SPELLING')
C
C---		Allow 2 invalid statements in sweep block before ending.
C
		INVALID = INVALID + 1
		IF ( INVALID .GE. 2 ) THEN
			CALL ISSUE_ERR_MSG
     1('TWO INVALID STATEMENTS FOUND IN SWEEP BLOCK:PROGRAM TERMINATED')
			WRITE(*,'(// 5X, A /)' ) 
     1('TWO INVALID STATEMENTS FOUND IN SWEEP BLOCK:PROGRAM TERMINATED')
			STOP
		ENDIF
        GOTO 100
      ENDIF
C
C---    Remove the key word from the working string.
C
      IFIRST = NXT_CHAR( LAST_CHAR( 1, WK_STRING )+1, WK_STRING )
      TEMP = WK_STRING( IFIRST:LENSTR(WK_STRING) )
      WK_STRING = TEMP
C
C---	Based on the key work contained in WK_STRING, goto the appropriate module.
C		  'MODE''LIMI' 'NUM''RANG''ANGL''END '     
      GOTO ( 1100, 1200, 1300, 1400, 1500, 1600 ) ICOMMAND      
C
C---	Determines the SWEEP Mode: 0-5
C
 1100 CALL SWEEP_MODE( MODE_IN )
      GOTO 100      
C
C---	 Retrieves test variable limits.
C
 1200 CALL SWEEP_LIMITS
     1     (IERR, TEST_VAR, LOC_TEST, TEST_MIN, TEST_MAX, T_MAX_FOUND)
      GOTO 100
C 
C---	Retrieves the number of trajectories or binary searches depending on the mode.
C
 1300 CALL SWEEP_NUMBER( NUM_TRAJ_BIN )
      GOTO 100
C
C---	Retrieves the RANGE DELTA then retrieves the RANGE MAX'S AND MIN'S
C
 1400 CALL SWEEP_DELT( IERR, RANG_DELT, R_DELT_FOUND, RANG_DELT_STR)
      IF( IERR .GT. 0 ) GOTO 100 
C      
      CALL SWEEP_LIMITS
     1     (IERR, RANG_VAR, LOC_RANG, RANG_MIN, RANG_MAX, R_MAX_FOUND)
      IF(.NOT. R_MAX_FOUND) THEN
        CALL ISSUE_ERR_MSG('RANGE MAX NOT ENTERED')
      ENDIF
      GOTO 100
C
C---	Retrieves the ANGLE UNITS, ANGLE DELTA then retrieves the ANGLE MAX'S AND MIN'S
C
 1500 CALL SWEEP_DEGR_RADS( DEGR )
C 
      CALL SWEEP_DELT( IERR, ANGL_DELT, A_DELT_FOUND, ANGL_DELT_STR)
      IF(.NOT. A_DELT_FOUND) THEN
        CALL ISSUE_ERR_MSG('ANGLE DELTA NOT ENTERED')
        GOTO 100
      ENDIF
      IF( IERR .GT.0 ) GOTO 100
C      
      CALL SWEEP_LIMITS
     1   (IERR, ANGL_VAR,LOC_ANGL, ANGL_MIN, ANGL_MAX, A_MAX_FOUND)
      IF(.NOT. A_MAX_FOUND) THEN
        CALL ISSUE_ERR_MSG('ANGLE MAX NOT ENTERED')
      ENDIF
      GOTO 100
C
C---	Continues due to 'END ' being found.
C
 1600 CONTINUE      
C
C---	If MODE is not present, kick user out.
C
	IF ( .NOT. VALID_KEY(1) ) THEN
		CALL ISSUE_ERR_MSG('"MODE" FOR SWEEP BLOCK NOT FOUND')
		IERR = 1
	ENDIF
C
C---	If there was MODE, kick user out!!!!
C
	IF( IERR .GT. 0 ) RETURN      
C
C---  Make sure that all necessary data is present.      
C		  'LIMI', 'NUM', 'RANG', 'ANGL'
C			2		3		4		5		
C
C---	For a SWEEP MODE = 0
C
	IF ( MODE_IN .EQ. 0 ) THEN
C
C---	Things that must be present are: LIMI, RANG, ANGL
C
		IF ( .NOT. VALID_KEY(2) ) THEN
			CALL ISSUE_ERR_MSG
     1        (SWEEP_KEY(2) // 'COMMAND NOT INCLUDED IN SWEEP BLOCK')
			IERR = 1
		ELSEIF ( .NOT. VALID_KEY(4) ) THEN
              CALL ISSUE_ERR_MSG
     1        (SWEEP_KEY(4) // 'COMMAND NOT INCLUDED IN SWEEP BLOCK')
			IERR = 1
		ELSEIF ( .NOT. VALID_KEY(5) ) THEN
              CALL ISSUE_ERR_MSG
     1        (SWEEP_KEY(5) // 'COMMAND NOT INCLUDED IN SWEEP BLOCK')
			IERR = 1
		ELSEIF(.NOT. R_DELT_FOUND) THEN
			CALL ISSUE_ERR_MSG
     1		('RANGE DELTA IS REQUIRED FOR SELECTED SWEEP MODE' )             
			IERR = 1
		ENDIF
C
C---	For a SWEEP MODE = 1 - 5
C
	ELSE
C
C---	Things that must be present are: LIMI, RANG, ANGL, NUM
C
		IF ( .NOT. VALID_KEY(2) ) THEN
              CALL ISSUE_ERR_MSG
     1        (SWEEP_KEY(2) // 'COMMAND NOT INCLUDED IN SWEEP BLOCK')
			IERR = 1
		ELSEIF ( .NOT. VALID_KEY(4) ) THEN
              CALL ISSUE_ERR_MSG
     1        (SWEEP_KEY(4) // 'COMMAND NOT INCLUDED IN SWEEP BLOCK')
			IERR = 1
		ELSEIF ( .NOT. VALID_KEY(5) ) THEN
              CALL ISSUE_ERR_MSG
     1        (SWEEP_KEY(5) // 'COMMAND NOT INCLUDED IN SWEEP BLOCK')
			IERR = 1
		ELSEIF ( .NOT. VALID_KEY(3) ) THEN
              CALL ISSUE_ERR_MSG
     1        (SWEEP_KEY(3) // 'COMMAND NOT INCLUDED IN SWEEP BLOCK')
			IERR = 1
		ELSEIF ((MODE_IN .EQ. 4) .OR. (MODE_IN .EQ. 5)) THEN
			IF(.NOT. R_DELT_FOUND) THEN
				CALL ISSUE_ERR_MSG
     1			('RANGE DELTA IS REQUIRED FOR SELECTED SWEEP MODE')
				IERR = 1
			ENDIF
		ENDIF
	ENDIF
C
C---	If there was a BAD enough error, kick user out!!!!
C
	IF( IERR .GT. 0 ) RETURN      
C
C---  Create card type 19      
C
      OUTLINE = ' '                    ! clear out the output line
      OUTLINE(1:2) = '19'
      OUTLINE(4:11) = ANGL_VAR(1:8)
      WRITE(OUTLINE(22:25),'(I4.4)') LOC_ANGL
      CALL WRITE_INTERNAL2(ANGL_DELT,IERR,OUTLINE(26:30))
      WRITE(OUTLINE(27:31), '(A)') ANGL_DELT_STR
      CALL WRITE_INTERNAL2(ANGL_MIN,IERR,OUTLINE(31:45))
      CALL WRITE_INTERNAL2(ANGL_MAX,IERR,OUTLINE(46:60))
      OUTLINE(61:62) = DEGR
      WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
C
C---  Create card type 20
C
      OUTLINE = ' '                    ! clear out the output line
      OUTLINE(1:2) = '20'
      OUTLINE(4:11) = RANG_VAR(1:8)
      WRITE(OUTLINE(22:25),'(I4.4)') LOC_RANG
      CALL WRITE_INTERNAL2(RANG_MIN,IERR,OUTLINE(31:45))
      CALL WRITE_INTERNAL2(RANG_MAX,IERR,OUTLINE(46:60))
      WRITE(OUTLINE(27:31), '(A)') RANG_DELT_STR
      WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
C
C---  Create card type 21
C
      OUTLINE = ' '                    ! clear out the output line
      OUTLINE(1:2) = '21'
      WRITE(OUTLINE(30:30),'(I1.1)') MODE_IN
	OUTLINE(4:11) = TEST_VAR(1:8)
	WRITE(OUTLINE(22:25),'(I4.4)') LOC_TEST
	CALL WRITE_INTERNAL2(TEST_MIN,IERR,OUTLINE(31:45))
C
C---	Determine if NUM will be calculated or set equal to 4.
C
      IF( MODE_IN .EQ. 4 .OR. MODE_IN .EQ. 5 ) THEN
C	
C---  The "NUM" variable will be provided by the user but is written in a specific place.
C
		WRITE(OUTLINE(61:62),'(I2.2)' ) NUM_TRAJ_BIN
      ELSE
C	
C---  For a mode = 0, the "NUM" variable will be calculated using
C	the max & min range values and the range delta.  For modes 1-3, the "NUM" variable
C	will be provided by the user.  For Modes 0-3 the "NUM" variable will be written
C	to a specific place.
C       
		IF ( MODE_IN .EQ. 0 ) THEN
			CALL CAL_NUM(NUM_TRAJ_BIN,RANG_MAX,RANG_MIN,RANG_DELT)
		ENDIF
		PNUM_TRAJ_BIN = NUM_TRAJ_BIN
		WRITE(OUTLINE(55:60),'(F6.0)' ) PNUM_TRAJ_BIN
      ENDIF
      WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
C
C---  Use a type 3 card to save the maximum value of the test mode of 
C---  the sweep, if it was set.
C
      IF( T_MAX_FOUND ) THEN
		OUTLINE = ' '                  ! clear out the output line
          OUTLINE(1:2) = '03'
          OUTLINE(4:8) = 'CRITM'
          OUTLINE(22:25) = '1805'
          CALL WRITE_INTERNAL2( TEST_MAX,IERR, OUTLINE(31:45) )
          WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
      ENDIF
C
      RETURN
      END
      SUBROUTINE KEY_VECT
C
C----------------------------------------------------------------------
C
C     This module places the appropriate cards in the CADIN.ASC, depending
C     on the type of vector command given.  Vector can result in a type 7 
C     card or multiple type 3.
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      CHARACTER VECT_NAME*12, TEMP*132, SAVE_STAGE*2, OUTLINE*132
      LOGICAL LAST_ASSIGN, VECT_FOUND, END_OF_FILE, SAVECOM, SAVE_TYPE4
C
C---  Initialize the output line.
      OUTLINE = ' '
C
C---  Remove the keyword from the working string.
      IFIRST = NXT_CHAR(LAST_CHAR(1,WK_STRING)+1,WK_STRING)
      TEMP = WK_STRING( IFIRST:LENSTR(WK_STRING))
      WK_STRING = TEMP
C
C---  Get the vector name.
      ILAST = LAST_CHAR(1,WK_STRING)
      VECT_NAME = WK_STRING(1:ILAST)
      SAVECOM = .TRUE.
      CALL SEARCH_HEAD
     1    (IERR,VECT_NAME,LOC_IN_C,VECT_FOUND,IDIM,SAVECOM) 
      IF( IERR .GT. 0 )RETURN
C
C---  Note: if idim is less than 2, then the vect_name is a single      
C---  variable or a vector element, which would make this line
C---  invalid.
      IF( .NOT. VECT_FOUND ) THEN 
        CALL ISSUE_ERR_MSG ('INVALID VECTOR STATEMENT: CHECK SPELLING;
     1                      POSSIBLE VARIABLE IS NOT A VECTOR')   !**
        RETURN
      ENDIF  
C      
      IF( IDIM .LT. 2 ) THEN
        CALL ISSUE_ERR_MSG
     1     ('INVALID VECTOR STATEMENT: VARIABLE IS NOT A VECTOR')
        RETURN                                                            !**
      ENDIF
C      
C---  Place the comment from the HEAD.ASC in the new INPUT.ASC file.
      CALL WRITE_NEWINPUT_WITH_COMMENT
C
C---  Place the vector name in the output line.
      OUTLINE(4:11) = VECT_NAME(1:8)
C
C---  Determine if the stage is being set on the vector command line.
      ISTAGE = INDEX(WK_STRING, 'STAGE' )
      IF( ISTAGE .GT. 0 ) THEN
C
        IFIRST = ISTAGE + 6
        LEN_WKSTRING = LENSTR(WK_STRING)
        CALL READ_INTERNAL(STAGE, IERR, WK_STRING(IFIRST:LEN_WKSTRING))

        IF( IERR .GT. 0 ) THEN
          CALL ISSUE_ERR_MSG( 'INVALID STAGE ASSIGNMENT' )                !**
          RETURN
        ENDIF
C
        JSTAGE = NINT(STAGE)
        WRITE(OUTLINE(61:62),'(I2.2)') JSTAGE
C
C---    Remove the stage data from the working string.
        WK_STRING(ISTAGE:LEN_WKSTRING) = ' '
      ENDIF
C
C---  Determine if the vector is being set to a single value (or another
C---  variable name) or if its elements are being assigned to individual
C---  functions/values/variables.  For the first case, the value (or v
C---  variable name) will be on the same line as the vector command. ie,
C---  the length of the wk_string at this point will be longer than than
C---  the position of the last character of the vector name.
      IF( LENSTR(WK_STRING) .GT. ILAST ) THEN
C
        OUTLINE(1:2) = '07'
C
C---    Save the c location of the first vector element.
        WRITE(OUTLINE(22:25),'(I4.4)') LOC_IN_C
C
        TEMP = WK_STRING(NXT_CHAR(ILAST+1,WK_STRING):LENSTR(WK_STRING))
        WK_STRING = TEMP
C
C---    Determine the value the vector elements are being set to. 
C---    This will be written to the CADIN.ASC file as a type  7 card.
C          
        CALL CT3_VALUE( IERR, PAR1 )
        IF( IERR .GT. 0 ) RETURN
C        
        CALL WRITE_INTERNAL2(PAR1, IERR, OUTLINE(31:45) )
CJH        CALL WRITE_INTERNAL(PAR1, IERR, OUTLINE(31:45) )
        OUTLINE(27:27) = '-'
        WRITE(OUTLINE(28:30),'(I3.3)' ) IDIM
        WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
C        
      ELSE
C
C---    The elements of the vector are being set to different "values".
C---    Note the user does not have to have an assignment for every
C---    element, but the array is filled in order as far as there are
C---    assignment statements.
C
        OUTLINE(1:2) = '03'
        SAVE_STAGE = OUTLINE(61:62)       ! Save the general stage.
        LAST_ASSIGN = .FALSE.
        IELEMENT_LOC_IN_C = LOC_IN_C - 1
        DO WHILE ( .NOT. LAST_ASSIGN )
          SAVE_TYPE4 = .TRUE.
          CALL GET_WK_STRING( END_OF_FILE, SAVE_TYPE4 )
          IF( END_OF_FILE ) CALL END_MSG_STOP
     1       ( 'END OF FILE REACHED IN VECTOR BLOCK' )
C
          IF( WK_STRING(1:3) .EQ. 'END' ) THEN
            LAST_ASSIGN =  .TRUE.
          ELSE
CC            CALL CT3_ASSIGN( IERR, OUTLINE )
CC            IF( IERR .GT. 0 ) RETURN
CC            IF(IERR .LT. 1) RETURN
C
C
C---        Determine if the stage is being set for the current vector
C---        element. 
            ISTAGE = INDEX(WK_STRING, 'STAGE' )
            IF( ISTAGE .GT. 0 ) THEN
              IFIRST = ISTAGE + 6
              LEN_WKSTRING = LENSTR(WK_STRING)
              CALL READ_INTERNAL(STAGE, IERR, 
     1                  WK_STRING(IFIRST:LEN_WKSTRING))
              IF( IERR .GT. 0 ) THEN
                CALL ISSUE_ERR_MSG('INVALID STAGE ASSIGNMENT' )           !**
                RETURN
              ENDIF
              JSTAGE = NINT(STAGE)
              WRITE(OUTLINE(61:62),'(I2.2)') JSTAGE
            ENDIF
            NUM_READ = 0
            DO WHILE( LENSTR(WK_STRING) .GT. 0 .AND. NUM_READ .LT. IDIM)
               IBLANK = INDEX( WK_STRING,' ')
               ICOMMA = INDEX( WK_STRING,',')
C
               IF( ICOMMA .EQ. 0 ) ICOMMA = IBLANK
                  ILAST = MIN( ICOMMA, IBLANK )
                  CALL READ_INTERNAL(RVAL,IERR,WK_STRING(1:ILAST-1))

               IF( IERR .GT. 0 ) THEN
                  CALL ISSUE_ERR_MSG                                            !**
     1                  ('ERROR READING DATA IN VECTOR STATEMENT') 
                  RETURN
               ENDIF
C
               NUM_READ = NUM_READ + 1
               CALL WRITE_INTERNAL2(RVAL, IERR,OUTLINE(31:45))
CJH               CALL WRITE_INTERNAL(RVAL, IERR,OUTLINE(31:45))
C
               IF( ILAST .LT. LENSTR(WK_STRING)) THEN
                  TEMP = 
     1          WK_STRING(NXT_CHAR(ILAST+1,WK_STRING):LENSTR(WK_STRING))
                  WK_STRING = TEMP
               ELSE
                  WK_STRING = ' '
               ENDIF
C
C
C---        The c location of the first vector element.
            IELEMENT_LOC_IN_C = IELEMENT_LOC_IN_C +1
            WRITE(OUTLINE(22:25),'(I4.4)') IELEMENT_LOC_IN_C

C---  Write the card to the CADIN.ASC file followed by its data.
            WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
            OUTLINE(61:62) = SAVE_STAGE(1:2)
            ENDDO
            IF( LENSTR(WK_STRING) .NE. 0 ) THEN
               CALL ISSUE_ERR_MSG
     1                ( 'TOO MANY VALUES IN VECTOR STATEMENT' )      !**
               RETURN
            ENDIF          
          ENDIF       
        ENDDO
      ENDIF
      RETURN
C
      END
      SUBROUTINE KEY_VECTV
C
C----------------------------------------------------------------------
C
C     This module places a type 7 card in the CADIN.ASC file.  In this 
C     command, values are included for all the elements of the array 
C     following the vector statement.  If all the values are not 
C     included, an error message is given.  
C
C----------------------------------------------------------------------
C
      DIMENSION VALUE(400)
C      
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      CHARACTER VECT_NAME*12, TEMP*132, OUTLINE*132
      LOGICAL VECT_FOUND, END_OF_FILE, SAVECOM, SAVE_TYPE4
C
C---  Initialize the output line.
      OUTLINE = ' '
C
C---  Place card type in outline.
      OUTLINE(1:2) = '07'
C
C---  Remove the keyword from the working string.
      IFIRST = NXT_CHAR(LAST_CHAR(1,WK_STRING)+1,WK_STRING)
      TEMP = WK_STRING( IFIRST:LENSTR(WK_STRING))
      WK_STRING = TEMP
C
C---  Get the vector name.
      ILAST = LAST_CHAR(1,WK_STRING)
      VECT_NAME = WK_STRING(1:ILAST)
      SAVECOM = .TRUE.
      CALL SEARCH_HEAD
     1   (IERR,VECT_NAME,LOC_IN_C,VECT_FOUND,IDIM,SAVECOM)
      IF( IERR .GT. 0 )RETURN
C      
C---  Place the comment from the HEAD.ASC in the new INPUT.ASC file.
      CALL WRITE_NEWINPUT_WITH_COMMENT
C
C---  Note: if idim is less than 2, then the vect_name is a single      
C---  variable or a vector element, which would make this line
C---  invalid.
      IF( .NOT. VECT_FOUND ) THEN
        CALL ISSUE_ERR_MSG
     1     ('INVALID VECTOR STATEMENT: CHECK SPELLING')                   !**
        RETURN
      ELSEIF( IDIM .LT. 2 ) THEN
        CALL ISSUE_ERR_MSG
     1     ('INVALID VECTOR STATEMENT: VARIABLE IS NOT A VECTOR')         !**
        RETURN
      ENDIF
C      
C---  Place the vector name in the output line, the vectors starting
C---  C location and its dimension.
      OUTLINE(4:11) = VECT_NAME(1:8)
      WRITE(OUTLINE(22:25),'(I4.4)') LOC_IN_C
      WRITE(OUTLINE(27:30),'(I4.4)') IDIM
C
C---  Determine if the stage is being set.
      ISTAGE = INDEX(WK_STRING, 'STAGE' )
      IF( ISTAGE .GT. 0 ) THEN
        IFIRST = ISTAGE + 6
        LEN_WKSTRING = LENSTR(WK_STRING)
        CALL READ_INTERNAL(STAGE, IERR, WK_STRING(IFIRST:LEN_WKSTRING))   
        IF( IERR .GT. 0 ) THEN
          CALL ISSUE_ERR_MSG('INVALID STAGE ASSIGNMENT' )                 !**
          RETURN
        ENDIF
        JSTAGE = NINT(STAGE)
        WRITE(OUTLINE(61:62),'(I2.2)') JSTAGE
      ENDIF
C
      SAVE_TYPE4 = .FALSE.
      NUM_READ = 0
      DO WHILE (NUM_READ .LT. IDIM )
C
        CALL GET_WK_STRING( END_OF_FILE, SAVE_TYPE4 )
        IF( END_OF_FILE ) CALL END_MSG_STOP
     1     ('END OF FILE REACHED WHILE READING VECTORV DATA' )
C
        DO WHILE( LENSTR(WK_STRING) .GT. 0 .AND. NUM_READ .LT. IDIM )
          IBLANK = INDEX( WK_STRING,' ')
          ICOMMA = INDEX( WK_STRING,',')
C
          IF( ICOMMA .EQ. 0 ) ICOMMA = IBLANK
          ILAST = MIN( ICOMMA, IBLANK )
          CALL READ_INTERNAL(RVAL,IERR,WK_STRING(1:ILAST-1))

          IF( IERR .GT. 0 ) THEN
            CALL ISSUE_ERR_MSG                                            !**
     1        ('ERROR READING DATA IN VECTORV STATEMENT') 
            RETURN
          ENDIF
C
          NUM_READ = NUM_READ + 1
          VALUE(NUM_READ) = RVAL
C
          IF( ILAST .LT. LENSTR(WK_STRING)) THEN
            TEMP = 
     1        WK_STRING(NXT_CHAR(ILAST+1,WK_STRING):LENSTR(WK_STRING))
            WK_STRING = TEMP
          ELSE
            WK_STRING = ' '
          ENDIF
        ENDDO
      ENDDO
C
      IF( LENSTR(WK_STRING) .NE. 0 ) THEN
        CALL ISSUE_ERR_MSG( 'TOO MANY VALUES IN VECTORV STATEMENT' )      !**
        RETURN
      ENDIF
C
C---  Write the card to the CADIN.ASC file followed by its data.
      WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
      WRITE(ICADIN,300) (VALUE(I),I=1,IDIM)
  300 FORMAT(2X, 5(G13.7,1X) )
C
      RETURN
      END
      SUBROUTINE KEY_WEATH
C       
C----------------------------------------------------------------------
C
C     This module places weather cards (type 8) in the CADIN.ASC file 
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      CHARACTER OUTLINE*132
C
      LOGICAL EORS, END_OF_FILE, SAVE_TYPE4
C
      OUTLINE = ' '
      OUTLINE(1:2) = '08'
      WK_STRING = INLINE
C
C---  Check to see if comments were included on the weather keyword line.      
      ICOMM = INDEX( WK_STRING, '!' )
      IF( ICOMM .GT. 0 ) THEN
        ISTART = NXT_CHAR(ICOMM+1, WK_STRING)
        ILAST = LENSTR(WK_STRING)
        OUTLINE(4:20) = WK_STRING(ISTART:ISTART+(20-4)-1)
      ENDIF
C
      WRITE(ICADIN,'(A)') OUTLINE(1:LENSTR(OUTLINE))
C
      NUM_RECORDS = 0
      SAVE_ALT = -1.0
      EORS = .FALSE.
      SAVE_TYPE4 = .FALSE.
      DO WHILE ( .NOT. EORS )
C  
        CALL GET_WK_STRING( END_OF_FILE, SAVE_TYPE4 )       
        IF( WK_STRING(1:3) .NE. 'END'  ) THEN
C          
C---      The line contains weather data. Check to see if the data is valid.
          CALL READ_WEATH( IERR, ALT, DIR, VEL, DENS, TEMPR, PRESS )
          IF( IERR .GT. 0 ) THEN
            CALL ISSUE_ERR_MSG('ERROR READING WEATHER DATA' )
            RETURN
          ELSEIF( ALT .LT. SAVE_ALT ) THEN
            CALL ISSUE_ERR_MSG('ALTITUDE DATA NOT IN ASCENDING ORDER')
            RETURN
          ELSE
            SAVE_ALT = ALT
          END IF
C          
C---      Write the data record to the CADIN.ASC file.
          WRITE(ICADIN,100) ALT, DIR, VEL, DENS, TEMPR, PRESS
  100     FORMAT(6(1X,F10.3))
          NUM_RECORDS = NUM_RECORDS +1
          IF( NUM_RECORDS .GT. 50 ) CALL ISSUE_ERR_MSG
     1       ( 'WEATHER RECORD LIMIT EXCEEDED' )
C        
        ELSE
C         
C---      The end record was found.  Place the end of data record in 
C---      the CADIN.ASC file.
          WRITE(ICADIN,100) -1.0, 0.0, 0.0, 0.0, 0.0, 0.0  
          EORS = .TRUE.
C        
        ENDIF
C
      ENDDO
C
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
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      DATA INPUT/31/, NEWINPUT/32/, ICADIN/33/
      DATA IHEAD/40/, NUHEAD/41/,   JERROR/21/
C
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
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C      
      COMMON /FILE_NAMES/INPUT_NAME, NEWINPUT_NAME, INPUT_BACKUP,
     1                     CADIN_NAME,  CADIN_BACKUP, ERROR_FILE   
      CHARACTER*60       INPUT_NAME, NEWINPUT_NAME, INPUT_BACKUP,
     1                     CADIN_NAME, CADIN_BACKUP, ERROR_FILE
C
      COMMON /PATH_DIR/ FDRIVE,   FDIR
      CHARACTER MSG*80, FDRIVE*2, FDIR*60, FNAME*8, FEXT*4
C
      LOGICAL EXIT_PROG
C
      INTEGER SPLITPATHQQ
C
CC      INTEGER INPUTFILE
CC      DATA INPUTFILE / 1 /     
C      
	  CALL CLEAR_SCREEN
C
        WRITE(*,'(//5X,A/5X,A)') 
     1  'This Program Inputs a Free-Formated File',
     2  'and Outputs a Fixed-Formated File.'
C
C---  Set the parameters needed for prompting for the input HEAD.ASC file.
C
      INPUT_NAME = 'INPUT.ASC'   
      MSG = 'Enter name of the Free-Formated input file:'  
C
C---  Prompt the user for the input input free-formatted file.
C
      CALL GET_FILE( INPUT_NAME, MSG, 0, EXIT_PROG ) 
      IF( EXIT_PROG ) STOP ' '
C                                 
C---  Break up the path and file name of the input file INPUT_NAME.  The
C---  information will be used when establishing INPUT_BACKUP and 
C---  CADIN_BACKUP.
      ILEN = SPLITPATHQQ( INPUT_NAME, FDRIVE, FDIR, FNAME, FEXT ) 
C
C---  Set the input file name to it full name including its path.
C
      INPUT_NAME = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
     1             FNAME(1:LENSTR(FNAME)) // FEXT
C      
C---  Set the NEWINPUT file name.
C
      NEWINPUT_NAME = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
     1             FNAME(1:LENSTR(FNAME)) // '.TMP'
C      
C---  Set the file name for the backup of the input file.
C
      INPUT_BACKUP = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
     1               FNAME(1:LENSTR(FNAME)) // '.BAK'
C
C---  Open the INPUT AND NEWINPUT files.      
C
      OPEN(INPUT, FILE=INPUT_NAME(1:LENSTR(INPUT_NAME)),
     1     FORM='FORMATTED', STATUS='OLD', ERR=900)
C
      OPEN(NEWINPUT,FILE=NEWINPUT_NAME(1:LENSTR(NEWINPUT_NAME)),
     1     FORM='FORMATTED', STATUS='UNKNOWN')    
C
C  Set the parameters needed for prompting for the OUTPUT file.
C
CJH      CADIN_NAME = 'CADIN.ASC'
CJH      MSG = 'Enter name of Fixed-Formated file for output:'
C     
C  
C  Prompt the user for the input Fixed-Formated file.
C
CJH      CALL GET_FILE( CADIN_NAME, MSG, 1, EXIT_PROG ) 
CJH      IF( EXIT_PROG ) STOP ' '
C                                 
C---  Break up the path and file name of the output file CADIN_NAME.  The
C---  information will be used when establishing CADIN_BACKUP.
C
CJH      ILEN = SPLITPATHQQ( CADIN_NAME, FDRIVE, FDIR, FNAME, FEXT ) 
C
C  Set the output file name to its full name including its path.
C
CJH      CADIN_NAME = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
CJH     1             FNAME(1:LENSTR(FNAME)) // FEXT
C
      CADIN_NAME = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
     1             'CADIN' // '.ASC'
C
C---  The output file backup is the user's input file name with the
C---  extenstion ".TP5"; this file will be located in the default directory.
C
      CADIN_BACKUP = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
     1             FNAME(1:LENSTR(FNAME)) // '.TP5'
C
C---  Open the CADIN AND ERROR files.      
C     
      OPEN(ICADIN,FILE=CADIN_NAME(1:LENSTR(CADIN_NAME)),
     1     FORM='FORMATTED',STATUS='UNKNOWN', ERR=910 )
C
C---  Open the error file.  But first build directory and path 
C     from OUTPUT FILE data.
      FNAME = 'ERROR'
      FEXT = '.ASC'
      ERROR_FILE = FDRIVE // FDIR(1:LENSTR(FDIR)) // 
     1             FNAME(1:LENSTR(FNAME)) // FEXT
      OPEN( JERROR, FILE=ERROR_FILE(1:LENSTR(ERROR_FILE)),
     1      STATUS='UNKNOWN' )
C
      RETURN
C
  900 CONTINUE
  910 CONTINUE
  920 CONTINUE
C
      RETURN
      END
      SUBROUTINE READ_INTEGER( I1, IERR, STRING )
C
C----------------------------------------------------------------------
C
C  This module performs an interactive of a real value from the user.
C  This module was written to provide ease of transfer to the PC and
C  for modularity. 
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
C---  Set the error flag.
      IERR = 0
C
C---  Create a temp file
      KSCRATCH = 99
      OPEN(KSCRATCH, FORM = 'FORMATTED', STATUS = 'SCRATCH')
C
C---  Write the string to the temp file.
      WRITE(KSCRATCH,'(A)') STRING
C
C---  Rewind the file and read the string as a real number.
      REWIND( KSCRATCH )
      READ(KSCRATCH,*,ERR=900) I1
C
      CLOSE( KSCRATCH )
      RETURN
C
  900 CONTINUE
      IERR = 1
      CLOSE( KSCRATCH )
C
      RETURN
      END
      SUBROUTINE READ_INTERNAL( R1, IERR, STRING )
C
C----------------------------------------------------------------------
C
C  This module performs an interactive of a real value from the user.
C  This module was written to provide ease of transfer to the PC and
C  for modularity. 
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
C---  Set the error flag.
      IERR = 0
C
C---  Create a temp file
      KSCRATCH = 99
      OPEN(KSCRATCH, FORM = 'FORMATTED', STATUS = 'SCRATCH')
C
C---  Write the string to the temp file.
      WRITE(KSCRATCH,'(A)') STRING
C
C---  Rewind the file and read the string as a real number.
      REWIND( KSCRATCH )
      READ(KSCRATCH,*,ERR=900) R1
C
      CLOSE( KSCRATCH )
      RETURN
C
  900 CONTINUE
      IERR = 1
      CLOSE( KSCRATCH )
C
      RETURN
      END
      SUBROUTINE READ_WEATH( IERR, ALT, DIR, VEL, DENS, TEMPR, PRESS )
C
C----------------------------------------------------------------------
C
C  This module reads a weather record from INLINE and verifies that 
C  the record is valid.
C
C----------------------------------------------------------------------
C
C  ALT, DIR, VEL, DENS, TEMPR, PRESS
C         - The real numbers read from the string passed into this module.
C  IERR   - (I) Output.  An error indicator flag.  IERR is set to 0 at 
C           the beginning of the module.  If an error occurs during the 
C           internal read, IERR is set to 1.
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
C
C---  Set the error flag.
      IERR = 0
C
C---  Create a temp file
      KSCRATCH = 99
      OPEN(KSCRATCH, FORM = 'FORMATTED', STATUS = 'SCRATCH')
C
C---  Write the string to the temp file.
C
      WRITE(KSCRATCH,'(A)') INLINE
C
C---  Rewind the file and read the string as a real number.
C
      REWIND( KSCRATCH )
      READ(KSCRATCH,*,ERR=900) ALT, DIR, VEL, DENS, TEMPR, PRESS
C
      CLOSE( KSCRATCH )
      RETURN
C
  900 CONTINUE
      IERR = 1
C
      CLOSE( KSCRATCH )
      RETURN
      END
      SUBROUTINE SEARCH_HEAD
     1       (IERR,VAR_NAME,LOC_IN_C,FOUND,IDIM,SAVECOMMENT)
C
C----------------------------------------------------------------------
C
C     This module reads past the scroll lines of the HEAD.ASC file and
C     searches for a variable in the valid variables.
C
C--History-------------------------------------------------------------
C
C     March 2000:
C         Removed the the flag SAME_VAR2.  The code with SAME_VAR2
C         allow dsthe nuhead.asc file to search again for a variable
C         definition.  However, this caused an error when an array
C         variable did not have a definition.  All code includinging the
C         SAME_VAR2 flag has been commented out (see CBC).  If a variable
C         does not have a definition, a blank comment is inserted and the
C         subroutine proceeds as if a comment has been found.  This 
C         continues on the basis that only one record per variable in 
C         the HEAD.ASC file.
C
C----------------------------------------------------------------------
C
      COMMON / ASSIGNMNT_COMM/ COMMENT
      CHARACTER                COMMENT*80
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C      
      CHARACTER ASTRING*80, TEMP*132
      CHARACTER VAR_NAME*12, VAR_READ*13, VAR_CHECK*13
C
      LOGICAL EOFILE, FOUND, SAVECOMMENT, SAME_VAR !BC , SAME_VAR2
C
C---  Set the error flag.
      IERR = 0
C      
      IDIM = 1
C
C---  Open the copied HEAD.ASC file.
      OPEN(NUHEAD, FILE='NUHEAD.ASC', FORM='FORMATTED', STATUS='OLD')
C
      FOUND = .FALSE.
      EOFILE = .FALSE.
	SAME_VAR = .FALSE.
CBC	SAME_VAR2 = .FALSE.
      DO WHILE ( .NOT. EOFILE  .AND.  .NOT. FOUND )
C
        EOFILE = .TRUE.
        READ(NUHEAD,'(A)',END=100) ASTRING
        EOFILE = .FALSE.
C
  100   CONTINUE
C
        IF( .NOT. EOFILE ) THEN
          VAR_READ = ASTRING(13:25)    !?? Why add space at end of string - bc, removed the end space
          IFREAD = NXT_CHAR(1, VAR_READ)
          ILREAD = LAST_CHAR(IFREAD, VAR_READ)
          VAR_READ = VAR_READ(IFREAD:ILREAD)
          LEN_READ = LENSTR(VAR_READ)
C          
          IFNAME = NXT_CHAR(1, VAR_NAME)
          ILNAME = LAST_CHAR(IFNAME, VAR_NAME)
          VAR_NAME = VAR_NAME(IFNAME:ILNAME)
          LEN_NAME = LENSTR(VAR_NAME)
          SAME_VAR = .FALSE.
          IF( VAR_READ(1:LEN_READ) .EQ.  VAR_NAME(1:LEN_NAME) )
     1       SAME_VAR = .TRUE.
C
C---      Check to see if the variable in the HEAD.ASC is an
C---      array.  It is possible that the reference to the variable
C---      reference in the INPUT.ASC file is just the array name, so
C---      add a "(" and compare the variables again.
          IF( .NOT. SAME_VAR ) THEN
            VAR_CHECK = VAR_NAME(1:LEN_NAME) // '('
            IF( VAR_READ(1:LEN_NAME+1) .EQ. VAR_CHECK(1:LEN_NAME+1) )
     1          SAME_VAR = .TRUE.
          ENDIF
          IF( SAME_VAR ) THEN
C!           
C!          IF( VAR_READ(1:LEN_READ) .EQ.  VAR_NAME(1:LEN_NAME) .OR.
C!     1        VAR_READ(1:LEN_NAME+1).EQ.
C!     2                VAR_NAME(1:LEN_NAME) // '(' ) THEN
            READ(ASTRING(5:8),'(I4)' ) LOC_IN_C
            IF( SAVECOMMENT .AND. LENSTR(ASTRING(26:80)) .GT. 0 ) THEN
              IFIRST = NXT_CHAR( 26, ASTRING )
              IF( ASTRING(IFIRST:IFIRST) .EQ. '!' ) THEN
                COMMENT = ASTRING(IFIRST:80)
              ELSE
                COMMENT = '! ' // ASTRING(IFIRST:80)
              ENDIF
CBC	        SAME_VAR2 = .FALSE.
            ELSE IF( SAVECOMMENT ) THEN
CBC	        IF( SAME_VAR2  .AND. SAME_VAR ) THEN
                  COMMENT = ' '
CBC	            SAME_VAR2 = .FALSE.
CBC	        ELSE
CBC	            SAME_VAR2=SAME_VAR
CBC                  SAME_VAR = .FALSE.
CBC	            GOTO 150
CBC	        ENDIF
            ENDIF
            FOUND = .TRUE.
C
C---        Check for a left paren in var_read, if one is found, then
C---        the variable is an array, determine the dimension.
            LPAR_READ = INDEX(VAR_READ,'(' )
            IF( LPAR_READ .GT. 0 ) THEN
C---          The variable in the HEAD.ASC is an array.  Determine the 
C---          end of the array dimension.  It could or could not have the ')'.
C
              IEND = INDEX( VAR_READ, ')'  ) - 1
              IF( IEND .LT. 0 ) IEND = LENSTR(VAR_READ)

              TEMP = VAR_READ(LPAR_READ+1:IEND)
C
C---          Check to see if the array is a two dimensional array.  If
C---          is, a comma will be found in the string.
              KOMMA = INDEX( TEMP, ',' )
              IF( KOMMA .GT. 0 ) THEN
                CALL READ_INTERNAL(ROW1,IERR,TEMP(1:KOMMA-1))
                CALL READ_INTERNAL(COL1,IERR,TEMP(KOMMA+1:LENSTR(TEMP)))
                IDIM = NINT( ROW1 * COL1 )
              ELSE
                CALL READ_INTERNAL(ROW1,IERR,TEMP(1:LENSTR(TEMP)))
                IDIM = NINT( ROW1 )
              ENDIF
              LOC_IN_C = LOC_IN_C - (IDIM - 1)
            END IF
C---        Check to see if the variable from INPUT.ASC is dimensioned or not.
C---        If it is dimensioned and the variable from the HEAD.ASC is,
C---        then the variable is refering to an element in the array. 
C---        Determine the c location.  If the variable from the INPUT.ASC
C---        is dimensioned and the variable from the HEAD.ASC is not,
C---        this is an invalid assigment statement.
            LPAR_NAME = INDEX(VAR_NAME,'(' )
            IF( LPAR_NAME .GT. 0 ) THEN
C---          Make sure if the variable in the INPUT.ASC file is a vector,               
C---          that the variable in the HEAD.ASC is a vector.
              IF( LPAR_READ .EQ. 0 ) THEN
                CALL ISSUE_ERR_MSG('UNDIMENSIONED VECTOR REFERENCED')     !**
                IERR = 1
                RETURN
              ENDIF
C
C---          The variable in the input file is a vector. Determine the
C---          end of the vector dimension. It should have the ')'.
C
              IEND = INDEX( VAR_NAME, ')'  ) - 1
              IF( IEND .LT. 0 ) THEN
                CALL ISSUE_ERR_MSG
     1            ('INVALID ASSIGNMENT STATMENT: CHECK PARENS')           !**
                IERR = 1
                RETURN
              ENDIF
              TEMP = VAR_READ(LPAR_NAME+1:IEND)
C
C---          Check to see if the array is a two dimensional array.  If
C---          is, a comma will be found in the string.
              KOMMA = INDEX( TEMP(1:IEND), ',' )
              IF( KOMMA .GT. 0 ) THEN
                CALL READ_INTERNAL(ROW2,IERR,TEMP(1:KOMMA-1))
                CALL READ_INTERNAL(COL2,IERR,TEMP(KOMMA+1:LENSTR(TEMP)))
                IF( ROW2 .GT. ROW1 .OR. COL2 .GT. COL1 ) THEN 
                  CALL ISSUE_ERR_MSG('INVALID VECTOR ELEMENT') 
                  IERR = 1                                                !**
                  RETURN
                ENDIF
              ELSE
                CALL READ_INTERNAL(ROW2,IERR,TEMP(1:LENSTR(TEMP)))
                COL2 = 1
                IF ( ROW2 .GT. ROW1 ) THEN
                  CALL ISSUE_ERR_MSG('INVALID VECTOR ELEMENT' )           !**
                  IERR = 1
                  RETURN
                ENDIF
              ENDIF
C              
C---          Determine the c location for the single vector element.
              IDIM = 1
              
              LOC_IN_C = LOC_IN_C - 1
C             
              DO J1 = 1, NINT(COL2)
                DO J2 = 1, NINT(ROW2)
                  LOC_IN_C = LOC_IN_C + 1
                ENDDO
              ENDDO
            END IF
          END IF
CBC     ELSE
CBC       IF( SAME_VAR2 .AND. .NOT. FOUND ) THEN
C             THERE IS NO OTHER DEFINITION FOR THIS VARIABLE; REWIND FILE
C             AND RE-READ FOR VARIABLE COMMENT AND CORRECT INFORMATION
CBC           EOFILE = .FALSE.
CBC           REWIND(NUHEAD)
CBC      GOTO 150
CBC       END IF
	  ENDIF
C
  150	 CONTINUE
      END DO
C
      CLOSE( NUHEAD )
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
      DO I = 1, LINE_LEN
        ASC_VAL = ICHAR(LINEOUT(I:I))
        IF( ASC_VAL .GE. 97 .AND. ASC_VAL .LE. 122 ) THEN
          LINEOUT(I:I) = CHAR( ASC_VAL - 32 )
        ENDIF
      ENDDO
C
      RETURN
      END
      SUBROUTINE SWEEP_DEGR_RADS( DEGR )
C
C----------------------------------------------------------------------
C
C     Determine if the angle for the sweep option is to be in 
C     degrees or radians
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      CHARACTER DEGR*2
C
      IDEGR = INDEX( WK_STRING, 'DEG' )
      IF( IDEGR .GT. 0 ) THEN
         DEGR = ' 0'
C---     Remove the DEGR keyword from the wk_string.
         DO I = IDEGR, LAST_CHAR(IDEGR,WK_STRING)
           WK_STRING(I:I) = ' '
         ENDDO
      ELSE
         DEGR = ' 1'
      ENDIF
C      
      RETURN
      END
      SUBROUTINE SWEEP_DELT( IERR, DELT, DELT_FOUND, DELT_STR)
C
C----------------------------------------------------------------------
C
C     This module determines the delta for the sweep range and angle
C     variables.
C
C----------------------------------------------------------------------
C      
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C      
      CHARACTER DELT_STR*5
C
      LOGICAL DELT_FOUND
C
      DELT_FOUND = .TRUE.
C
C---  Set the error flag.
      IERR = 0
C      
C---  Search for the DELT keyword.      
      IDELT = INDEX( WK_STRING, 'DELT' )
      IF( IDELT .EQ. 0 ) THEN
        DELT_FOUND = .FALSE.
        RETURN
      ENDIF
C
C---  Make sure the equal sign was included.
      IEQUAL = INDEX( WK_STRING(IDELT:LENSTR(WK_STRING)), '=' )            
      IF( IEQUAL .EQ. 0 ) THEN
        CALL ISSUE_ERR_MSG('ERROR READING DELTA' )                        !**
        IERR = 1
        RETURN
      ENDIF
C
C---  Determine the first and last position of the delta value.
      IFIRST = NXT_CHAR(IDELT+IEQUAL, WK_STRING)
      ILAST  = LAST_CHAR(IFIRST,WK_STRING)
C
C---  Save DELT as a string.      
      DELT_STR = WK_STRING(IFIRST:ILAST)
C
C---  Read the delta value.
      CALL READ_INTERNAL( DELT, IERR, WK_STRING(IFIRST:ILAST) )
      IF( IERR .GT. 0 ) THEN
        CALL ISSUE_ERR_MSG('ERROR READING DELTA')                         !**
        RETURN
      ENDIF
C
C---  Remove the DELT command from the working string.
      DO I = IDELT, ILAST
        WK_STRING(I:I) = ' '
      ENDDO
C
      RETURN
      END
      SUBROUTINE SWEEP_LIMITS
     1       (IERR,VAR_NAME,LOC_VAR,VAR_MIN,VAR_MAX,MX_FOUND)
C
C----------------------------------------------------------------------
C
C     This module determines the name of the range/angle/test variable, 
C     the c location for the range/angle/test variable, and the limits 
C     for the range/angle/test.  Note: at this point the only info in 
C     the working string should be like:   val1 < variable < val2
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      CHARACTER VAR_NAME*12, TEMP*132
      LOGICAL VAR_FOUND, MX_FOUND, SAVECOM
C
C---  Set the error flag.
      IERR = 0
C      
C---  Read the minimum value.
      ILESS = INDEX( WK_STRING, '<' )
      IF(ILESS .LE. 1) THEN
        CALL ISSUE_ERR_MSG('INVALID SWEEP LIMITS FORMAT')                 !**
        IERR = 1
        RETURN
      ENDIF
C      
      CALL READ_INTERNAL( VAR_MIN, IERR, WK_STRING(1:ILESS-1) )
      IF( IERR .GT. 0 ) THEN
        CALL ISSUE_ERR_MSG('ERROR READING SWEEP LIMITS')                  !**
        RETURN
      ENDIF
C
C---  Remove the minimum value and the < sign from the working string.
      TEMP = WK_STRING(NXT_CHAR(ILESS+1,WK_STRING):LENSTR(WK_STRING))
      WK_STRING = TEMP
C
C---  Read the maximum value.
      ILESS = INDEX( WK_STRING, '<' )
      IF( ILESS .EQ. 0 ) THEN
        MX_FOUND = .FALSE.
      ELSE
        MX_FOUND = .TRUE.
      ENDIF
C      
      IF( MX_FOUND ) THEN
C
        ILAST = LENSTR(WK_STRING)
        CALL READ_INTERNAL( VAR_MAX, IERR, WK_STRING(ILESS+1:ILAST) )     
        IF( IERR .GT. 0 ) THEN
          CALL ISSUE_ERR_MSG('ERROR READING SWEEP LIMITS')                !**
          RETURN                                                          
        ENDIF
C
C---    Remove the < sign and the maximum value from the working string.
        TEMP = WK_STRING(1:ILESS-1)
        WK_STRING = TEMP
C
      ENDIF
C
      VAR_NAME = WK_STRING(1:13)
      SAVECOM = .FALSE.
      CALL SEARCH_HEAD
     1   (IERR,VAR_NAME,LOC_VAR,VAR_FOUND,IDIM,SAVECOM)
      IF( IERR .GT. 0 )RETURN
C
      IF( .NOT. VAR_FOUND .OR. IDIM .GT. 1 ) THEN
        CALL ISSUE_ERR_MSG('INVALID VARIABLE IN SWEEP BLOCK' )            !**
        IERR = 1
        RETURN
      ENDIF
C
C---  If the variable is an array element, remove the parens (and comma).
      CALL CLEAN_VAR( VAR_NAME )
C
      RETURN
      END
      SUBROUTINE SWEEP_MODE( IMODE )
C
C----------------------------------------------------------------------
C
C     This module determines the sweep mode.
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C      
      IF     ( WK_STRING(1:5) .EQ. 'CONST' ) THEN
        IMODE = 0
      ELSE IF( WK_STRING(1:4) .EQ. 'DECR' ) THEN
        IMODE = 2
      ELSE IF( WK_STRING(1:9) .EQ. 'INCR/DECR' ) THEN
        IMODE = 3
      ELSE IF( WK_STRING(1:4) .EQ. 'INCR' ) THEN
        IMODE = 1
      ELSE IF( WK_STRING(1:3) .EQ. 'OUT' ) THEN
        IMODE = 4
      ELSE IF( WK_STRING(1:3) .EQ. 'ALL' ) THEN
        IMODE = 5
      ELSE 
        CALL ISSUE_ERR_MSG('INVALID SWEEP MODE' )
        IMODE = -1
      ENDIF    
C      
      RETURN
      END
      SUBROUTINE SWEEP_NUMBER( NUM_TRAJ_BIN )
C
C----------------------------------------------------------------------
C
C     This module determines number of trajectories or number of bins
C     depending on the mode.  
C
C----------------------------------------------------------------------
C
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER               INLINE*132, WK_STRING*132
C
      ILAST = LAST_CHAR(1, WK_STRING )
      CALL READ_INTERNAL( RVAL, IERR, WK_STRING(1:ILAST) )
C
      IF(IERR .GT. 0) THEN
        CALL ISSUE_ERR_MSG('INVALID SWEEP TRAJ/BIN NUMBER')               !**
      ELSE
        NUM_TRAJ_BIN = NINT( RVAL )                                       
      ENDIF
C     
      RETURN
      END
      SUBROUTINE WRITE_CARD1
C
C----------------------------------------------------------------------
C
C     This module places the type 1 cards in the output file CADIN.ASC.
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      WRITE(ICADIN,100)
  100 FORMAT('01 OUTPUT 2,3', T22, '0003'/
     1       '01 STAGE 2,3',  T22, '0004' )
C
      RETURN
      END
      SUBROUTINE WRITE_COMMENT(STRING)
C
C----------------------------------------------------------------------
C
C     This module places the comment read from the input file in the
C     CADIN.ASC file as a type 4 card.
C
C----------------------------------------------------------------------
C
      CHARACTER*(*) STRING
C
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C
      LEN_STRING = LENSTR( STRING )
      IF( LEN_STRING .GT. 1 ) 
     1WRITE(ICADIN,100) STRING(NXT_CHAR(2,STRING):LEN_STRING)
  100 FORMAT( '04', A )
C
      RETURN
      END
      SUBROUTINE WRITE_INTERNAL( R1, IERR, STRING )
C
C----------------------------------------------------------------------
C
C  This module simulates an unformatted write to a string.
C  This module was written to provide ease of transfer to the PC and
C  for modularity.
C
C----------------------------------------------------------------------
C
C  R1     - (R) Input.  The real number to be written to the string.
C  STRING - (C) OUTPUT.  The string that contains the real number
C                        passed in.
C
C----------------------------------------------------------------------
C
      CHARACTER*(*) STRING
      CHARACTER TEMPSTRING*30
C
C---  Set the error flag.
      IERR = 0
      LEN_STRING = LEN( STRING )
C
C---  Create a temp file
      KSCRATCH = 99
      OPEN(KSCRATCH, FORM='FORMATTED', STATUS='SCRATCH')
C
C---  Write the value to the temp file.  If the string to be written to is
C---  >= 10, then there is enough room to write the value scientifically.
      IF( LEN_STRING .LE. 10 ) THEN
        WRITE(KSCRATCH,'(F10.5)') R1
      ELSE
        WRITE(KSCRATCH,'(G10.4)') R1
      ENDIF
C
C---  Rewind the file and read the number as a string.
      REWIND( KSCRATCH )
      READ(KSCRATCH,'(A)') TEMPSTRING
      CLOSE( KSCRATCH )
C
C---  Clean up the string.
      INEXT = NXT_CHAR(1,TEMPSTRING)      ! Eliminate leading blanks
      ICHECK = INEXT + LEN_STRING         ! Last digit of number to be saved
C
C---  If the last character is a 0 or a blank, put a blank at the beginning
C---  of the string so that it is easier to read in the CADIN.ASC file.
      IF ( TEMPSTRING(ICHECK:ICHECK) .EQ. '0' .OR.
     1     TEMPSTRING(ICHECK:ICHECK) .EQ. ' ') THEN
        STRING = ' ' // TEMPSTRING( INEXT:INEXT+LEN_STRING-2 )
      ELSE
        STRING = TEMPSTRING(INEXT:INEXT+LEN_STRING-1)
      ENDIF
C
      RETURN
      END
      SUBROUTINE WRITE_INTERNAL2( R1, IERR, STRING )
C
C----------------------------------------------------------------------
C
C  This module simulates an unformatted write to a string.
C  This module was written to provide ease of transfer to the PC and
C  for modularity.
C
C----------------------------------------------------------------------
C
C  R1     - (R) Input.  The real number to be written to the string.
C  STRING - (C) OUTPUT.  The string that contains the real number
C                        passed in.
C
C----------------------------------------------------------------------
C
      CHARACTER*(*) STRING
      CHARACTER TEMPSTRING*30
C
C---  Set the error flag.
      IERR = 0
      LEN_STRING = LEN( STRING )
C
C---  Create a temp file
      KSCRATCH = 99
      OPEN(KSCRATCH, FORM='FORMATTED', STATUS='SCRATCH')
C
C---  Write the value to the temp file.  Need only 5 significant digits.
      IF( ABS(R1) .GT. 99999.0 ) THEN
        WRITE(KSCRATCH,'(E11.5)') R1
      ELSEIF( ABS(R1) .LE. 99999.0 .AND. ABS(R1) .GT. 9999.9 ) THEN
        WRITE(KSCRATCH,'(F7.0)') R1
      ELSEIF( ABS(R1) .LE. 9999.9 .AND. ABS(R1) .GT. 999.99 ) THEN
        WRITE(KSCRATCH,'(F7.1)') R1
      ELSEIF( ABS(R1) .LE. 999.99 .AND. ABS(R1) .GT. 99.999 ) THEN
        WRITE(KSCRATCH,'(F7.2)') R1
      ELSEIF( ABS(R1) .LE. 99.999 .AND. ABS(R1) .GT. 9.9999 ) THEN
        WRITE(KSCRATCH,'(F7.3)') R1
      ELSEIF( ABS(R1) .LE. 9.9999 .AND. ABS(R1) .GT. 0.99999 ) THEN
        WRITE(KSCRATCH,'(F7.4)') R1
      ELSEIF( ABS(R1) .LE. 0.99999 ) THEN
        WRITE(KSCRATCH,'(F7.5)') R1
      ENDIF
C
C---  Rewind the file and read the number as a string.
      REWIND( KSCRATCH )
      READ(KSCRATCH,'(A)') TEMPSTRING
      CLOSE( KSCRATCH )
C
C---  Clean up the string.
      INEXT = NXT_CHAR(1,TEMPSTRING)      ! Eliminate leading blanks
      ICHECK = INEXT + LEN_STRING         ! Last digit of number to be saved
C
C---  If the last character is a 0 or a blank, put a blank at the beginning
C---  of the string so that it is easier to read in the CADIN.ASC file.
      IF ( TEMPSTRING(ICHECK:ICHECK) .EQ. '0' .OR.
     1     TEMPSTRING(ICHECK:ICHECK) .EQ. ' ') THEN
        STRING = ' ' // TEMPSTRING( INEXT:INEXT+LEN_STRING-2 )
      ELSE
        STRING = TEMPSTRING(INEXT:INEXT+LEN_STRING-1)
      ENDIF
C
      RETURN
      END
      SUBROUTINE WRITE_NEWINPUT_WITH_COMMENT
C
C-----------------------------------------------------------------------
C
C     This module places the current INPUT.ASC line into the new CADIN.ASC
C     file if the card type was a type 3, type 7 or type 11.  If a commnet
C     was not included in the HEAD.ASC for the current variable, then the 
C     the comment from the INPUT.ASC is used.
C
C-----------------------------------------------------------------------
C      
      COMMON / ASSIGNMNT_COMM/ COMMENT
      CHARACTER                COMMENT*80
C      
      COMMON /FILE_IDS/ INPUT, NEWINPUT, ICADIN, IHEAD, NUHEAD, JERROR
C      
      COMMON / INPUT_STRING / INLINE,     WK_STRING
      CHARACTER                INLINE*132, WK_STRING*132  
C
      CHARACTER OUTLINE*132      
C
C---  Because the line is automatically written to the NEWINPUT file, if a      
C---  comment exists, we need to backup one line and rewrite the line
C---  with the new comment.  If a comment was included on the original
C---  INPUT.ASC file, it needs to be removed first.
      IF( LENSTR(COMMENT) .NE. 0 ) THEN       
        BACKSPACE( NEWINPUT )
        ICOMMENT = INDEX( INLINE, '!' ) 
        IF( ICOMMENT .GT. 0 ) THEN
          ILAST = LENSTR(INLINE(1:ICOMMENT-1))
        ELSE
          ILAST = LENSTR(INLINE)
        ENDIF  
C
C---    Write INLINE and COMMENT to the string OUTLINE so that
C---    we can place the comment in the same location, making sure that
C---    the space between the statement and the comment is blank.
        DO J = 1, 132        
          OUTLINE(J:J) = ' ' 
        ENDDO
        IF( ILAST .LT. 19 ) THEN
          ICOM = 20
        ELSE
          ICOM = ( INT(ILAST/5) + 1 ) * 5
C---      Make sure there is a space between the statement and the comment.          
          IF( ICOM .EQ. ILAST + 1 ) ICOM = ICOM + 5
        ENDIF
        OUTLINE(1:ILAST) = INLINE(1:ILAST)
        OUTLINE(ICOM:ICOM+LENSTR(COMMENT)-1)=COMMENT(1:LENSTR(COMMENT))
C        
        WRITE(NEWINPUT,500) OUTLINE(1:LENSTR(OUTLINE))
  500   FORMAT( A )
      ENDIF
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
C  THESTRING - (C) Input.  The character string to be searched for the
C                  end of the text string.
C  LENSTR - (I) The location of the last non-blank character in
C               THESTRING.  A 0 value is returned if the string is
C               completely blank
C----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
C
      LENGTH = LEN( THESTRING )
C
      DO WHILE ( LENGTH .GT. 0 .AND. THESTRING(LENGTH:LENGTH) .EQ. ' ' )
         LENGTH = LENGTH - 1
      END DO
C
      LENSTR = LENGTH
C      
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
C----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
C
      IPOSITION = IN_POS
      IF( THESTRING(IPOSITION:IPOSITION) .EQ. ' ' ) THEN
        DO WHILE ( THESTRING(IPOSITION:IPOSITION) .EQ. ' ' )
          IPOSITION = IPOSITION + 1
        ENDDO
      ENDIF
C
      NXT_CHAR = IPOSITION

      RETURN
      END
