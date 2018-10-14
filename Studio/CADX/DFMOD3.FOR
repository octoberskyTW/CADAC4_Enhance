      PROGRAM DFMOD3
C
C-----------------------------------------------------------------------
C
C     DFMOD Version 3.0                                       June  2000
C     DFMOD Version 2.0                                       May   1997
C     DFMOD Version 1.0                                       March 1995
C
C-----------------------------------------------------------------------
C
C     This program places the variable definitions found in the HEAD.ASC
C     file into the MODULE.FOR file.
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
C                  Eglin Air Force Base, Florida
C                                       32542-6817
C
C                  Voice: (850) 882-8195
C                  Fax:   (850) 882-9049
C
C---- Program History --------------------------------------------------
C
C      DFMOD3 2000
C        - version number increase with release of new CADAC Studio
C      DFMOD2	1997
C        - version number increased
C      DFMOD1	1995
C       - program MHED was renamed DFMOD
C       - program was modified to reflect new input/output file names
C       - the prompts were modified to be consistent with all the
C         CADAC utility programs
C-----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INP_MOD, INP_HEAD, IOUT_MOD, IND_TEMP, IND_NAME
C      
      CHARACTER NAME*68, RECRD*80, TEMP*80
C
      LOGICAL  ASTERIK, EOFILE, PHRASE_FOUND, CHK_FOR_PHRASE

C
C
C---  Initialize the unit numbers for the files.
      CALL SET_FILE_IDS 
C
C
C---  Prompt for the HEAD.ASC and MODULE.FOR name.
      CALL PREPARE_FILES
C
C
C---  Read the HEAD.ASC file and determine which parameter names
C---  have descriptions associated with them.  Place these parameter
C---  names and descriptions in the NAME indexed file.
C
C---  Read to the first * in field one to start searching for
C---  C array locations.
      ASTERIK = .FALSE.
      DO WHILE( .NOT. ASTERIK )
        READ( INP_HEAD, '(A)', END= 999 ) RECRD
        IF( RECRD(1:1) .EQ. '*' ) ASTERIK = .TRUE.
      END DO
C
      INAME = 0
C
      EOFILE = .FALSE.
      DO WHILE( .NOT. EOFILE )
C
        EOFILE = .TRUE.
        READ( INP_HEAD, '(A80)', END=320 ) RECRD
        EOFILE = .FALSE.
C
  320   CONTINUE
C
C
        IF(.NOT. EOFILE .AND. RECRD(1:1) .NE. '*' .AND.
     1                        LENSTR(RECRD(26:80)) .NE. 0) THEN
C
C---      The record is not a comment and it has a parameter description.
          INAME = INAME + 1
C
          IPARN = INDEX ( RECRD(13:25), '(' )
          IF( IPARN .EQ. 0 ) THEN
            NAME(1:68)  = RECRD(13:80)
          ELSE
            NAME(1:13)  = RECRD(13:13+IPARN-2)
            NAME(14:68) = RECRD(26:80)
          END IF
C
          WRITE(IND_NAME,'(A68)',REC=INAME) NAME
C
        ENDIF
C
      ENDDO
C
C
C---  Only continue execution if the HEAD.ASC file contained descriptions.
      IF( INAME .EQ. 0 ) STOP
C
C
C---  Read the original MODULE.FOR file searching for the "INPUT FROM"
C---  key phrase.  Once found define the parameters in the equivalence 
C---  statements associated with the key phrase when possible using 
C---  a match found in the NAME array.
 150  CONTINUE
      READ(INP_MOD,'(A)',END=999)RECRD      
      IEND = LENSTR(RECRD)
      IF( IEND .LT. 1 ) IEND = 1
C
      WRITE(IOUT_MOD,'(A)') RECRD(1:IEND)
      IF ( RECRD(1:1) .EQ. 'C' ) THEN  
C
C---    Search for the phrase "INPUT FROM OTHER"
        PHRASE_FOUND = CHK_FOR_PHRASE( RECRD )
        IF( .NOT. PHRASE_FOUND ) GOTO 150
C                                                    
C---   The key phrase has been found
 175    CONTINUE
C
        READ(INP_MOD,'(A)',END=999)RECRD
        IEND = LENSTR( RECRD)
        IF( IEND .LT. 1 ) IEND = 1
C
        WRITE(IOUT_MOD,'(A)')RECRD(1:IEND)
        IEQSTR = INDEX( RECRD, 'EQUIVALENCE')
        IF ( IEQSTR .EQ. 0 ) GOTO 175
C
C---    An equivalence statement following the key phrase has been found
        ITEMP = 0
C
 180    CONTINUE
C
C---   Strip the parameter name from the equivalence statement then
C---   compare it to the NAME array.  If a match is found place the
C---   parameter name and its description in the TEMP array
        IBEG = INDEX( RECRD, ',' ) + 1
 181    CONTINUE
C
C---   Eliminate leading blanks
        IF ( RECRD(IBEG:IBEG) .EQ. ' ' ) THEN
          IBEG = IBEG + 1
          GOTO 181
        END IF 
        IEND = IBEG
C
 182    CONTINUE
C
        IF ( RECRD(IEND:IEND) .EQ. ' ' .OR.
     +       RECRD(IEND:IEND) .EQ. '(' .OR.
     +       RECRD(IEND:IEND) .EQ. ')'     ) THEN
          IEND = IEND - 1
          GOTO 183
        ELSE
          IEND = IEND + 1
          GOTO 182
        END IF
C
 183    CONTINUE
C
C---   The parameter name has been stripped, do a comparison to
C---   the NAME array and place the descriptor in the TEMP array.
        DO I = 1, INAME
          READ(IND_NAME,'(A68)',REC=I) NAME
          IF ( NAME(1:13) .EQ. RECRD(IBEG:IEND) ) THEN
            ITEMP = ITEMP + 1                     
            LNAME = LENSTR( NAME )
            IF( LNAME .LT. 14) LNAME = 14
            TEMP = 'C ' //  RECRD(IBEG:IEND) // '= ' // NAME(14:LNAME)
C            TEMP(1:2) = 'C ' 
C            TEMP(3:3+(IEND-IBEG))= RECRD(IBEG:IEND)
C            TEMP( 4+(IEND-IBEG):4+(IEND-IBEG) ) = '='
C            TEMP( 5+(IEND-IBEG):80)= ' ' // NAME(14:72) // '        '
            WRITE(IND_TEMP,'(A80)',REC=ITEMP) TEMP
            GOTO 184
          END IF
        END DO
C
 184    CONTINUE
C
C---    Read until the comment following the equivalence block is found
        READ(INP_MOD,'(A)',END=999)RECRD
C
        IEND = LENSTR(RECRD)
        IF( IEND .LT. 1 ) IEND = 1
C
        WRITE(IOUT_MOD,'(A)')RECRD(1:IEND)
        IF ( RECRD(1:1) .EQ. 'C' ) THEN
C
C---      Remove previously placed parameter descriptions
  195     CONTINUE
          READ(INP_MOD,'(A)',END=999)RECRD
          IF ( RECRD(1:1) .EQ. 'C' ) THEN
C
C---        Look for a key phrase of '= '
            IKEY = INDEX(RECRD,'= ')
C
C---        Read again if the key phrase is found or 
C---        there is blank comment line: ie, length of recrd = 1.
            IF ( IKEY .NE. 0 .OR. LENSTR(RECRD) .EQ. 1 ) GOTO 195
C
          END IF
C
C---      Write the comment block in TEMP array to the MODULE.FOR file
          DO I = 1, ITEMP
            READ(IND_TEMP,'(A80)',REC=I) TEMP
            WRITE(IOUT_MOD,'(A)') TEMP(1:LENSTR(TEMP))
          END DO
C
C---      Write a comment line to seperate the comment block in TEMP
          IF ( ITEMP .NE. 0 ) WRITE(IOUT_MOD,'(A)')'C'
C
C---      Write out the first line following the old comment block
C---      after the new comment block has been placed.
          IEND = LENSTR( RECRD )
          IF ( IEND .LT. 1 ) IEND = 1
          WRITE(IOUT_MOD,'(A)')RECRD(1:IEND)
C
C----     Start the search for a new INPUT FROM block
          GOTO 150
C
        ELSE
C
          GOTO 180
C
        END IF
C
      ELSE 
C
        GOTO 150
C
      END IF
C
 999  CONTINUE
C
C---  Close the files and rename the module output file to the user
C---  selected file name.
      CALL CLOSE_FILES
      
C
      STOP  '   '
      END
      SUBROUTINE CLEAR_SCREEN
C-----------------------------------------------------------------------
C     This module clears the data from the terminal screen.
C-----------------------------------------------------------------------
C
CJH      PRINT*, CHAR(27) // '[2J'
C 
      RETURN
      END          
      SUBROUTINE CLOSE_FILES
C-----------------------------------------------------------------------
C     This module closes the files accessed by this program
C-----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INP_MOD, INP_HEAD, IOUT_MOD, IND_TEMP, IND_NAME
C      
      COMMON /OUTPUT_FILE/ OUT_MOD_NAME,    TEMP_MOD_NAME 
      CHARACTER            OUT_MOD_NAME*60, TEMP_MOD_NAME*60      
C
      LOGICAL*4 REN_FILE, RENAMEFILEQQ      
C      
      CLOSE(INP_MOD)
      CLOSE(INP_HEAD)
      CLOSE(IOUT_MOD)
C            
C---  Open a file using the user selected output file name with status=old.
C---  If the file exists, delete it so that the temp output file can be 
C---  renamed to the user selected output file name.      
      OPEN( IOUT_MOD, FILE=OUT_MOD_NAME(1:LENSTR(OUT_MOD_NAME)),      
     1      STATUS='OLD', ERR=200) 
      CLOSE(IOUT_MOD,STATUS='DELETE')
  200 CONTINUE           
C
      REN_FILE = RENAMEFILEQQ( TEMP_MOD_NAME(1:LENSTR(TEMP_MOD_NAME)),
     1                         OUT_MOD_NAME (1:LENSTR(OUT_MOD_NAME )) )       
C      
      CLOSE(IND_TEMP)
      CLOSE(IND_NAME)  
C
      RETURN
      END      
      SUBROUTINE GET_FILE( FILE_NAME, FILE_MSG, FILETYPE, EXIT_PROG )
C--------------------------------------------------------------------
C     This module prompts the user for the name of a file; If the file
C     is an input file, this module opens the file and verifies that it
C     exists.  If the file is an output file, the module verifies that 
C     it is a valid name.  If the filename is not valid, the user may 
C     enter a new file or exit the program.
C----------------------------------------------------------------------
C      
      CHARACTER FILE_MSG*80, FILE_NAME*60, INFILE*60, DIRPATH*80
C
      LOGICAL GOOD_FILE, EXIT_PROG
C      
      INTEGER FILETYPE, OUTPUTFILE
      INTEGER*4 LENDIR, GETDRIVEDIRQQ
C
      DATA OUTPUTFILE / 0 /      
C                        
      WRITE(*,'(5X, A)')'DFMOD - Verison 3.0'
C
      GOOD_FILE = .FALSE.    
      DO WHILE ( .NOT. GOOD_FILE )
C
	  CALL CLEAR_SCREEN
	  LENDIR = GETDRIVEDIRQQ( DIRPATH )  
C
	  WRITE(*,'(//5X,A/5X,A//5X,A,/ 5X,A,A,A \)')
     1      'Current Directory is :', DIRPATH, 	  
     1  	   FILE_MSG(1:LENSTR(FILE_MSG)),
     1	     'Default = ', FILE_NAME(1:LENSTR(FILE_NAME)), ' : '
C     
	  READ(*,'(A)') INFILE
	  IF( LENSTR(INFILE) .EQ. 0 ) INFILE = FILE_NAME
C
	  OPEN(3, FILE=INFILE(1:LENSTR(INFILE)), STATUS='OLD', ERR=10)
	  GOOD_FILE = .TRUE. 
	  CLOSE( 3 )  
C
   10   CONTINUE     
C             
        IF( .NOT. GOOD_FILE .AND. FILETYPE .EQ. OUTPUTFILE ) THEN
C
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
	  IF( .NOT. GOOD_FILE ) THEN 
C	  
	    WRITE(*,'(/5X,A//5X,A/5X,A\)' )
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
      SUBROUTINE PREPARE_FILES
C-----------------------------------------------------------------------
C     This module prompts the user for input file names and opens the 
C     files.  In addition, it prompts for the name of the output file.
C-----------------------------------------------------------------------
C                                                  
      COMMON /FILE_IDS/ INP_MOD, INP_HEAD, IOUT_MOD, IND_TEMP, IND_NAME
C     
      COMMON /OUTPUT_FILE/ OUT_MOD_NAME,    TEMP_MOD_NAME
      CHARACTER            OUT_MOD_NAME*60, TEMP_MOD_NAME*60
C      
C
      CHARACTER INP_HEAD_NAME*60, INP_MOD_NAME*60, MSG*80      
      LOGICAL EXIT_PROG
C
      INTEGER  INPUTFILE, OUTPUTFILE
      DATA INPUTFILE / 1 /, OUTPUTFILE / 0 /   
C
C---  Set the parameters needed for prompting for the input HEAD.ASC file.
      INP_HEAD_NAME = 'HEAD.ASC'   
      MSG = 'Enter name of file containing variables (input)'  
C---  Prompt the user for the input HEAD.ASC file.
      CALL GET_FILE( INP_HEAD_NAME, MSG, INPUTFILE, EXIT_PROG ) 
      IF( EXIT_PROG ) STOP ' '
C
C---  Set the parameters needed for prompting for the input file MODULE.FOR.
      INP_MOD_NAME = 'MODULE.FOR'
      MSG = 'Enter name of modules file (input)'
C---  Prompt for the input module name.
      CALL GET_FILE( INP_MOD_NAME, MSG, INPUTFILE, EXIT_PROG )
      IF( EXIT_PROG ) STOP ' '   
C
C---  Set the parameters needed for prompting for the output MODULE.FOR file.
      OUT_MOD_NAME = 'MODULE.FOR'
      MSG = 'Enter name of the output modules file (output)'
C---  Prompt the user for the output MODULE.FOR file.
      CALL GET_FILE(OUT_MOD_NAME, MSG, OUTPUTFILE, EXIT_PROG)
      IF( EXIT_PROG ) STOP ' '
C
C---  Now that we have the names of all the files, open the input files.  
C---  The input files do not remain open when determining the file names
C---  because it is possible to have an output file with the same name
C---  as an input file.  If the files remained open, an error would occur
C---  while trying to determine if the output file name was valid.
      OPEN( INP_HEAD, FILE=INP_HEAD_NAME(1:LENSTR(INP_HEAD_NAME)), 
     1      STATUS='OLD', ERR=900)
      OPEN( INP_MOD,  FILE=INP_MOD_NAME(1:LENSTR(INP_MOD_NAME)), 
     1      STATUS='OLD', ERR=910)    
C
C---  Open the MODULE output file with a temp name in case the output
C---  file has the same name as the input file.  After the definitions
C---  are added to the output file, the file is renamed to the user
C---  selected file name.  If the output file has the same name as the 
C---  the input file, the input file is deleted.  Check to see if the 
C---  output file name included a drive specification.  If it did, the
C---  temp file name needs to include the drive specification because
C---  the rename option doesn't work across devices.
      ICOLON = INDEX( OUT_MOD_NAME, ':')
      IF( ICOLON .GT. 0 ) THEN
        TEMP_MOD_NAME = OUT_MOD_NAME(ICOLON-1:ICOLON) // '\TEMP.MOD'
      ELSE
        TEMP_MOD_NAME = '\TEMP.MOD'
      ENDIF        
      OPEN( IOUT_MOD, FILE=TEMP_MOD_NAME(1:LENSTR(TEMP_MOD_NAME)),
     1      STATUS='UNKNOWN', ERR=920)
C     
C---  Open the indexed 'NAME' scratch file.
      OPEN(IND_NAME, ACCESS = 'DIRECT', RECL = 68,
     1               FORM = 'FORMATTED', STATUS = 'SCRATCH')
C
C---  Open the indexed 'TEMP' scratch file.
      OPEN(IND_TEMP, ACCESS = 'DIRECT', RECL = 80,
     1              FORM = 'FORMATTED', STATUS = 'SCRATCH')
C
C
      RETURN  
C
 900  CONTINUE
      WRITE(*,*)'ERROR OCCURRED WHILE OPENING INPUT FILE ',INP_HEAD_NAME 
      STOP ' '     
C
 910  CONTINUE
      WRITE(*,*)'ERROR OCCURRED WHILE OPENING INPUT FILE ',INP_MOD_NAME
      STOP ' '
C            
  920 CONTINUE
      WRITE(*,*)'ERROR OCCURRED WHILE OPENING OUTPUT FILE'
      STOP ' '
C      
      END   
      SUBROUTINE SET_FILE_IDS
C-----------------------------------------------------------------------
C     This module sets the file indeces for the files accessed by this
C     program
C-----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INP_MOD, INP_HEAD, IOUT_MOD, IND_TEMP, IND_NAME
C      
      INP_MOD  = 1           ! unit number for the module file
      INP_HEAD = 2           ! unit number for header.dat file
      IOUT_MOD = 3           ! unit number for output module file
C
      IND_TEMP = 26          ! unit # of indexed file containing module defs
      IND_NAME = 25          ! unit # of indexed file containing header defs
C
      RETURN
      END   
      LOGICAL FUNCTION CHK_FOR_PHRASE( ASTRING )
C--------------------------------------------------------------------
C     This module checks for the phrase 'INPUT FROM OTHER'
C--------------------------------------------------------------------
C
      CHARACTER*(*) ASTRING   
C
      JLEN = LENSTR( ASTRING )      
C
C---  Search for "INPUT"      
      JCHK1 = INDEX( ASTRING, 'INPUT ')           
      IF ( JCHK1 .EQ. 0 ) GOTO 200
C          
C---  Find the starting position of the word after "INPUT"          
      JCHK1 = NXT_CHAR( JCHK1+5, ASTRING )
      IF( JCHK1 .GE. JLEN ) GOTO 200
C
C---  Search for "FROM"; it should be located after "INPUT"      
      JCHK2 = INDEX( ASTRING(JCHK1:JLEN), 'FROM' ) 
      IF( JCHK2 .EQ. 0 ) GOTO 200                  
C
C---  Find the starting position of the word after "FROM"      
      JCHK1 = JCHK1 + (JCHK2 - 1)
      JCHK1 = NXT_CHAR( JCHK1+4, ASTRING )
      IF( JCHK1 .GE. JLEN ) GOTO 200 
C
C---  Search for "OTHER"; it should be located after "FROM"
      JCHK2 = INDEX( ASTRING(JCHK1:JLEN), 'OTHER' ) 
      IF( JCHK2 .EQ. 0 ) GOTO 200    
C
      CHK_FOR_PHRASE = .TRUE.    
      RETURN                
C    
C
  200 CONTINUE
      CHK_FOR_PHRASE = .FALSE.
C      
      RETURN
      END      
      FUNCTION LENSTR( THESTRING )            
C--------------------------------------------------------------------
C  This function searches the text contained in the string variable for 
C  the end of the text.  The function returns the character location 
C  of the last non-blank character location.  This is useful in 
C  locating the end of text within a string.  The module will work 
C  with any length input string.
C----------------------------------------------------------------------
C  THESTRING - (C) Input.  The character string to be searched for the 
C                  end of the text string.
C  LENSTR    - (I) The location of the last non-blank character in 
C                  THESTRING.  A 0 value is returned if the string is 
C                  completely blank
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
