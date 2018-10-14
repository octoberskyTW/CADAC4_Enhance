      PROGRAM AHEAD3
C----------------------------------------------------------------------
C
C     AHEAD Version 3.0                                     June  2000
C     AHEAD Version 2.0                                     May   1997
C     AHEAD Version 1.0                                     March 1995
C
C----------------------------------------------------------------------
C
C     This program organizes the variables in a head.asc file in
C     alphabetic order.
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
C
C--Program History------------------------------------------------------
C
C      AHEAD3     2000
C          - version number increase with release of updated CADAC Studio
C      AHEAD2		1997
C          - version number increased
C      AHEAD1		1995
C          - program ALFA was renamed AHEAD
C          - program was modified to reflect new input/output file names
C          - the prompts were modified to be consistent with all the
C               CADAC - PC utility programs
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INP_HEAD, IOUT_HEAD, IND_TMP
C
      COMMON /OUTPUT_FILE/ OUT_HEAD_NAME
      CHARACTER            OUT_HEAD_NAME*60
C
      CHARACTER ASTRING*80, COMMENT*65
C
      LOGICAL ASTERIK, EOFILE
C
C
C---  Initialize the unit numbers for the files.
      CALL SET_FILE_IDS  
C
C---  Prompt for the HEAD.ASC and MODULE.FOR name.
      CALL PREPARE_FILES    
C      
C---  Read to first * in field 1 to start search for C array locations.
      ASTERIK = .FALSE.
      DO WHILE( .NOT. ASTERIK )
	 READ(INP_HEAD,'(A80)') ASTRING
	 IF( ASTRING(1:1) .EQ. '*' ) ASTERIK = .TRUE.
      END DO
C
      NREC = 0
      EOFILE = .FALSE.
C
C---  Inform the user the status of the program.
      CALL CLEAR_SCREEN
      WRITE(*,'(/////10X, A)') 
     1  '* * * Reading HEAD.ASC file * * *'
C      
C---  Read the input file and place the definitions in the
C---  scratch indexed file.
      DO WHILE ( .NOT. EOFILE )
C
	  EOFILE = .TRUE.
	  READ(INP_HEAD,'(A80)',END=100) ASTRING
	  EOFILE = .FALSE.
C
  100   CONTINUE
C
	  IF( .NOT. EOFILE .AND. ASTRING(1:1) .NE. '*' ) THEN
	    NREC = NREC + 1
	    WRITE(IND_TMP,'(A80)',REC=NREC) ASTRING
	  END IF
C
      END DO   
C      
      CLOSE( INP_HEAD )
C
C---  Inform the user the status of the program.
      CALL CLEAR_SCREEN
      WRITE(*,'(/////10X, A)') '* * * Alphabetizing variables  * * *' 
C     
C---  Sort the data using a Shell sort
      CALL DO_SHELL_SORT( NREC )
C    
C---  Open the output file.
      OPEN( IOUT_HEAD,  FILE=OUT_HEAD_NAME(1:LENSTR(OUT_HEAD_NAME)), 
     1      STATUS='UNKNOWN')    
C      
C---  Inform the user the status of the program.
      CALL CLEAR_SCREEN
      WRITE(*,'(/////10X, A, A, A)') 
     1'* * * Creating ', OUT_HEAD_NAME(1:LENSTR(OUT_HEAD_NAME)),' * * *' 

C---  Copy the indexed file to a random access file, putting the
C---  random access file in the format that the vax version creates.
      DO I = 1, NREC
	  READ(IND_TMP,'(A80)',REC=I) ASTRING
	  COMMENT(1:65) = ASTRING(26:80)
	  IEND = LENSTR( COMMENT )
	  IF( IEND .GT. 0 ) THEN
	    IBEG = NXT_CHAR( 1, COMMENT )
	    WRITE(IOUT_HEAD,200) ASTRING(13:25), ASTRING(5:8),
     1                       COMMENT(IBEG:IEND)
  200     FORMAT( 1X, A, 1X, A, 1X, A )
	  ELSE
	    WRITE(IOUT_HEAD,300) ASTRING(13:25), ASTRING(5:8)
  300     FORMAT( 1X, A, 1X, A )
	  ENDIF
      END DO
C
      CLOSE( IOUT_HEAD )
      CLOSE( IND_TMP )
C
      STOP
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
      SUBROUTINE DO_SHELL_SORT( NREC )
C----------------------------------------------------------------------
C     This module performs a SHELL sort on the values in an index file.
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INP_HEAD, IOUT_HEAD, IND_TMP
C
      CHARACTER*80 ASTRING1, ASTRING2
C
C---  Determine starting increment.
      INC = 1
  100 INC = 3*INC + 1
C
      IF( INC .LE. NREC )  GOTO 100
C
C---  Loop over the partial sorts.
  200 CONTINUE
C
      INC = INC / 3
C
C---  Outer loop of straight insertion.
      DO I = INC+1, NREC
C
	 READ(IND_TMP, '(A80)', REC=I) ASTRING1
C
	 J = I
C
  300    CONTINUE
C
	 READ(IND_TMP, '(A80)', REC=J-INC) ASTRING2
C
C---     Inner loop of straight insertion.
	 IF( ASTRING2(13:25) .GT. ASTRING1(13:25) ) THEN
C
C---        Switch data for LC, ASK and NAM.
	    WRITE(IND_TMP, '(A80)', REC=J) ASTRING2
C
	    J = J - INC
C
	    IF( J .LE. INC ) THEN
	       GOTO 400
	    ELSE
	       GOTO 300
	    ENDIF
C
	 ENDIF
C
C---     Switch data for LC, ASK and NAM.
  400    WRITE(IND_TMP, '(A80)', REC=J) ASTRING1
C
      ENDDO
C
      IF( INC .GT. 1 ) GOTO 200
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
      WRITE(*,'(5X, A)')'AHEAD - Verison 3.0'
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
      COMMON /FILE_IDS/ INP_HEAD, IOUT_HEAD, IND_TMP
C     
      COMMON /OUTPUT_FILE/ OUT_HEAD_NAME
      CHARACTER            OUT_HEAD_NAME*60
C      
C
      CHARACTER INP_HEAD_NAME*60, MSG*80      
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
C---  Set the parameters needed for prompting for the output MODULE.FOR file.
      OUT_HEAD_NAME = 'AHEAD.ASC'
      MSG =
     1   'Enter name of file containing alphabetized variables (output)'
C---  Prompt the user for the output HEAD.ASC file.
      CALL GET_FILE(OUT_HEAD_NAME, MSG, OUTPUTFILE, EXIT_PROG)
      IF( EXIT_PROG ) STOP ' '
C
C---  Now that we have the names of all the files, open the input file.  
C---  The input file do not remain open when determining the file names
C---  because it is possible to have an output file with the same name
C---  as an input file.  If the file remained open, an error would occur
C---  while trying to determine if the output file name was valid.
      OPEN( INP_HEAD, FILE=INP_HEAD_NAME(1:LENSTR(INP_HEAD_NAME)), 
     1      STATUS='OLD', ERR=900)
C
C---  Open the indexed scratch file.
      OPEN(IND_TMP, ACCESS = 'DIRECT', RECL = 80, FORM = 'FORMATTED',
     1         STATUS = 'SCRATCH')
C

C
      RETURN  
C
 900  CONTINUE
      WRITE(*,*)'ERROR OCCURRED WHILE OPENING INPUT FILE ',INP_HEAD_NAME 
      STOP ' '     
C
      END   
      SUBROUTINE SET_FILE_IDS
C----------------------------------------------------------------------
C     This module sets the ids for the files being accessed by this
C     program. 
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INP_HEAD, IOUT_HEAD, IND_TMP  
C      
      INP_HEAD = 23
      IOUT_HEAD = 24
      IND_TMP = 25
C
      RETURN
      END      
      FUNCTION LENSTR( THESTRING )
C
C--------------------------------------------------------------------
C
C     This function searches the text contained in the string variable  
C     for the end of the text.  The function returns the character  
C     location of the last non-blank character location.  This is 
C     useful in locating the end of text within a string.  The module
C     will work with any length input string.
C
C----------------------------------------------------------------------
C
C  THESTRING - (C) Input.  The character string to be searched for the 
C                  end of the text string.
C
C  LENSTR - (I) The location of the last non-blank character in 
C               THESTRING.  A 0 value is returned if the string is 
C               completely blank
C
C----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
!
      LENGTH = LEN( THESTRING )
!
      DO WHILE ( LENGTH .GT. 0 .AND. THESTRING(LENGTH:LENGTH) .EQ. ' ' ) 
	 LENGTH = LENGTH - 1
      END DO
!
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
C  INSTRING  - (I) Input.  The position in the string to start looking
C                  for the next character.
C
C  THESTRING - (C) Input.  The character string to be searched for the
C                  next non-blank character.
C
C  NXT_CHAR -  (I) The location of the next non-blank character in
C                  THESTRING.
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
