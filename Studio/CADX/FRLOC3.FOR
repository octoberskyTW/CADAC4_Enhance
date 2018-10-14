      PROGRAM FRLOC3
C----------------------------------------------------------------------------
C
C     FRLOC Version 3.0                                  June  2000
C     FRLOC Version 2.0                                  May   1997
C     FRLOC Version 1.0                                  March 1995
C
C-----------------------------------------------------------------------------
C
C     Program CLOC determines the available C array locations from a 
C     HEAD.ASC file.
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
C--Program History------------------------------------------------------
C
C      FRLOC3 2000
C        - version number increase with release of new CADAC Studio
C      FRLOC2	1997
C        - version number increased
C      FRLOC1	1995
C        - program CLOC was renamed FRLOC
C        - the input and output file names were modified
C        - the prompts were modified to be consistent with all the
C          CADAC utility programs
C-----------------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INP_HEAD, IOUT_FRLOC
C      
      CHARACTER RECRD*80, TEMP*13
C      
      INTEGER C(3510)
C                      
C---  Initialize the unit numbers for the files.
      CALL SET_FILE_IDS
C
C---  Prompt for and open the input files; prompt for the output file name.
      CALL PREPARE_FILES
C      
C---  Initialize the C array for marking
      DO I = 1, 3510
        C(I) = 0
      END DO
C
C---  Read to the first * in field one to start searching for
C---  C array locations.
 30   CONTINUE
      READ(INP_HEAD,'(A)',END=200) RECRD
      IF ( RECRD(1:1) .NE. '*' ) GOTO 30
C
 40   CONTINUE
      READ(INP_HEAD,'(A)',END=100) RECRD   
C
CJH   ADDED SO THAT THE IC ARRAY WOULD NOT MESS THINGS UP
C
      IF ( RECRD(13:14) .EQ. 'IC' ) GOTO 40
C
C       
      IF ( RECRD(1:1) .EQ. '*' ) THEN
C
C---    The record is a comment, read the next record
        GOTO 40
C        
      ELSE
C
C---    Read a non-comment record from the HEADER.DAT and parse
C---    out the C location and possible array dimensions
C
C---    Read the starting location of a parameter into ILOC
        READ(RECRD(5:8),'(I4)')ILOC 
C        
C---    Determine whether the parameter is an array
        READ(RECRD(13:25),'(A13)')TEMP
        IPOS1 = INDEX( TEMP, '(' )
        IF ( IPOS1 .EQ. 0 ) THEN
C        
C---      The parameter is not an array mark the single location in
C---      the C array as used.
          C(ILOC) = 1
C
        ELSE
C
C---      The parameter is an array, determine its dimensions (1 or 2)
          IPOS2 = INDEX ( TEMP, ',' )
          IPOS3 = INDEX ( TEMP, ')' )
          IF ( IPOS3 .EQ. 0 ) THEN
C---        If the array dimension is incomplete then enter the 
C---        error code in the C array
            C(ILOC) = -1
            GOTO 40
          END IF
C
          IF ( IPOS2 .EQ. 0 ) THEN
C---        Mark the C locations for a 1-dimensional array
            CALL READ_INTG(TEMP(IPOS1+1:IPOS3-1),NUM,IERR)
            DO I = 0, NUM-1
              C(ILOC+I) = 1
            END DO
          ELSE
C---        Mark the C locations for a 2-dimensional array
            CALL READ_INTG(TEMP(IPOS1+1:IPOS2-1),NUM1,IERR)
            CALL READ_INTG(TEMP(IPOS2+1:IPOS3-1),NUM2,IERR)
            NUM = NUM1 * NUM2
            DO I = 0, NUM-1
              C(ILOC+I) = 1
            END DO
          END IF
C
        END IF
C
        GOTO 40
C
      END IF
C
 100  CONTINUE
C                                              
C---  Include a message in the output file.
      WRITE(IOUT_FRLOC,'(A)') ' Unused locations in array C(3510)'
C      
C---  Write to CLOC.DAT the empty C locations
      IBEG = 0
      I = 1
      DO WHILE ( I .LE. 3510 )
C
        IF ( C(I) .EQ. 0 ) THEN  
C        
          IF ( IBEG .EQ. 0 ) THEN
C---        Set up the beginning of an interval of empty C locations
            IBEG = I
            IEND = I-1
          END IF   
C          
          IF ( I .EQ. IEND+1 ) THEN
            IEND = I
          ELSE
C---        End of the interval write the information to the file
            IF ( (IBEG-IEND) .NE. 0 ) THEN
              WRITE(IOUT_FRLOC,105)IBEG, IEND
 105          FORMAT(I4, ' - ', I4)
            ELSE
              WRITE(IOUT_FRLOC,106)IBEG
 106          FORMAT(I4)
            END IF
            IBEG = 0
            I = I - 1
          END IF        
C          
        ELSE IF ( C(I) .EQ. 1 ) THEN
C        
C---      C location is full record the previous interval to the file
          IF ( IBEG .NE. 0 ) THEN
            IF ( (IBEG-IEND) .NE. 0 ) THEN
              WRITE(IOUT_FRLOC,105)IBEG, IEND
            ELSE
              WRITE(IOUT_FRLOC,106)IBEG
            END IF
            IBEG = 0
          END IF 
C
        ELSE IF ( C(I) .EQ. -1 ) THEN
C
C---      There is an error in an array dimension write interval and message
          IF ( IBEG .NE. 0 ) THEN
            IF ( (IBEG-IEND) .NE. 0 ) THEN
              WRITE(IOUT_FRLOC,105)IBEG, IEND
            ELSE
              WRITE(IOUT_FRLOC,106)IBEG
            END IF
            IBEG = 0
          END IF 
          WRITE(IOUT_FRLOC,110)I
  110     FORMAT(1X, 'INCOMPLETE ARRAY DIMENSION STARTING AT ',I4)

        END IF 
C
        I = I + 1
      END DO
C
C---  Write the final interval the file
      IF ( IBEG .NE. 0 ) THEN
        IF ( (IBEG-IEND) .NE. 0 ) THEN
          WRITE(IOUT_FRLOC,105)IBEG, IEND
        ELSE
          WRITE(IOUT_FRLOC,106)IBEG
        END IF
      END IF 
C
  200 CONTINUE
C
      CLOSE(INP_HEAD)
      CLOSE(IOUT_FRLOC)
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
      WRITE(*,'(5X, A)')'FRLOC - Verison 3.0'
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
      COMMON /FILE_IDS/    INP_HEAD, IOUT_FRLOC
C       
      COMMON /FILE_NAMES / INP_HEAD_NAME, OUT_FRLOC_NAME
      CHARACTER*60         INP_HEAD_NAME, OUT_FRLOC_NAME  
C
      CHARACTER MSG*80      
C
      LOGICAL EXIT_PROG
C
      INTEGER INPUTFILE, OUTPUTFILE
      DATA INPUTFILE / 1 /, OUTPUTFILE / 0 /      
C      
C---  Set the parameters needed for prompting for the input HEAD.ASC file.
      INP_HEAD_NAME = 'HEAD.ASC'   
      MSG = 'Enter name of file containing variables (input)'  
C---  Prompt the user for the input HEAD.ASC file.
      CALL GET_FILE( INP_HEAD_NAME, MSG, INPUTFILE, EXIT_PROG ) 
      IF( EXIT_PROG ) STOP ' '
C
C---  Set the parameters needed for prompting for the output FRLOC.ASC file.
      OUT_FRLOC_NAME = 'FRLOC.ASC'
      MSG='Enter name of file to contain C locations (output)'
C---  Prompt the user for the output HEAD.ASC file.
      CALL GET_FILE(OUT_FRLOC_NAME, MSG, OUTPUTFILE, EXIT_PROG)
      IF( EXIT_PROG ) STOP ' '
C
C---  Now that we have the names of all the files, open the input files.  
C---  The input files do not remain open when determining the file names
C---  because it is possible to have an output file with the same name
C---  as an input file.  If the files remained open, an error would occur
C---  while trying to determine if the output file name was valid.
      OPEN( INP_HEAD, FILE=INP_HEAD_NAME(1:LENSTR(INP_HEAD_NAME)), 
     1      STATUS='OLD', ERR=900)
      OPEN( IOUT_FRLOC,  FILE=OUT_FRLOC_NAME(1:LENSTR(OUT_FRLOC_NAME)), 
     1      STATUS='UNKNOWN', ERR=910)    
C
      RETURN  
C
 900  CONTINUE
      WRITE(*,*)'ERROR OCCURRED WHILE OPENING INPUT FILE ',
     1           INP_HEAD_NAME 
      STOP ' '     
C
 910  CONTINUE
      WRITE(*,*)'ERROR OCCURRED WHILE OPENING INPUT FILE ',
     1           OUT_FRLOC_NAME     
      STOP ' '
C      
      END
      SUBROUTINE READ_INTG( INSTRING, IOUTDAT, IERR )
C---------------------------------------------------------------------
C     This module reads an integer from a string.
C---------------------------------------------------------------------
C  STRING  - (Char) In.  The string that the integer is to be read from.
C
C  IOUTDAT - (I) In/Out.  If the user responds to the prompt with an
C            integer input, the value is returned in this variable.  If
C            the user enters <RETURN> with no data, INDAT is not modified
C            preserving the default value.
C
C  IERR    - (I) Output.  An error indicator flag.  IERR is set to 0 at
C            the beginning of the module.  If an error occurs during the
C            internal read, IERR is set to 1.
C-----------------------------------------------------------------------
C
      CHARACTER*(*) INSTRING
C
      ILEN = LENSTR(INSTRING)
      DO WHILE ( INDEX(INSTRING(1:1),' ') .NE. 0 )
C
         INSTRING = INSTRING(2:ILEN)
C
      END DO
C
      IERR = 1
      READ(INSTRING(1:LENSTR(INSTRING)),'(I5)',ERR=100) IOUTDAT 
      IERR = 0
C
  100 CONTINUE
C
      RETURN
      END
      SUBROUTINE SET_FILE_IDS
C---------------------------------------------------------------------
C     This module sets the indeces for the files used by this program.
C---------------------------------------------------------------------
C
      COMMON /FILE_IDS/   INP_HEAD, IOUT_FRLOC
C
      INP_HEAD = 21
      IOUT_FRLOC = 22
C
      RETURN
      END           
      FUNCTION LENSTR( THESTRING )
C--------------------------------------------------------------------
C     This function searches the text contained in the string variable  
C     for the end of the text.  The function returns the character  
C     location of the last non-blank character location.  This is useful
C     in locating the end of text within a string.  The module will work 
C     with any length input string.
C----------------------------------------------------------------------
C  THESTRING - (C) Input.  The character string to be searched for the 
C                  end of the text string.
C
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
      RETURN
      END