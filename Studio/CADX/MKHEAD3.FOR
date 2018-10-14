      PROGRAM MKHEAD3
C
C----------------------------------------------------------------------
C
C     Program MKHEAD version 3.0                     June  2000
C     Program MKHEAD version 2.1                     March 2000
C     Program MKHEAD version 2.0                     May   1997
C     Program MKHEAD version 1.0                     March 1995
C
C----------------------------------------------------------------------
C
C     MKHEAD creates a HEAD.ASC file by pulling all of the EQUIVALENCED
C     parameter names form a MODULE.FOR file.   
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
C                  Voice: (850) 882-3722
C                  Fax:   (850) 882-9049
C
C---- Program History -------------------------------------------------
C
C      MKHEAD3    2000
C        - version number increase with release of new CADAC Studio
C      MKHEAD21   2000
C        - version number increased
C        - added the NOSWEEP option as default to header title record
C      MKHEAD2	1997
C        - version number increased
C      MKHEAD1	1995
C       - convert input string to all uppercase
C       - program EQSORT was renamed to MKHEAD
C       - the pause at the end of program execution was added to allow
C         the user to view the messages generated  
C       - the output file was renamed to HEAD.ASC
C       - the program was modified to include the first line of the 
C         HEAD.ASC
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INP_MOD, IOUT_HEAD, 
     1                  INAME, INAM, IDIM, IALP, IASK, IKC, ILC
C
      COMMON /VAR_INFO/ KOUT
C     
      INTEGER OUTPUTFILE
C      
      CHARACTER A*72, OUT_HEAD_NAME*60, MSG*80
      CHARACTER*1   ABET(20), ANS
      CHARACTER*100 BLK100, TEMPA
C
C---  These character variables replace the large arrays in the vax version.
      CHARACTER*1   ALPI, ASKI
      CHARACTER*13  DIMI, DIMJ, NAMI, NAMJ, NAMEI, NAMEJ
C
      LOGICAL DO_HEADFILE, EXIT_PROG
C
      DATA OUTPUTFILE / 0 / 
C        
      DATA ABET/'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
     +          'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T'/
C      
C---  Create a string of 100 blanks.
      DO I = 1, 100
	  BLK100(I:I) = ' '
      END DO
C         
C---  Initialize the file ids.
      CALL SET_FILE_IDS
C      
C---  Open the input files and prompt for the output file; open the indexed
C---  files and initialize them.
      CALL PREPARE_FILES                                  
C      
C
C---  Initialize variables.
      NC=0
      NDIM=0
      MAXKC=-99
C                
C---  Inform the user of the current status of the program.
      CALL CLEAR_SCREEN
      WRITE(*,'(/////10X,A)' ) 
     1   'Extracting DIMENSION variables and EQUIVALENCE variables'
C                                        
C---  Read a line from the module file.
   20 READ(INP_MOD,'(A)',END=90) A
C
C---  Check for a comment statement.
      IF( A(1:1) .EQ. 'C' .OR. A(1:1) .EQ. 'c' .OR.
     1    A(1:1) .EQ. '!' ) GOTO 20
C
 30   CONTINUE
C
      IF ( INDEX(A,'DIMENSION') .GT. 0 ) THEN
C
C---    A Dimension statement was found.
C                                                
C---    Find the beginning position (ie. the location after the keyword)
  	  TEMPA = A(INDEX(A,'DIMENSION')+9:LENSTR(A)) // BLK100
C
   40   IEND = LENSTR( TEMPA ) 
C
C---    Check for a comment at the end of the line.
	  IBEG = 1
C
C---    Eliminate leading blanks and commas.
	  DO WHILE( IBEG .LT. IEND .AND.
     1    (TEMPA(IBEG:IBEG) .EQ. ' ' .OR. TEMPA(IBEG:IBEG) .EQ. ',') )
	    IBEG = IBEG + 1
	  END DO
C
	  J = LENSTR( TEMPA )
C
C---    Check for a close paren.  This indicates the end of the variable
C---    in the dimension statement.
	  IF( J .EQ. 0 ) GOTO 55
	  IND = INDEX( TEMPA(1:J), ')' )
	  IF( IND .EQ. 0 ) GOTO 55
C
C---    Search to see if this variable has already be entered in the DIM
C---    array from a previous dimension statement.   
	  DO I = 1, NDIM
	    READ(IDIM,'(A13)',REC=I) DIMI
	    IF ( TEMPA(IBEG:IND) .EQ. DIMI ) GOTO 50
	  ENDDO
C
C---    Variable size not in DIM - add to the end of the list.
	  NDIM = NDIM + 1
	  DIMI = TEMPA(IBEG:IND)               ! DIM(NDIM)=TEMPA(IBEG:IND)
	  WRITE(IDIM,'(A13)',REC=NDIM) DIMI
C
C
 50     CONTINUE
C
C---    Remove this variable from the current dimension statement.
	  IF( (J-IND) .GT. 0.0 ) THEN
C
C---      Not end of variable list in current DIM statement.
C---      Remove the processed variable from the list.
	    IBEG = IND + 1
	    TEMPA = TEMPA( IBEG : J ) // BLK100
	    GOTO 40
C
	  ENDIF 
C
C---    End of variable list found.
C
	  J = 0
C
C
 55     CONTINUE
C
C---    End of current dimension statement.  Read the next line from the
C---    module program file.
C
	  READ(INP_MOD,'(A)',END=90) A
C
C         CONVERT TO ALL UPPER CASE
C
          CALL STR_UPCASE(A,A)
C
C---    Check for a comment statement.
	  IF ( A(1:1) .EQ. 'C' .OR. A(1:1) .EQ. 'c' .OR.
     1       A(1:1) .EQ. '!' ) GOTO 20
C
C---    Check for a continuation of the current dimension statement.
	  IF ( A(6:6) .NE. ' ' ) THEN
	    IF( J .GT. 0 ) THEN
	      TEMPA = TEMPA(1:J) // A(7:LENSTR(A)) // BLK100
	    ELSE
	      TEMPA = A(7:LENSTR(A))
	    ENDIF
	    GOTO 40  
  	  END IF
C
	  GOTO 30
C
C
      ELSE IF ( INDEX(A,'EQUIVALENCE') .GT. 0 ) THEN
C
C---    An Equivalence statement - Determine the C location:
C---        EQUIVALENCE (C(0051),SBEL(1))
C---        EQUIVALENCE (C(0052),MAER)
C
C---    Find the location after the second open paren.
	  IFRST = INDEX( A(1:72), '(' ) + 1
	  IBEG  = INDEX( A(IFRST:72), '(' ) + IFRST
C
C---    Eliminate leading blanks
	  IBEG  = NXT_CHAR( IBEG, A )
C
C---    Determine length of A.
	  J = LENSTR( A )      
C
C---    Check for a comment after the equivalence.
        JJ = INDEX( A, '!' )
        IF( JJ .GT. 0 ) J = JJ - 1	    
C
C---    Find the location prior to the close paren of the C location
C---    in the EQUIVALENCE.
	  IEND = INDEX( A(IBEG:J), ')' ) - 1 + IBEG - 1
C
C---    Save the C location in the KCI temp file.
	  NC = NC + 1
	  READ(A(IBEG:IEND),'(I4)') KCI
	  WRITE(IKC,'(I4)',REC=NC) KCI  
C
C---    Save the max C location.
	  MAXKC = MAX0( MAXKC, KCI )
C
C
C---    Determine the parameter name.
C
C---    Find the location of the comma in the equivalence statement.
	  IBEG = INDEX( A(1:J), ',' ) + 1
C
C---    Eliminate leading blanks
	  IBEG = NXT_CHAR( IBEG, A )
C
C---    Find the location of the close paren. If there are two, use the
C---    the second, the first is the array index of the variable.
	  IPAREN1 = INDEX( A(IBEG:J), ')' ) + IBEG - 1
	  IF( IPAREN1 .LT. J ) THEN
	    IPAREN2 = INDEX( A(IPAREN1+1:J), ')' ) + IPAREN1
	  ELSE
	    IPAREN2 = 0
	  ENDIF
C
	  IF( IPAREN2 .GT. 0 ) THEN
	   IEND = IPAREN2 - 1
	  ELSE
	   IEND = IPAREN1 - 1
	  ENDIF
C
C--     Determine length of variable name.
	  ILEN = LENSTR( A(IBEG:IEND) )
C
C---    Save the variable name to the name index file.        
	  WRITE(INAME,'(A)',REC=NC) A(IBEG:IBEG+ILEN-1)  ! NAME(NC)(1:13)=CHEF(1:13)
C
      END IF
C
      GOTO 20
C
C
   90 CONTINUE
C
      CLOSE (INP_MOD)
C
C---  Inform the user of the current status of the program.
      CALL CLEAR_SCREEN
      WRITE(*,'(/////15X,A)' ) 
     1        'Eliminating duplicate EQUIVALENCE variables'
C
C---  SORT ON C  LOCATION, & FLAG DIFFERENT NAMES ON SAME C LOCATION ***
C---  Place an asterick in the corresponding position of the ASK array
      KOUT = 0
      DO I = 1, NC
C
	  READ(IKC,'(I4)',REC=I) KCI
C
	  DO J = 1, KOUT
	    READ(ILC,'(I4)',REC=J) LCJ
	    IF ( KCI .EQ. LCJ ) GOTO 100
	  END DO
C
	  KOUT = KOUT + 1
C
C---    LC(KOUT) = KC(I)
	  WRITE(ILC,'(I4)',REC=KOUT) KCI
C
C---    NAM(KOUT) = NAME(I)
	  READ(INAME,'(A13)',REC=I) NAMEI
	  WRITE(INAM,'(A13)',REC=KOUT) NAMEI
C
	  DO J = I, NC
C
	    READ(IKC,'(I4)',REC=J) KCJ
C
	    IF ( KCI .EQ. KCJ ) THEN
C
	      READ(INAME,'(A13)',REC=J) NAMEJ
	      IF ( NAMEI .NE. NAMEJ ) THEN
C
C---          ASK(KOUT) = '*'
		       WRITE(IASK, '(A1)',REC=KOUT) '*'
C
	       	GOTO 100
C
	      END IF
C
	    END IF
C
	  END DO
C
 100    CONTINUE
C
      END DO
C
C---  Inform the user of the current status of the program.
      CALL CLEAR_SCREEN
      WRITE(*,'(/////12X,A)' ) 
     1     'Sorting the EQUIVALANCE variables in ascending order'
C
C---  Perform a SHELL sort the LC index file and the corresponding
C---  NAM and ASK index files.
      CALL SORT_C
C
C---  Inform the user of the current status of the program.
      CALL CLEAR_SCREEN
      WRITE(*,'(/////10X,A)' ) 
     1    'Checking for different C locations using same variable name'
C
C---  FIND MULTI OCCURANCE OF DIFFERENT C LOCATIONS USING SAME NAME ***
      JBET = 1
      DO I = 1, KOUT - 1
C
C---    Check to see if variable has already been flagged
C
	  READ(IALP,'(A1)',REC=I) ALPI
C
	  IF ( ALPI .EQ. ' ' ) THEN
C
	    READ(INAM,'(A13)',REC=I) NAMI
C
	    DO J = I+1, KOUT
C
	      READ(INAM,'(A13)',REC=J) NAMJ
C
	      IF ( NAMI .EQ. NAMJ ) THEN
C
C---                            ALP(I) = ABET(JBET)
		       IF(ALPI .EQ. ' ') WRITE(IALP,'(A1)',REC=I) ABET(JBET)
C
C---          ALP(J) = ABET(JBET)
		       WRITE(IALP,'(A1)',REC=J) ABET(JBET)
C
	      END IF
C
	    END DO
C
	    READ(IALP,'(A1)',REC=I) ALPI
C
	    IF ( ALPI .NE. ' ' ) JBET = JBET + 1
C
	    IF ( JBET .GE. 21 ) WRITE(*,190)
  190       FORMAT(' TOO MANY C LOCATIONS USE THE SAME VARIABLE NAME.'/
     1             ' USE HEAD.ASC TO FIX THOSE FLAGGED VARIABLES.'/
     2             ' RUN THIS PROGRAM AGAIN ON THE RESULT.' )
C
	  END IF
C
      END DO
C
 200  CONTINUE
C
C---  Inform the user of the current status of the program.
      CALL CLEAR_SCREEN
      WRITE(*,'(/////15X,A)' ) 'Including dimensions for variables'
C
C--- Dimension array variables in the HEAD.ASC
      DO I = 1, KOUT
C
	  READ(INAM,'(A13)',REC=I) NAMI
C
	  READ(ILC,'(I4)',REC=I) LCI
C
	  IBEG = INDEX( NAMI, '(' )
	  IF( IBEG .GT. 0 ) THEN
C
	    DO J = 1, NDIM
C
	      READ(IDIM,'(A13)',REC=J) DIMJ
C
	      INDX = INDEX( DIMJ, '(' )
	      IF( NAMI(1:IBEG) .EQ. DIMJ(1:INDX) ) THEN
C
C---          Found a match in the dimension array
C
		       IMATCH = J
C
		       GOTO 300
C
	      ENDIF
C
	    ENDDO
C
C---      Normal completion of the do loop indicates no match
C
	    GOTO 375
C
 300      CONTINUE
C
C---      Check for mismatch in array dimensioning
	    READ(IDIM,'(A13)',REC=IMATCH) DIMJ
C
	    IND_NAM  = INDEX( NAMI(IBEG:13), ',' )
	    IND_DIM = INDEX( DIMJ(INDX:13), ',' )
C
	    IF( (IND_NAM .NE. 0 .AND. IND_DIM .EQ. 0) .OR.
     1        (IND_NAM .EQ. 0 .AND. IND_DIM .NE. 0) ) THEN
C
	      WRITE(*,340)
  340       FORMAT( ' SUBSCRIPT MISMATCH AS FOLLOWS: ' )
C
	      WRITE(*,350) LCI, NAMI
  350       FORMAT(' EQUIVALENCE (C(', I4, ' ), ', A13, ')' )
C
	      GOTO 375
C
	    ENDIF
C
C---      The array is dimensioned properly.
C
	    NAMI(IBEG:13) = DIMJ(INDX:13)
	    WRITE(INAM,'(A13)',REC=I) NAMI
C
C---      Check for overlap in memory locations
	    IF( NAMI(1:IBEG-1) .NE. 'KSEE' ) THEN
C
	      IF( I .EQ. KOUT ) THEN
C
C---          This is the last variable (highest C location)
C---          Check that it does not exceed C array size.
C
		       ITESTVAL = 3510
C
	      ELSE
C
C---          Check that the variable does not exceed the next
C---          defined C element number.
C
C---          ITESTVAL = LC( I + 1 )
		       ITESTVAL = 0
		       READ(ILC,'(I4)',REC=I+1,ERR=355) ITESTVAL
C
  355         CONTINUE
C
	      ENDIF
C
	      IF( IND_NAM .EQ. 0 ) THEN
C
C---          One dimensional array
		       IEND = INDEX( NAMI, ')' )
		       CALL READ_INTG( NAMI(IBEG+1:IEND-1), ID1, IERR )
C
		       IF ( (LCI+ID1-1) .GT. ITESTVAL ) THEN
		         GOTO 360
		       ELSE
		         GOTO 375
		       END IF
C
	      ELSE
C
C---          Two dimensional array
		       IEND = INDEX( NAMI, ',' )
		       CALL READ_INTG( NAMI(IBEG+1:IEND-1), ID1, IERR )
		       IBEG = IEND
		       IEND = INDEX( NAMI, ')' )
		       CALL READ_INTG( NAMI(IBEG+1:IEND-1), ID2, IERR )
C
		       IF( (LCI+ID1*ID2-1) .GT. ITESTVAL ) THEN
		         GOTO 360
		       ELSE
		         GOTO 375
		       ENDIF
C
	      ENDIF
C
 360        CONTINUE
C
	      IF( ITESTVAL .NE. 3510 ) THEN
		       WRITE(*,365) LCI, NAMI, ITESTVAL
 365          FORMAT(' C-LOCATION ', I4, ' FOR ', A13,
     +                ' OVERLAPS C-LOCATION ', I4 )
	      ELSE
		       WRITE(*,370) LCI, NAMI, ITESTVAL
 370          FORMAT(' C-LOCATION ', I4, ' FOR ', A13,
     +                ' EXCEEDS C ARRAY LOCATION ', I4 )
	      ENDIF
C
	    ENDIF
C
 375      CONTINUE
C
	  ENDIF
C
      ENDDO        
C
      CALL CLEAR_SCREEN
      WRITE(*,480)
 480  FORMAT(///// 5X,
     1    ' Do you wish to save the variables to a file? (Y or N) '/ 5X,
     2    ' Default = Y: ' \ )
      READ(*,'(A)') ANS 
C         
      IF( ANS .EQ. 'N' .OR. ANS .EQ. 'n' ) THEN
        DO_HEADFILE = .FALSE.
      ELSE
        DO_HEADFILE = .TRUE.
      ENDIF
C
      IF (DO_HEADFILE ) THEN 
C
C---    Set the parameters needed for prompting for the output HEAD.ASC file.
        OUT_HEAD_NAME = 'HEAD.ASC'   
        MSG = 'Enter name of file to contain variables (output)'  
C---    Prompt the user for the output HEAD.ASC file.
        CALL GET_FILE( OUT_HEAD_NAME, MSG, OUTPUTFILE, EXIT_PROG ) 
        IF( EXIT_PROG ) STOP ' '
C
        OPEN( IOUT_HEAD,  FILE=OUT_HEAD_NAME(1:LENSTR(OUT_HEAD_NAME)), 
     1      STATUS='UNKNOWN')    
C
	  WRITE(IOUT_HEAD,485)
 485    FORMAT( 1X, 'SCROLL NOECHOIN NOINTMSG NOSTGMSG NORANVAR ', 
     1              'NOTRAJBIN TRAJASC NOSTATBIN NOSTATASC NOTABOUT ',
     2              'NOSWEEP', /
     3          '*' )
      END IF
C
      WRITE(*,*) ' '
      WRITE(*,'(A,A,A)')
     1    ' POTENTIAL PROBLEMS IN ',  
     2      OUT_HEAD_NAME(1:LENSTR(OUT_HEAD_NAME)),
     3    ' ARE LISTED BELOW'
      WRITE(*,*) ' '
C
      DO I = 1, KOUT
C
	  READ(ILC,'(I4)',REC=I)   LCI
	  READ(IALP,'(A1)',REC=I)  ALPI
	  READ(IASK,'(A1)',REC=I)  ASKI
	  READ(INAM,'(A13)',REC=I) NAMI
C
	  IF ( DO_HEADFILE ) WRITE(IOUT_HEAD,600) LCI, ALPI, ASKI, NAMI
C
	  IF( ALPI(1:1) .NE. ' ' .OR. ASKI(1:1) .NE. ' ' )
     1    WRITE(*,600) LCI, ALPI, ASKI, NAMI
  600     FORMAT(4X,I4,1X,2A1,1X,A13)
C
      END DO
C          
      IF ( DO_HEADFILE ) CLOSE(IOUT_HEAD)
C
C---  Pause the program to allow the user to view the messages generated.
      WRITE(*,'(//15X,A\)') 'Press RETURN to continue ... '
      READ(*,'(A)') ANS
C      
      STOP
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
      CHARACTER FILE_MSG*80, FILE_NAME*60, INFILE*60, DIRPATH*80
C
      LOGICAL GOOD_FILE, EXIT_PROG
C      
      INTEGER*4 LENDIR, GETDRIVEDIRQQ
C                             
      INTEGER FILETYPE, OUTPUTFILE
      DATA OUTPUTFILE / 0 /      
C                        
      WRITE(*,'(5X, A)')'MKHEAD - Verison 3.0'
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
C
C-----------------------------------------------------------------------
C
C     This module prompts the user for input file names and opens the 
C     files.  In addition, it prompts for the name of the output file.
C
C-----------------------------------------------------------------------
C                                                  
      COMMON /FILE_IDS/ INP_MOD, IOUT_HEAD, 
     1                  INAME, INAM, IDIM, IALP, IASK, IKC, ILC
C
      CHARACTER INP_MOD_NAME*60, MSG*80      
      LOGICAL EXIT_PROG
C
      INTEGER INPUTFILE
C      
      CHARACTER  BLAN*13
      DATA       BLAN /'             '/
C          
      DATA INPUTFILE / 1 / 
C
C
C---  Set the parameters needed for prompting for the input file MODULE.FOR.
      INP_MOD_NAME = 'MODULE.FOR'
      MSG = 'Enter name of modules file (input)'
C---  Prompt for the input module name.
      CALL GET_FILE( INP_MOD_NAME, MSG, INPUTFILE, EXIT_PROG )
      IF( EXIT_PROG ) STOP ' '   
C
C---  Open the input file.
      OPEN( INP_MOD, FILE=INP_MOD_NAME(1:LENSTR(INP_MOD_NAME)), 
     1      STATUS='OLD', ERR=900)
C     
C---  Open the indexed scratch files.
      OPEN(INAME,ACCESS='DIRECT',RECL=13,FORM='FORMATTED',
     1     STATUS='SCRATCH')
      OPEN(INAM,ACCESS='DIRECT',RECL=13,FORM='FORMATTED',
     1     STATUS='SCRATCH')
      OPEN(IDIM,ACCESS='DIRECT',RECL=13,FORM='FORMATTED',
     1     STATUS='SCRATCH')
      OPEN(IASK,ACCESS='DIRECT',RECL=1,FORM='FORMATTED',
     1     STATUS='SCRATCH')
      OPEN(IALP,ACCESS='DIRECT',RECL=1,FORM='FORMATTED',
     1     STATUS='SCRATCH')
      OPEN(IKC,ACCESS='DIRECT',RECL=4,FORM='FORMATTED',STATUS='SCRATCH')
      OPEN(ILC,ACCESS='DIRECT',RECL=4,FORM='FORMATTED',STATUS='SCRATCH')
C
C---  Inform user the status of the program.
      CALL CLEAR_SCREEN
      WRITE(*,'(/////15X,A)' ) 'Initializing index files'
C      
C---  Initialize the indexed files.
      DO I = 1, 3000     
	  WRITE(INAM, '(A13)',REC=I) BLAN
	  WRITE(INAME,'(A13)',REC=I) BLAN
	  WRITE(IDIM, '(A13)',REC=I) BLAN
C
	  WRITE(IASK, '(A1)', REC=I) ' '
	  WRITE(IALP, '(A1)', REC=I) ' '
C
	  WRITE(ILC,'(I4)',REC=I) 0
	  WRITE(IKC,'(I4)',REC=I) 0
      END DO
C
      DO I = 3001, 10000    
	  WRITE(INAME,'(A13)',REC=I) BLAN
	  WRITE(IDIM, '(A13)',REC=I) BLAN
      END DO
C
      DO I = 10001, 20000
	  WRITE(INAM,'(A13)',REC=I) BLAN
      END DO
C
C
      RETURN  
C
  900 CONTINUE
      WRITE(*,*)'ERROR OCCURRED WHILE OPENING INPUT FILE ',INP_MOD_NAME 
      STOP ' '     
C
      END   
      SUBROUTINE SET_FILE_IDS
C
C----------------------------------------------------------------------
C
C     This module opens the scratch index files that are used in place
C     of the large arrays in the VAX version and initializes them.
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INP_MOD, IOUT_HEAD, 
     1                  INAME, INAM, IDIM, IALP, IASK, IKC, ILC
C
C---  Initialize the data file indeces.
      INP_MOD   = 21
      IOUT_HEAD = 22     
C      
C---  Initialize the file indeces for the scratch files.
      INAME = 31            !  These indexed files were all
      INAM  = 32            !  originally character arrays:
      IDIM  = 33            !  NAME(10000)*13, NAM(3000)*13,
      IALP  = 34            !  DIM(10000)*13, ALP(3000)*1,
      IASK  = 35            !  ASK(3000)*1
C
      IKC   = 36            !  These indexed files were originally
      ILC   = 37            !  integer arrays. KC(20000) & LC(3000)
C
C
      RETURN
      END
      SUBROUTINE READ_INTG( INSTRING, IOUTDAT, IERR )
C
C---------------------------------------------------------------------
C
C     This module reads an integer from a string.
C
C---------------------------------------------------------------------
C
C  STRING  - (Char) In.  The string that the integer is to be read from.
C  IOUTDAT - (I) In/Out.  If the user responds to the prompt with an
C            integer input, the value is returned in this variable.  If
C            the user enters <RETURN> with no data, INDAT is not modified
C            preserving the default value.
C  IERR    - (I) Output.  An error indicator flag.  IERR is set to 0 at
C            the beginning of the module.  If an error occurs during the
C            internal read, IERR is set to 1.
C
C-----------------------------------------------------------------------
C
      CHARACTER*(*) INSTRING
C
      ILEN = LENSTR(INSTRING)
      DO WHILE ( INDEX(INSTRING(1:1),' ') .NE. 0 )
	  INSTRING = INSTRING(2:ILEN)
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
      SUBROUTINE SORT_C
C
C----------------------------------------------------------------------
C
C     This module performs a SHELL sort on the values in an index file.
C     The SORT is based on the algorithm found on page 323 in Numerical
C     Methods.
C
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INP_MOD, IOUT_HEAD, 
     1                  INAME, INAM, IDIM, IALP, IASK, IKC, ILC
C
      COMMON /VAR_INFO/ KOUT
C
C---  These character variables replace the large arrays in the vax version.
      CHARACTER*1   ASK1, ASK2
      CHARACTER*13  NAM1, NAM2
C
C---  Determine starting increment.
      INC = 1
  100 INC = 3*INC + 1
C
      IF( INC .LE. KOUT )  GOTO 100
C
C---  Loop over the partial sorts.
  200 CONTINUE
C
      INC = INC / 3
C
C---  Outer loop of straight insertion.
      DO I = INC+1, KOUT
C
	  READ(ILC, '(I4)', REC=I) LC1
	  READ(IASK,'(A1)', REC=I) ASK1
	  READ(INAM,'(A13)',REC=I) NAM1
C
	  J = I
C
  300   CONTINUE
C
	  READ(ILC, '(I4)', REC=J-INC) LC2
	  READ(IASK,'(A1)', REC=J-INC) ASK2
	  READ(INAM,'(A13)',REC=J-INC) NAM2
C
C---    Inner loop of straight insertion.
	  IF( LC2 .GT. LC1 ) THEN
C
C---      Switch data for LC, ASK and NAM.
	    WRITE(ILC, '(I4)', REC=J) LC2
	    WRITE(IASK,'(A1)', REC=J) ASK2
	    WRITE(INAM,'(A13)',REC=J) NAM2
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
C---    Switch data for LC, ASK and NAM.
  400   WRITE(ILC, '(I4)', REC=J) LC1
	  WRITE(IASK,'(A1)', REC=J) ASK1
	  WRITE(INAM,'(A13)',REC=J) NAM1
C
      ENDDO
C
      IF( INC .GT. 1 ) GOTO 200
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
      FUNCTION LENSTR( THESTRING )
C
C--------------------------------------------------------------------
C     This function searches the text contained in the string variable  
C     for the end of the text.  The function returns the character  
C     location of the last non-blank character location.  This is useful 
C     in locating the end of text within a string.  The module will work 
C     with any length input string.
C
C----------------------------------------------------------------------
C  THESTRING - (C) Input. The character string to be searched for the 
C                  end of the text string.
C  LENSTR    - (I) The location of the last non-blank character in 
C                  THESTRING. A 0 value is returned if the string is 
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
C      
      RETURN
      END
      FUNCTION NXT_CHAR(IN_POS, THESTRING)
C
C--------------------------------------------------------------------
C
C     This function returns the location of the next non-blank
C     character in a string.  The module will work with any length
C     input string.
C
C----------------------------------------------------------------------
C
C  THESTRING - (C) Input.  The character string to be searched for the
C                  next non-blank character.
C
C  LOC_FRST  - (I) The location of the next non-blank character in
C                  THESTRING.
C
C----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
C
      IPOSITION = IN_POS  
C      
      IF( THESTRING(IPOSITION:IPOSITION) .EQ. ' ' ) THEN
	  DO WHILE ( THESTRING(IPOSITION:IPOSITION) .EQ. ' ' )
	    IPOSITION = IPOSITION + 1
	  ENDDO
      ENDIF
C
      NXT_CHAR = IPOSITION
C
      RETURN
      END
