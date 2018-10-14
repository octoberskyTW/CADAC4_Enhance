      PROGRAM DFHEAD3
C
C----------------------------------------------------------------------
C
C     DFHEAD Version 3.0                                  June  2000
C     DFHEAD Version 2.4                                  June  2000
C     DFHEAD Version 2.0                                  March 1995
C     DFHEAD Version 1.0                                  March 1995
C
C-----------------------------------------------------------------------
C     This program is an aid for CADAC studies documentation.  It allows
C     the engineer to define variables, which reside in common locations,
C     using comment statements while encoding each module.  
C
C     DFHEAD uses such definitions as input and moves them to HEAD.ASC,
C     matching them with variable names and common locations already in 
C     place.  The results are written on a new version of HEAD.DAT.
C
C     The user must have two files of input - HEAD.ASC and MODULE.FOR
C     (or some fragment of MODULE.FOR).
C
C     The HEAD.ASC must be in the default directory.  It receives
C     definitions of variables which are located in any FORTRAN program,
C     ie., all CADAC modules or just one module.
C
C     The VAX version uses the following arrays for this program, but
C     the pc FORTRAN compiler decided these arrays were too huge, so
C     indexed files were used instead.
C
C     CHARACTER HEAD(3500)*80, MOD(6000)*80
C
C----------------------------------------------------------------------
C
C     Created by:  TYBRIN Corporation
C                  1030 Titan Court
C                  Ft. Walton Beach, Florida 32547
C
C                  Voice: (850) 337-2500
C                  Fax:   (850) 337-2634
C
C     Created for: AFRL/MNG
C                  Eglin Air Force Base, Florida 32542-6817
C
C                  Voice: (850) 882-3722
C                  Fax:   (850) 882-9049
C
C--Program History------------------------------------------------------
C
C      DFHEAD3  2000
C        - Added capability to add executive definitions to executive
C          variables found in MODULES
C        - Version number increased
C      DFHEAD2	1997
C        - version number increased
C      DFHEAD1	1995
C        - program DEFHEAD was rename DFHEAD
C        - the input and output file names were modified
C        - the prompts were modified to be consistent with all the
C          CADAC utility programs
C----------------------------------------------------------------------
C                                                       
      COMMON /FILE_IDS/ INP_MOD, INP_HEAD, IND_HEAD, IND_MOD, IOUT_HEAD
C      
      LOGICAL   EOFILE, MATCHED, SCROLLS
C
      CHARACTER ISCROL(20)*100, HEAD_STRING*80, MOD_STRING*80, 
     1          SCROLL_STRING*100
C
      COMMON /FILE_NAMES / INP_HEAD_NAME, INP_MOD_NAME, OUT_HEAD_NAME
      CHARACTER*60         INP_HEAD_NAME, INP_MOD_NAME, OUT_HEAD_NAME  
C
      LOGICAL VEXEC
C
C---  Initialize the unit numbers for the files.
      CALL SET_FILE_IDS
C
C---  Prompt for and open the input files; prompt for the output file name.
      CALL PREPARE_FILES
C
C---  Inform the user of the status of the program.
      CALL CLEAR_SCREEN
      WRITE(*,'(/////10X, A)') 
     1 '* * * Extracting variable definitions from module file * * *' 
C
C---  Place the comments in the module that contain only definitions
C---  of variables in the module indexed file.
      NMOD = 0
      EOFILE = .FALSE.
      DO WHILE ( .NOT. EOFILE  )
C
        EOFILE = .TRUE.
        READ(INP_MOD,'(A)',END=400) MOD_STRING
        EOFILE = .FALSE.
C
  400   CONTINUE
C
        IF( .NOT. EOFILE ) THEN
C        
C---      Make sure the line is a comment. Comment line defined if 
C         column 1 is either 'C' or 'c'
          IF(MOD_STRING(1:1) .EQ. 'C' .OR. MOD_STRING(1:1) .EQ. 'c')THEN
C
C---        Search for the string ' = '.
            IF( INDEX( MOD_STRING, ' = ' ) .GT. 0 ) THEN
              NMOD = NMOD + 1
              WRITE(IND_MOD,'(A80)',REC=NMOD) MOD_STRING
C             WRITE(*,'(1X, A)') MOD_STRING
            END IF              ! index(mod_string,' = ') .gt. 0
C
          END IF              ! mod_string(1:1) .eq. 'C'
C
        END IF              ! .not. eofile
C
      END DO              ! do while ( .not. eofile )
C
C
C---  Only continue executing the program if the module contained
C---  comments with variable definitions (ie., NMOD > 0 ).
      IF( NMOD .EQ. 0 ) THEN
C
        WRITE(*,450)
  450   FORMAT( // 1X, '* * * No variable definitions found * * *'/
     1             1X, '  * * * HEAD.ASC was NOT updated * * *' )
C
      ELSE
C
C
C---    Read past the scroll variables in the input HEAD.ASC.
        ISC = 0
        SCROLLS = .TRUE.
        EOFILE  = .FALSE.
        DO WHILE( SCROLLS .AND. .NOT. EOFILE )
C
          EOFILE = .TRUE.
          READ(INP_HEAD,'(A)',END=500) SCROLL_STRING
          EOFILE = .FALSE.
C
  500     CONTINUE
C
          IF( SCROLL_STRING(1:1) .NE. '*' ) THEN
C
C---        The line contains a scroll variable; save it in the ISCROL array.
            ISC = ISC + 1
            ISCROL(ISC) = SCROLL_STRING
C
          ELSE
C
C---        The line does not contain a scroll variable, but the first 
C---        non-scroll variable line needs to be saved in the ISCROL array.
C
            ISC = ISC + 1
            ISCROL(ISC) = SCROLL_STRING
C
            SCROLLS = .FALSE.
C
          END IF
C
        END DO      
C
C
        IF( .NOT. EOFILE ) THEN
C
C---      Inform the user the status of the program.
          CALL CLEAR_SCREEN
          WRITE(*,'(/////10X, A)')
     1    '* * * Reading variables from input varible file  * * *' 
        
C---      Read the header records that contain variable names
C---      and their locations in the indexed header file.
          NHEAD = 0
          EOFILE = .FALSE.
          DO WHILE( .NOT. EOFILE )
C
            EOFILE = .TRUE.
            READ(INP_HEAD,'(A)',END=600) HEAD_STRING
            EOFILE = .FALSE.
C
  600       CONTINUE
C
            IF(.NOT. EOFILE .AND. HEAD_STRING(1:1) .NE. '*')THEN
C
C---          The string contained a non-commented line.
              NHEAD = NHEAD + 1
              WRITE(IND_HEAD,'(A80)',REC=NHEAD) HEAD_STRING
C
            END IF
C
          END DO 
C          
C---      Inform the user the status of the program.
          CALL CLEAR_SCREEN
          WRITE(*,'(/////10X, A)')
     1     '* * * Matching variables with definitions  * * *' 
C
          DO I = 1, NHEAD
C           
            READ(IND_HEAD,'(A80)',REC=I) HEAD_STRING
C
            IEND = INDEX(HEAD_STRING(13:28), '    ') + 11
C
C           WRITE(*,620) I, IEND, HEAD_STRING(1:28)
C  620      FORMAT(1X, 'I = ', I3, 1X, 'IEND = ', I3, 1X, A )
C
C
C---        Check for executive variable and add executive definition
C           NOTE: If this variable is found in the MODULES with a 
C                 definition, the MODULES definition will be used in
C                 the new HEAD.ASC file.
            CALL CK_EXEC(HEAD_STRING, IEND, VEXEC)
C
            IF( IEND .GE. 12 ) THEN
C                                  
              J = 0
              MATCHED = .FALSE.
              DO WHILE( J .LT. NMOD .AND. .NOT. MATCHED )
C                                      
                J = J + 1
                READ(IND_MOD,'(A80)',REC=J) MOD_STRING
C
                JEND = INDEX( MOD_STRING, ' = ' ) - 1
C
C---            Find the location in the string of the last
C---            character in the variable name.
                LAST = LENSTR( MOD_STRING(1:JEND) )
C
                IF( LAST .GT. 1 ) THEN
C
C---            Find location of the first character of the variable name.
C
                IFIRST = FIRST_OF_LAST( MOD_STRING(1:LAST) )
                IF( IFIRST .LT. 2 ) IFIRST = 2
C
C                 WRITE(*,630) IEND, IFIRST, LAST
C  630            FORMAT( 1X, 'IEND, JST, LAST ', 3(I3) )
C                 WRITE(*,635)HEAD_STRING(13:IEND),MOD_STRING(IFIRST:LAST)
C  635            FORMAT(' |',A,'|',A,'| HEAD THEN MOD')
C
                  IF( (HEAD_STRING(13:IEND) .EQ.
     1                         MOD_STRING(IFIRST:LAST)) .AND. 
     2                         .NOT. VEXEC ) THEN
C
                    MATCHED = .TRUE.
C
C---                'Mate' definition (as found in module) with
C---                 variable (as found in input file head.asc).
C
                    IFIRST = INDEX( MOD_STRING, ' = ' ) + 3
                    LEN_MOD = LENSTR( MOD_STRING )
                    IF( (LEN_MOD-IFIRST+1) .LE. 80-25 ) THEN
                      ILAST = LEN_MOD
                    ELSE
                      ILAST = IFIRST + 80 - 25
                    ENDIF
C                   
                    HEAD_STRING(26:80) = MOD_STRING(IFIRST:ILAST)
                    WRITE(IND_HEAD,'(A80)',REC=I) HEAD_STRING
C
C                   WRITE(*,640) IFIRST, HEAD_STRING(13:IEND)
C  640              FORMAT( 1X, 'JST = ', I3, 1X, A )
C
                  ENDIF         ! head_string .eq. mod_string
C
                ENDIF         ! last .gt. 1
C
              ENDDO         ! do while j .lt. nmod .and. .not. MATCHED
C
              IF(VEXEC) WRITE(IND_HEAD,'(A80)',REC=I) HEAD_STRING
C
            ENDIF         ! if iend .ge. 12
C       
          ENDDO         ! do i = 1, nhead
C
C
C---      All definitions are matched up with names in header
C---      indexed file.
C
C---      Close the input files.
          CLOSE( INP_MOD )
          CLOSE( INP_HEAD )
C
C---      Try to open the output file to see if it exists.
C---      If it exists, close it with the delete option.
          OPEN (IOUT_HEAD,FILE=OUT_HEAD_NAME(1:LENSTR(OUT_HEAD_NAME)),
     1          STATUS='OLD',ERR=800 )
          CLOSE(IOUT_HEAD,STATUS='DELETE')
C
  800     CONTINUE
C
          OPEN(IOUT_HEAD,FILE=OUT_HEAD_NAME(1:LENSTR(OUT_HEAD_NAME)),
     1         STATUS='NEW' )
C
C---  Inform the user the status of the program.
      CALL CLEAR_SCREEN
      WRITE(*,'(/////10X, A)')
     1 '  * * * Creating file with defined variables  * * *' 
        
C---      Place the scroll records in the HEAD.ASC file.
          DO I = 1, ISC
            WRITE(IOUT_HEAD,'(A)') ISCROL(I)(1:LENSTR(ISCROL(I)))
          END DO
C 
C---      Place the variables with their definitions in the HEAD.ASC file.
          DO I = 1, NHEAD
            READ( IND_HEAD,'(A80)', REC=I) HEAD_STRING
            WRITE(IOUT_HEAD,'(A)') HEAD_STRING(1:LENSTR(HEAD_STRING))
C           WRITE(*,'(1X,A)') HEAD_STRING(1:LENSTR(HEAD_STRING))
          END DO
C
        ENDIF         ! .not. eofile 
C        
        CLOSE ( IOUT_HEAD ) 
        CLOSE ( IND_HEAD)
C
      ENDIF
C
      STOP ' '
      END    
      SUBROUTINE CK_EXEC(STRING, JEND, VEXEC)
C-----------------------------------------------------------------------
C
C     This subroutine checks for an executive variable on the 
C     string.  If a specified executive variable is found, the
C     appropriate definition is added to the string.
C
C---Variable defintion
C
C      E_DEF = Array of executive variable definitions to be used
C                in the HEAD.ASC file
C      EXECV   = Array of executive variables to be searched for
C                match
C      STRING  = Character string read from input HEAD.ASC
C      JEND    = Last character column of variable name located
C                within STRING
C      VEXEC   = Flag to determine if an executive variable was found
C
C-----------------------------------------------------------------------
C
      CHARACTER STRING*80, EXECV(100)*100, E_DEF(100)*250
C
      DATA ( EXECV(J), J=1,57) /'ERRVAL', 'ERRN', 'AERR', 'PRELOC', 
     1     'REARTH', 'CRAD','OPTMET', 'AGRAV', 'CFTM', 'CKFPS', 'AMU', 
     2     'WEII3', 'OPNORO', 'RANSEED', 'TRCOND', 'ISWEEP', 
     3     'CRITNO', 'CRTIVAL', 'SEARNO', 'NUMR', 'CRITMAX', 
     4     'ANGLNO', 'ANGMIN', 'ANGMAX', 'ANGDEL', 'ANGUNT', 
     5     'RANGNO', 'RANMIN', 'RANMAX', 'RANDEL', 'ANGX', 'RANG',
     6     'TIME', 'TSTAGE', 'PCNT', 'PPNT', 'IPLV', 'PPP', 
     7     'ITAP90', 'KSTPE', 'ITCNT', 'CPP', 'PGCNT', 'PMIN', 
     8     'NV', 'NJ', 'NOMOD', 'XMODNO', 'NOSUB', 'NIP', 'IPL', 
     9     'HMIN', 'HMAX', 'DER', 'V(101)', 'ICOOR', 'LCONV'/
C
      LOGICAL VEXEC
C
      VEXEC = .FALSE.
C
      CALL SET_DEF(E_DEF)
C
      DO I = 1,57
        ISTART = INDEX(STRING,TRIM(EXECV(I)))
        IF(ISTART .GT. 0 )THEN
          STRING(26:80) = TRIM(E_DEF(I))
          VEXEC = .TRUE.
        ENDIF
      END DO
C
      RETURN
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
      WRITE(*,'(5X, A)')'DFHEAD - Verison 3.0'
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
      COMMON /FILE_IDS/ INP_MOD, INP_HEAD, IND_HEAD, IND_MOD, IOUT_HEAD 
C       
      COMMON /FILE_NAMES / INP_HEAD_NAME, INP_MOD_NAME, OUT_HEAD_NAME
      CHARACTER*60         INP_HEAD_NAME, INP_MOD_NAME, OUT_HEAD_NAME  
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
      MSG = 'Enter name of file containing variables to define (input)'  
C
C---  Prompt the user for the input HEAD.ASC file.
      CALL GET_FILE( INP_HEAD_NAME, MSG, INPUTFILE, EXIT_PROG ) 
      IF( EXIT_PROG ) STOP ' '
C
C
C---  Set the parameters needed for prompting for the input file MODULE.FOR.
      INP_MOD_NAME = 'MODULE.FOR'
      MSG = 'Enter name of modules file (input)'
C
C---  Prompt for the CADAC module name.  This input file of FORTRAN
C---  code is expected to have some variables defined.  Each definition
C---  must have the string " = " (an equal with at least on blank on
C---  each side).  Only the string following the equal will be copied
C---  to the new HEAD.ASC.
      CALL GET_FILE( INP_MOD_NAME, MSG, INPUTFILE, EXIT_PROG )
      IF( EXIT_PROG ) STOP ' '   
C
C
C---  Set the parameters needed for prompting for the output HEAD.ASC file.
      OUT_HEAD_NAME = 'HEAD.ASC'
      MSG='Enter name of file to contain defined variables (output)'
C---  Prompt the user for the output HEAD.ASC file.
      CALL GET_FILE(OUT_HEAD_NAME, MSG, OUTPUTFILE, EXIT_PROG)
      IF( EXIT_PROG ) STOP ' '
C
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
C     
C---  Open the indexed 'HEAD' scratch file.
      OPEN(IND_HEAD,ACCESS='DIRECT',RECL=80,FORM='FORMATTED',
     1     STATUS='SCRATCH')
C
C---  Open the indexed 'MOD' scratch file.
      OPEN(IND_MOD,ACCESS='DIRECT',RECL=80,FORM='FORMATTED',
     1     STATUS='SCRATCH')
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
      END
      SUBROUTINE SET_DEF(E_DEF)
C----------------------------------------------------------------------
C
C     This subroutine loads the executive variable definition to be
C     used in the HEAD.ASC file.
C
C---Variables
C
C      E_DEF = Array of executive variable definitions to be used
C                in the HEAD.ASC file
C
C----------------------------------------------------------------------
C
      CHARACTER E_DEF(100)*250
C
      E_DEF(1)  = 'E Maximum Integration step error Value'            !ERRVAL
      E_DEF(2)  = 'E IPL location of variable causing ERRVAL'         !ERRN
      E_DEF(3)  = 'E C location of variable causing ERRVAL'           !AERR
      E_DEF(4)  = 'E'                                                 !PRELOC
      E_DEF(5)  = 'E Radius of the Earth = 6370987 - m'               !REARTH
      E_DEF(6)  = 'E Conversion factor = 57.29577951 (Deg/Rad)'       !CRAD
      E_DEF(7)  = 'E Units of measure: 1=metric; 0=English'           !OPTMET
      E_DEF(8)  = 'E Acceleration due to gravity @ sea level
     1 = 9.8066 m/s^2'                                                !AGRAV
      E_DEF(9)  = 'E Conversion factor = 0.3048006 (Meters/Feet)'     !CFTM
      E_DEF(10) = 'E Converstion factor = 1.6878 (Knots/(ft/s))'      !CKFPS
      E_DEF(11) = 'E Gravitational parameter =3.986007E+14 - m^3/s^2' !AMU
      E_DEF(12) = 'E Earth Angular rotation = 7.2921154E-5 (rad/s)'   !WEII3
      E_DEF(13) = 'E Option flag: 0=Rotating earth; 1=Non-rotating
     1 earth earthmodel'                                              !OPNORO
      E_DEF(14) = 'E Random function generator initialization'        !RANSEED
      E_DEF(15) = 'E Terminate Condition Codes from right to left'    !TRCOND
      E_DEF(16) = 'E (Sweep) Sweep option flag (0 through 5)'         !ISWEEP
      E_DEF(17) = 'E (Sweep) Critical variable C location'            !CRITNO
      E_DEF(18) = 'E (Sweep) Minimum test for critical variable'      !CRITVAL
      E_DEF(19) = 'E (Sweep) Number of binary search runs (opt 4, 5)' !SEARNO
      E_DEF(20) = 'E (Sweep) The number of trajectory runs'           !NUMR
      E_DEF(21) = 'E (Sweep) The maximum test for critical variable'  !CRITMAX
      E_DEF(22) = 'E (Sweep) The C location of the angluar variable'  !ANGLNO
      E_DEF(23) = 'E (Sweep) The minimum angle value'                 !ANGMIN
      E_DEF(24) = 'E (Sweep) The maximum angle value'                 !ANGMAX
      E_DEF(25) = 'E (Sweep) The Delta angle'                         !ANGDEL
      E_DEF(26) = 'E (Sweep) The units of the input data: rad or deg' !ANGUNT
      E_DEF(27) = 'E (Sweep) The C location of the range variable'    !RANGNO
      E_DEF(28) = 'E (Sweep) The minimum range value'                 !RANMIN
      E_DEF(29) = 'E (Sweep) The maximum range value'                 !RANMAX
      E_DEF(30) = 'E (Sweep) The delta range value'                   !RANDEL
      E_DEF(31) = 'E (Sweep) The polar angle from target'             !ANGX
      E_DEF(32) = 'E (Sweep) The range (distance) from target'        !RANG
      E_DEF(33) = 'E  Trajectory time - sec'                          !TIME
      E_DEF(34) = 'E Time in current stage - sec'                     !TSTAGE
      E_DEF(35) = 'E Time of the next print at TABOUT.ASC'            !PCNT
      E_DEF(36) = 'E Time of next print to the plot files.'           !PPNT
      E_DEF(37) = 'E The location of the state variable;
     1 corresponding to the derivative in the IPLV array'             !IPLV
      E_DEF(38) = 'E Time interval writing to TRAJ.BIN or TRAJ.ASC
     1 - sec'                                                         !PPP
      E_DEF(39) = 'E Flag:  0=No CSAVE.ASC; 1=trajectory started
     1 from data saved to CSAVE.ASC'                                  !ITAP90
      E_DEF(40) = 'E Controls flow after an integration step'         !KSTEP
      E_DEF(41) = 'E Print flag counter'                              !ITCNT
      E_DEF(42) = 'E Time interval writing to screen or TABOUT.ASC
     1 - sec'                                                         !CPP
      E_DEF(43) = 'E Page counter flag'                               !PGCNT
      E_DEF(44) = 'E Array of minimum plot variable values'           !PMIN
      E_DEF(45) = 'E The number of variables in the plot list.'       !NV
      E_DEF(46) = 'E Number of state variables to be integrated'      !NJ
      E_DEF(47) = 'E The number of modules to be called.'             !NOMOD
      E_DEF(48) = 'E The list of the module numbers to be called by
     1 the Exe, in the calling order.'                                !XMODNO
      E_DEF(49) = 'E The number of the ouput and auxiliary
     1 subroutines to be called by the Exe.'                          !NOSUB
      E_DEF(50) = 'E The number of variables being integrated.'       !NIP
      E_DEF(51) = 'E The locations of the derivative of the state
     1 variable.'                                                     !IPL
      E_DEF(52) = 'E  Not used'                                       !HMIN
      E_DEF(53) = 'E  Not used'                                       !HMAX
      E_DEF(54) = 'E Integration interval - sec'                      !DER
      E_DEF(55) = 'E '                                                !V(101)
      E_DEF(56) = 'E Flag=-1: Initialization, =0:Predictor, 
     1 =1:Corrector;'                                                 !ICOOR
      E_DEF(57) = 'E Flag indicating end of trajectory run'           !LCONV
C
      RETURN
      END
C
C----------------------------------------------------------------------
      SUBROUTINE SET_FILE_IDS
C----------------------------------------------------------------------
C     This module initializes the file ids used
C----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ INP_MOD, INP_HEAD, IND_HEAD, IND_MOD, IOUT_HEAD
C            
      INP_MOD   = 1      ! unit number for the module file
      INP_HEAD  = 2      ! unit number for the input HEAD.ASC file
C
      IND_HEAD  = 25     ! unit # of indexed file containing header defs
      IND_MOD   = 26     ! unit # of indexed file containing module defs
C
      IOUT_HEAD = 27     ! unit # for new HEAD.ASC file  
C
      RETURN
      END
      FUNCTION FIRST_OF_LAST( THESTRING )
C--------------------------------------------------------------------
C     This function searches THESTRING for the position of the first 
C     character of the last "word".
C----------------------------------------------------------------------
C     THESTRING     - (C) Input.  The string to be searched for the
C                     position of the last word of the text string.
C
C     FIRST_OF_LAST - (I) Location of the first character of the last
C                      word in THESTRING.  Zero is returned if the 
C                      string is blank.
C----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
C
      IFIRST = LENSTR ( THESTRING )
C
      DO WHILE(IFIRST .GT. 0 .AND.
     1        INDEX(THESTRING(IFIRST:IFIRST),' ') .EQ. 0)
        IFIRST = IFIRST - 1
      END DO
C
      FIRST_OF_LAST = IFIRST + 1
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
C     THESTRING - (C) Input.  The character string to be searched for
C                 the end of the text string.
C
C     LENSTR    - (I) The location of the last non-blank character in
C                 THESTRING.  A 0 value is returned if the string is
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
