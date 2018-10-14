      PROGRAM EQMAP3
C-------------------------------------------------------------------
C
C     EQMAP Version 3.0                                  June  2000
C     EQMAP Version 2.0                                  May   1997
C     EQMAP Version 1.0                                  March 1995
C
C-------------------------------------------------------------------
C
C     This program creates a cross listing of the variables found
C     in the HEAD.ASC with a MODULE.FOR.
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
C      EQMAP3	2000
C        - version number increase with release of new CADAC Studio
C      EQMAP2	1997
C        - version number increased
C      EQMAP1	1995
C        - the input and output file names were modified
C        - the prompts were modified to be consistent with all the
C          CADAC utility programs
C-----------------------------------------------------------------------
C                       EQMAP SYMBOLS
C                       -------------
C
C   SYMBOL       INDICATES:
C  ------------------------------------------------------------------------- 
C | [ ]        | the variable is an argument for the current module;        |
C |            | the name of the module is located within the brackets      |
C |-------------------------------------------------------------------------|
C |  =         | the variable is being assigned a value if the "=" is on    |
C |            | the left side of the subroutine name; the variable is      |
C |            | being used in the assignment of another variable if the    |
C |            | if the "=" is on the right side of the subroutine name     |                                   |
C |-------------------------------------------------------------------------|
C | C(Int Val) | the C location the variable is equivalanced to             |
C |-------------------------------------------------------------------------|
C | R          | the variable is a real                                     |
C |-------------------------------------------------------------------------|
C | I          | the variable is an integer                                 |
C |-------------------------------------------------------------------------|
C | DP         | the variable is a double precision real                    |
C |-------------------------------------------------------------------------|
C | (Int Val)  | the dimension of the variable                              |
C |-------------------------------------------------------------------------|
C | D          | the variable is in a data statement                        |
C |-------------------------------------------------------------------------|
C | >          | the variable is used in the criteria of an IF statement    |
C |-------------------------------------------------------------------------|
C | :          | the variable name was changed when it was the argument of  |
C |            | a subroutine; left side of ":" is name of subroutine and   |
C |            | right side of ":" is the new name of the variable          |
C |-------------------------------------------------------------------------|
C | ;          | the end of the information for the current module          |
C  -------------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      LOGICAL GOTO_END
C
C---  Prompt for the MODULE.FOR and the variable file to use and open
C---  the files.
      CALL PREPARE_FILES( GOTO_END )
      IF( GOTO_END ) GOTO 9999
C
      CALL GET_RANGE     
C      
      CALL SPLIT_MODULE
C
      CALL CREATE_MAP
C
      CALL DEL_FILES
C
 9999 STOP ' '
      END
      SUBROUTINE CHK_STRING
     1  ( WRITE_STRING, COMMON_FOUND, DOUBLE_FOUND, ILAST, THESTRING )
C-----------------------------------------------------------------------
C     Determine if the string is a IF..THEN, ELSE IF,  ENDIF, FORMAT,
C     END, CONTINUE, DO, WRITE or RETURN statement. If it is, return
C     and read the next line.  If it is simply an IF statement remove
C     the IF condition and treat the remainder of the string as if it
C     was read in.  Also, eliminate comments from the end of the string.
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      COMMON /FILE_IDS/ID_INMOD, ID_OUTMOD, ID_MOD(0:10), ID_OUT, ID_VAR
C
      LOGICAL WRITE_STRING, COMMON_FOUND, DOUBLE_FOUND
C
      CHARACTER*(*)  THESTRING
      CHARACTER*1650 IFSTRING    
C
      IFIRST = NXT_CHAR( 7, THESTRING )
C
C---  Check for an IF statement.
      ICHK = 2                      ! Length of word IF.
      IF( THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'IF ' .OR.
     1    THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'IF(' ) THEN
C
C---    The statement is an IF statement, see if it is an IF...THEN.
	  IF( THESTRING(ILAST-3:ILAST) .EQ. 'THEN' ) THEN
C
	    THESTRING = THESTRING(1:ILAST-4)
	    ILAST = ILAST - 4
	    WRITE_STRING = .TRUE.
	    RETURN
C
	  ELSE
C
C---      Move past the condition of the IF statement.  This is done by
C---      finding the same number of right parenthesis as left parenthesis.
	    NUM_LPAREN = 1
	    NUM_RPAREN = 0
	    LPAREN = INDEX( THESTRING, '(' )
	    I = LPAREN + 1
 100      DO WHILE( NUM_LPAREN .NE. NUM_RPAREN .AND. I .LT. ILAST  )
	      IF(      THESTRING(I:I) .EQ. '(' ) THEN
		       NUM_LPAREN = NUM_LPAREN + 1
	      ELSE IF( THESTRING(I:I) .EQ. ')' ) THEN
		       NUM_RPAREN = NUM_RPAREN + 1
		     ENDIF
		     I = I + 1
	    ENDDO
C
	    IF( I .EQ. ILAST ) THEN
C
C---        An invalid IF statement. Don't save it to the output file.
	      WRITE_STRING = .FALSE.
	      RETURN
C
	    ENDIF
C
	  ENDIF
C
C---    Write the IF statement to the file and then set
C---    THESTRING to the part of the statement following the
C---    condition of the IF statement, padding the front with
C---    six spaces so that the statement starts in the 7th column.
	  IFSTRING = THESTRING( NXT_CHAR(7,THESTRING) : I-1 )
	  IFLEN = LENSTR( IFSTRING )
	  CALL WRITE_OUTMOD( IFLEN, IFSTRING(1:IFLEN))
C
	  IFSTRING = '      ' // THESTRING( I:ILAST )
	  THESTRING = IFSTRING
	  IFIRST = NXT_CHAR( 7, THESTRING )
	  ILAST = LENSTR( THESTRING )
C	  
      ENDIF
C
C---  Check for an ELSEIF or ELSE IF statement.
      ICHK1 = LENSTR( 'ELSEIF' )
      ICHK2 = LENSTR( 'ELSE IF' )
      IF( IFIRST+ICHK2 .LE. ILAST ) THEN      ! Stay within string length.
C
	  IF(     THESTRING(IFIRST:IFIRST+ICHK1) .EQ. 'ELSEIF '  .OR.
     1          THESTRING(IFIRST:IFIRST+ICHK1) .EQ. 'ELSEIF('  .OR.
     2          THESTRING(IFIRST:IFIRST+ICHK2) .EQ. 'ELSE IF ' .OR.
     3          THESTRING(IFIRST:IFIRST+ICHK2) .EQ. 'ELSE IF(' ) THEN

C---      The string is an ELSEIF or ELSE IF.
	    IFIRST = INDEX( THESTRING, 'IF' )
C
C---      Remove the ELSE at the beginning and the THEN at the end.
	    IFSTRING = '      ' // THESTRING( IFIRST:ILAST-4 )
	    THESTRING = IFSTRING
	    IFIRST = 1
	    ILAST = LENSTR( THESTRING )
C
	    WRITE_STRING = .TRUE.
	    RETURN
C
	  ENDIF
C
      END IF
C
C---  Check for an END, ENDIF, END IF, ENDDO, END DO, ELSE, RETURN
C---  or CONTINUE statement.
      IF( THESTRING(IFIRST:ILAST) .EQ. 'END'    .OR.
     1    THESTRING(IFIRST:ILAST) .EQ. 'ENDIF'  .OR.
     2    THESTRING(IFIRST:ILAST) .EQ. 'END IF' .OR.
     3    THESTRING(IFIRST:ILAST) .EQ. 'ENDDO'  .OR.
     4    THESTRING(IFIRST:ILAST) .EQ. 'END DO' .OR.
     5    THESTRING(IFIRST:ILAST) .EQ. 'ELSE'   .OR.
     6    THESTRING(IFIRST:ILAST) .EQ. 'RETURN' .OR.
     7    THESTRING(IFIRST:ILAST) .EQ. 'CONTINUE' ) THEN
C
	  WRITE_STRING = .FALSE.
	  RETURN
C
      END IF
C
C---  Check for a COMMON statement, if one has not been found yet.
      IF( .NOT. COMMON_FOUND ) THEN
C
	  ICHK = LENSTR( 'COMMON' )
C
	  IF( IFIRST+ICHK .LE. ILAST ) THEN
C
	    IF( THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'COMMON ' ) THEN
C
C---        The word COMMON was found in the string.  Now determine if
C---        THESTRING is the common statement for the C array.
C
C---        Go to the next character following the word COMMON.
	      IC = NXT_CHAR( IFIRST+ICHK,  THESTRING )
C
	      IF( THESTRING(IC:IC) .EQ. 'C' ) THEN
C
C---          The next non-blank character is a C.  Now make sure
C---          that the next non-blank character is a '('.
	        IC = NXT_CHAR(IC+1, THESTRING)
	        IF( THESTRING(IC:IC) .EQ. '(' ) COMMON_FOUND = .TRUE.
C
	        RETURN
C
  	      ENDIF
C
	    ENDIF
C
	  ENDIF  
C
      ENDIF
C
C---  Check for the words: IMPLICIT DOUBLE PRECISION.
      IF( .NOT. DOUBLE_FOUND ) THEN
C
	  ICHK = LENSTR('IMPLICIT')
	  IF( IFIRST+ICHK .LE. ILAST ) THEN       ! Stay within string length.
C
	    IF( THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'IMPLICIT ' ) THEN
C
C---        The word IMPLICIT was found; look for DOUBLE.
	      INXT = NXT_CHAR( IFIRST+ICHK+1, THESTRING  )
	      ICHK = LENSTR( 'DOUBLE' )
	      IF( INXT+ICHK .LE. ILAST ) THEN     ! Stay within string length.
C
	        IF( THESTRING(INXT:INXT+ICHK) .EQ. 'DOUBLE ' ) THEN
C
C---            The word DOUBLE was found; look for PRECISION.
		         INXT = NXT_CHAR( INXT+ICHK+1, THESTRING )
		         ICHK = LENSTR( 'PRECISION ' )
C
		         IF( INXT+ICHK .LE. ILAST ) THEN  ! Stay within string length.
		           IF( THESTRING(INXT:INXT+ICHK) .EQ. 'PRECISION ' )
     1                DOUBLE_FOUND = .TRUE.
		         ENDIF
C              
	        ENDIF
C
	      ENDIF
C
	    ENDIF
C
	  ENDIF
C
      ENDIF
C
C---  Check for a GOTO statement.
      ICHK = LENSTR( 'GO TO' )                   ! Length of the word GOTO.
      IF( IFIRST+ICHK .LE. ILAST ) THEN          ! Stay within string length.
C
C---    Make sure we stay within string length.
	  IF( THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'GOTO ' .OR.
     1      THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'GO TO' ) THEN
C
C---      The string is GOTO statement.
	    WRITE_STRING = .FALSE.
	    RETURN
C
	  END IF
C
      END IF
C
C---  Check for a WRITE statement.
      ICHK = 5                                   ! Length of the word WRITE.
      IF( IFIRST+ICHK .LE. ILAST ) THEN          ! Stay within string length.
C
C---    Make sure we stay within string length.
	  IF( THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'WRITE ' .OR.
     1      THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'WRITE(' ) THEN
C
C---      The string is WRITE statement.
	    WRITE_STRING = .FALSE.
	    RETURN
C
	  END IF
C
      END IF
C
C---  Check for a DO statement.
      ICHK = 2                                   ! Length of the word DO.
      IF( IFIRST+ICHK .LE. ILAST ) THEN          ! Stay within string length.
C
C---    Make sure we stay within string length.
	  IF( THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'DO '  ) THEN
C
C---      The string is DO statement.
	    WRITE_STRING = .FALSE.
	    RETURN
C
	  END IF
C
      END IF
C
C---  Check for a PRINT* statement.
      ICHK = 5                                   ! Length of the word PRINT*.
      IF( IFIRST+ICHK .LE. ILAST ) THEN          ! Stay within string length.
C
C---    Make sure we stay within string length.
	  IF( THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'PRINT*' .OR.
     1      THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'PRINT ' ) THEN
C
C---      The string is PRINT* statement.
	    WRITE_STRING = .FALSE.
	    RETURN
C
	  END IF
C
      END IF
C
C---  Check for a FORMAT statement.
      ICHK = 6                                   ! Length of the word FORMAT.
      IF( IFIRST+ICHK .LE. ILAST ) THEN          ! Stay within string length.
C
C---    Make sure we stay within string length.
	  IF( THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'FORMAT ' .OR.
     1      THESTRING(IFIRST:IFIRST+ICHK) .EQ. 'FORMAT(' ) THEN
C
C---      The string is FORMAT statement.
	    WRITE_STRING = .FALSE.
	    RETURN
C
	  END IF
C
      END IF
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
      SUBROUTINE CREATE_MAP
C-----------------------------------------------------------------------
C     This module is the main driver for creating the map file.
C-----------------------------------------------------------------------
C     FILE_INFO(100,3)
C         contains information for each of the files created in
C         module SPLIT.
C
C         FILE_INFO(I,1) = file contains common statement for C array
C         FILE_INFO(I,2) = file contains implicit double precision
C         FILE_INFO(I,3) = file is a subroutine that was called from
C                          the current file having the current variable
C                          as an argument
C
C    IVAR_INFO(11)
C         contains information on the current variable in reference
C         to the current file.
C         IVAR_INFO(1)  = var is argument for subroutine              (0/1)
C         IVAR_INFO(2)  = var is argument for function                (0/1)
C         IVAR_INFO(3)  = var dimension;   (0=not in dimension stmnt) (0/dim)
C         IVAR_INFO(4)  = real size        (0=not a real)             (0/1)
C         IVAR_INFO(5)  = double precision                            (0/1)
C         IVAR_INFO(6)  = integer size     (0=not an int)             (0/1)
C         IVAR_INFO(7)  = var is equivalenced                         (0/1)
C         IVAR_INFO(8)  = var in data statement                       (0/1)
C         IVAR_INFO(9)  = var in call statement                       (sub #)
C         IVAR_INFO(10) = var is in an IF statement                   (0/1)
C         IVAR_INFO(11) = var is on left side of =                    (0/1)
C         IVAR_INFO(12) = var is on right side of =                   (0/1)
C-----------------------------------------------------------------------
C
      COMMON /FILE_DAT/ LEV_FILE(10), NFILES, INFILE_TYPE,
     1                  FILE_INFO(100,3), FILEN(100)
      LOGICAL           FILE_INFO
      CHARACTER                          FILEN*8
C
      COMMON /FILE_IDS/ID_INMOD, ID_OUTMOD, ID_MOD(0:10), ID_OUT, ID_VAR
C
      COMMON /OUTINFO/ OUTSTRING, IOUT, LEN_OUT
      CHARACTER        OUTSTRING*80
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      CHARACTER ASTRING*1650, VSTRING*80
      LOGICAL DONE
C
      PARAMETER( HEADER=0 )
C
C
C---  The user can use a HEADER.DAT or an ALPHA.DAT file for the
C---  variable input file.  Since these files have different formats,
C---  read the first line of the variable file and determine the
C---  format and set the file type flag, INFILE_TYPE.
      READ(ID_VAR,'(A)',END=9999) VSTRING
      CALL GET_FILE_TYPE( INFILE_TYPE, VSTRING )
C
C---  Read the variable name, c location and comment from the string.
  100 READ(ID_VAR,'(A)',END=999) VSTRING
      WRITE(ID_OUT, '(A)' ) VSTRING(1:LENSTR(VSTRING)) 
      IF( VSTRING(1:1) .EQ. '*') GOTO 100
      CALL GET_VAR_NAME( VSTRING )
C
      CALL CLEAR_SCREEN
      WRITE(*,'(/////////30X,A)') VAR_NAME(LEVEL)
C
C---  Make sure the c location of the variable is within the range
C---  specified by the user.
      IF( LOC_IN_C(LEVEL) .LT. IVAR1 .OR. LOC_IN_C(LEVEL) .GT. IVAR2 )
     1    GOTO 100
C
C---  Now that we have the variable name, go through each of the
C---  the files and see how/if it was used in each file.
      DO J = 1, NFILES
C
	  IF( FILE_INFO(J,1) ) THEN
C
C---      File contains the common statement for the C array, therefore
C---      it is possible to find the current variable in the file.
	    OPEN(ID_MOD(LEVEL),
     1           FILE=FILEN(J)(1:LENSTR(FILEN(J)))//'.EQM',STATUS='OLD')
	    DONE = .FALSE.
C
C---      Reset the array that indicates what information was found and
C---      the array that saves the info found; also reset the string that
C---      saves the name of the subroutines having the variable as an
C---      argument.
	    CALL SET_INFO_ARRAYS
C
C---      Read a line from the module file.
  200     CONTINUE
          CALL READ_MOD(ID_MOD(LEVEL),DONE,LEN_ASTRING,ASTRING)
CCC       READ( ID_MOD(LEVEL),'(I4, 1X, A)', END=500) LEN_ASTRING, ASTRING
	    IF( DONE ) GOTO 500
C
C---      Determine if the current variable is in the string
	    LOC_IN_STR(LEVEL) = LOCATE_VAR
     1              ( VAR_NAME(LEVEL), ASTRING(1:LEN_ASTRING) )
C
C---      Determine how the variable was used in the string.
	    IF( LOC_IN_STR(LEVEL) .GT. 0 )
     1        CALL VAR_USAGE( ASTRING(1:LEN_ASTRING) )
C
C---      Go back and read the next line of the module file.
	    GOTO 200
C
  500     CLOSE( ID_MOD(LEVEL) )
C
C---      If the variable was found in the current file, write
C---      the information to the MAP file.
	    INFO_FOUND = 0
	    DO I = 1, 11
	      INFO_FOUND = INFO_FOUND + IVAR_INFO(I,LEVEL)
	    ENDDO
C
	    IF( INFO_FOUND .GT. 0  .AND. LEVEL .EQ. 0 ) THEN
C
C---        This is the main subroutine, write out the info found.
	      CALL WRITE_INFO( J )
C
	    ELSE IF( INFO_FOUND .GT. 0 ) THEN
C
C---        Save the information found in a sub subroutine.
	      CALL SAVE_INFO
C
	    ENDIF
C
C---      If level is greater than 0, then we need to go back to
C---      the subroutine that called the current file and finish
C---      reading it.
	    IF( LEVEL .GT. 0 ) THEN
            LEVEL = LEVEL - 1
            DONE = .FALSE.
	      GOTO 200
	    ENDIF
C
	  ENDIF
C
      ENDDO
C
      IF( IOUT .GT. 1 )THEN
	  IF( INFILE_TYPE .EQ. HEADER ) THEN
	    WRITE(ID_OUT,'(27X,A)') OUTSTRING(1:IOUT)
	  ELSE ! INFILE_TYPE .EQ. ALFA
	    WRITE(ID_OUT,'(22X,A)') OUTSTRING(1:IOUT)
	  ENDIF
	  IOUT = 1
	  OUTSTRING = ' '
      ENDIF
C
      GOTO 100
C
  999 RETURN
C
 9999 WRITE(*,*) '* * * NO VARIABLES FOUND IN VARIABLE FILE * * *'
      RETURN
      END
      SUBROUTINE DEL_FILES
C-----------------------------------------------------------------------
C     This module deletes the files that were created when the
C     MODULE.FOR was split up.
C-----------------------------------------------------------------------
C
      COMMON /FILE_DAT/ LEV_FILE(10), NFILES, INFILE_TYPE,
     1                  FILE_INFO(100,3), FILEN(100)
      LOGICAL           FILE_INFO
      CHARACTER                          FILEN*8
C
      DO J = 1, NFILES
	  OPEN( 22, FILE=FILEN(J)(1:LENSTR(FILEN(J)))//'.EQM',
     1             STATUS='OLD' )
	  CLOSE( 22, STATUS='DELETE')
      END DO
C
      RETURN
      END
      SUBROUTINE DO_CALL( IKEY, THESTRING )
C-----------------------------------------------------------------------
C     This module indicates that the current variable was found in
C     a call statement to another subroutine.  It changes the current
C     variable to the name of the variable in the new subroutine.
C-----------------------------------------------------------------------
C
      COMMON /FILE_DAT/ LEV_FILE(10), NFILES, INFILE_TYPE,
     1                  FILE_INFO(100,3), FILEN(100)
      LOGICAL           FILE_INFO
      CHARACTER                          FILEN*8
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      CHARACTER SUBNAME*15
C
      CHARACTER*(*) THESTRING
C
      LOGICAL UTIL_MOD
C
C---  The FORTRAN reserved word CALL was found.  Indicate this in
C---  the info array.
      IVAR_INFO(IKEY,LEVEL) = 1
C
C---  Get the name of the subroutine.
      CALL FIND_SUBNAME( L_SUBNAME, SUBNAME, THESTRING )
C
C---  See if the module is a utility.  If it is, determine if the
C---  variable is being initialized in the utility.
      CALL UTILITY_MOD( UTIL_MOD, L_SUBNAME, SUBNAME, THESTRING)
C
C---  Add the subroutine name to the string if it isn't included yet.
      CALL SAVE_SUBNAME( L_SUBNAME, SUBNAME )
C
      IF( .NOT. UTIL_MOD ) THEN
C
C---    Determine if the subroutine is included in the MODULE.FOR.  If
C---    it is, determine if the variable name is changed in the
C---    subroutine statement or if the module does not include a
C---    blank common.
	  CALL FIND_FILEN( L_SUBNAME, SUBNAME, THESTRING )
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE DO_DATA( IKEY )
C-----------------------------------------------------------------------
C     This module indicates that the statement contained the current
C     variable in a data statement.
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
C
C---  The FORTRAN reserved word SUBROUTINE was found.  Indicate this in
C---  the info array.
      IVAR_INFO(IKEY,LEVEL) = 1
C
      RETURN
      END
      SUBROUTINE DO_DIMENSION( IKEY, THESTRING )
C-----------------------------------------------------------------------
C     The variable was found in a DIMENSION statement.  We need
C     to determine the dimension.
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      CHARACTER*(*) THESTRING
C
C---  The FORTRAN reserved word DIMENSION was found.  Find the dimension
C---  of the variable.
      LEN_THESTRING = LENSTR( THESTRING )
      L_LPAREN = LOC_IN_STR(LEVEL) +
     1    INDEX( THESTRING(LOC_IN_STR(LEVEL):LEN_THESTRING), '(' ) - 1
      L_RPAREN = L_LPAREN +
     1           INDEX( THESTRING(L_LPAREN  :LEN_THESTRING), ')' ) - 1
      VAR_DIM(LEVEL) = THESTRING(L_LPAREN:L_RPAREN)
C
      IVAR_INFO( IKEY, LEVEL ) = 1
C
      RETURN
      END
      SUBROUTINE DO_DOUBLE_PREC( IKEY, THESTRING )
C-----------------------------------------------------------------------
C     The variable was found in a DOUBLE PRECISION statement.  We
C     need to determine if it was dimensioned in the statement.
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
C
      CHARACTER*(*) THESTRING
C
C---  The FORTRAN reserved words DOUBLE PRECISION was found.  Indicate this
C---  in the info array.
      IVAR_INFO(IKEY, LEVEL) = 1
C
C---  Check to see if the double precision statement includes an
C---  array dimension for the variable, if it does, save it
C---  in the dimension slot in the IVAR_INFO array.
      L_LPAREN =
     1    NXT_CHAR(LOC_IN_STR(LEVEL)+LENSTR(VAR_NAME(LEVEL)), THESTRING)
C     
      IF( THESTRING(L_LPAREN:L_LPAREN) .EQ. '(' ) THEN 
C      
	  L_RPAREN = L_LPAREN +
     1          INDEX( THESTRING(L_LPAREN:LENSTR(THESTRING)), ')' ) - 1
	  VAR_DIM(LEVEL) = THESTRING(L_LPAREN:L_RPAREN)
	  IVAR_INFO(3,LEVEL) = 1 
C	  
      ENDIF
C
      RETURN
      END
      SUBROUTINE DO_EQUIVALENCE( IKEY, THESTRING )
C-----------------------------------------------------------------------
C     This module indicates that the variable was found in an
C     equivalence statement.
C-----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
C---  EQUIVALENCE (C(1203),ISETLA)
C---  EQUIVALENCE (C(1203),ISETLA(1))
C---  Determine the C location of the variable in the EQUIVALENCE.
      ILEN     = LENSTR( THESTRING )
C
C---  Find the location of the first character following the first
c---  open paren (left paren).
      LC       = NXT_CHAR( (INDEX(THESTRING,'(' )+1), THESTRING )
C
C---  Make sure the variable is being equivalenced to a C location.
      IF( THESTRING(LC:LC+1) .NE. 'C(' .AND.
     1    THESTRING(LC:LC+1) .NE. 'C ' ) RETURN
C
C---  Find the location of the second open paren (left paren).
      L_LPAREN = LC +
     1   NXT_CHAR(INDEX(THESTRING(LC:ILEN),'('), THESTRING(LC:ILEN)) - 1
C
C---  Find the location of the close paren (right paren).
      L_RPAREN = L_LPAREN +
     1   INDEX(THESTRING(L_LPAREN:ILEN), ')' ) - 1
C
C---  Read the C location from the equivalence statement.
      CALL READ_INTEGER(IEQUIV, IERR, THESTRING(L_LPAREN+1:L_RPAREN-1))
C
C---  Indicate in the info array that the FORTRAN reserved word
C---  EQUIVALENCE was found
      IF( IERR .EQ. 0 ) IVAR_INFO(IKEY,LEVEL) = IEQUIV
C
      RETURN
      END
      SUBROUTINE DO_FUNCTION( IKEY )
C-----------------------------------------------------------------------
C     This module indicates that the variable was a argument for a
C     function.
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
C---  The FORTRAN reserved word FUNCTION was found.  Indicate this in
C---  the info array.
      IVAR_INFO(IKEY,LEVEL) = 1
C
      RETURN
      END
      SUBROUTINE DO_IF( IKEY )
C-----------------------------------------------------------------------
C     This module indicates that the string contained the current
C     variable in an IF statement.
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
C
C---  The FORTRAN reserved word IF was found.  Indicate this in
C---  the info array.
      IVAR_INFO(IKEY,LEVEL) = 1
C
      RETURN
      END
      SUBROUTINE DO_INTEGER( IKEY, THESTRING )
C-----------------------------------------------------------------------
C     The variable was found in a INTEGER statement.  We need to
C     determine if it was dimensioned in the statement.
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
C
      CHARACTER*(*) THESTRING
C
C---  The FORTRAN reserved word INTEGER was found.  Indicate this in
C---  the info array.
      IVAR_INFO(IKEY,LEVEL) = 1
      RETURN
C
C---  Check to see if the INTEGER statement includes an
C---  array dimension for the variable, if it does, save it
C---  in the dimension slot (3) in the IVAR_INFO array.
      L_LPAREN =
     1    NXT_CHAR(LOC_IN_STR(LEVEL)+LENSTR(VAR_NAME(LEVEL)), THESTRING)
C     
      IF( THESTRING(L_LPAREN:L_LPAREN) .EQ. '(' ) THEN
	  L_RPAREN = L_LPAREN +
     1           INDEX( THESTRING(L_LPAREN:LENSTR(THESTRING)), ')' ) - 1
	  VAR_DIM(LEVEL) = THESTRING(L_LPAREN:L_RPAREN)
	  IVAR_INFO(3,LEVEL) = 1
      ENDIF
C
      RETURN
      END
      SUBROUTINE DO_REAL( IKEY, THESTRING )
C-----------------------------------------------------------------------
C     The variable was found in a REAL statement.  We need to
C     determine if it was dimensioned in the statement.
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
C
      CHARACTER*(*) THESTRING
C
C---  The FORTRAN reserved word REAL was found.
      IVAR_INFO(IKEY,LEVEL) = 1
C
C---  Check to see if the double precision statement includes an
C---  array dimension for the variable, if it does, save it
C---  in the dimension slot (3) in the IVAR_INFO array.
      L_LPAREN =
     1    NXT_CHAR(LOC_IN_STR(LEVEL)+LENSTR(VAR_NAME(LEVEL)), THESTRING)
C
      IF( THESTRING(L_LPAREN:L_LPAREN) .EQ. '(' ) THEN
C      
	  L_RPAREN = L_LPAREN +
     1         INDEX( THESTRING(L_LPAREN:LENSTR(THESTRING)), ')' ) - 1
	  VAR_DIM(LEVEL) = THESTRING(L_LPAREN:L_RPAREN)
	  IVAR_INFO(3,LEVEL) = 1
C	  
      ENDIF
C
      RETURN
      END
      SUBROUTINE DO_SUBROUTINE( IKEY, THESTRING )
C-----------------------------------------------------------------------
C     This module indicates that the variable was found in SUBROUTINE
C     statement.
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      CHARACTER*(*) THESTRING
C
C---  Make sure that if the variable is found in a subroutine statement,
C---  that it is an argument of the statement.
      LPAREN = INDEX( THESTRING, '(' )
C
      IF( LPAREN .GT. 0 .AND. LPAREN .LT. LOC_IN_STR(LEVEL) ) THEN
C
C---     The variable is the argument of a SUBROUTINE statement.
C---     Indicate this in the info array.
	 IVAR_INFO(IKEY,LEVEL) = 1
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE FIND_FILEN( L_SUBNAME, SUBNAME, THESTRING )
C-----------------------------------------------------------------------
C     This module determines if the subroutine in a call statement is
C     a module in the MODULE.FOR.
C-----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ID_INMOD, ID_OUTMOD, ID_MOD(0:10), ID_OUT, ID_VAR
C
      COMMON /FILE_DAT/ LEV_FILE(10), NFILES, INFILE_TYPE,
     1                  FILE_INFO(100,3), FILEN(100)
      LOGICAL           FILE_INFO
      CHARACTER                          FILEN*8
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      CHARACTER*(*) THESTRING
      CHARACTER SUBNAME*15, ASTRING*1650
      LOGICAL MOD_FOUND, DONE
C
      MOD_FOUND = .FALSE.
      JFILE = 0
      DO WHILE(.NOT. MOD_FOUND .AND. JFILE .LT. NFILES)
C
	  JFILE = JFILE + 1
	  IF( FILEN(JFILE)(1:LENSTR(FILEN(JFILE))) .EQ.
     1        SUBNAME(1:L_SUBNAME) ) MOD_FOUND = .TRUE.
C
      ENDDO
C
C---  If the module was found in the MODULE.FOR, check to see if
C---  the variable name is modified in the subroutine statement or
C---  if the subroutine does not contain a blank common for the C
C---  array.  If either condition is found, follow this variable
C---  name through the subroutine, otherwise, the variable will
C---  be caught by the DO J = 1, NFILES loop in CREATE_MAP.
      IF( .NOT. MOD_FOUND ) RETURN
C
C---  Determine the location in the argument list of the variable
C---  in the call statement by finding the number of commas prior
C---  to the location of the variable in the string.
      ICOMMA = INDEX( THESTRING(1:LOC_IN_STR(LEVEL)), ',' )
      ICOUNT = 0
      IFOUND = ICOMMA + 1
      DO WHILE( ICOMMA .GT. 0 )
	  ICOUNT = ICOUNT + 1
	  ICOMMA = INDEX( THESTRING(IFOUND:LOC_IN_STR(LEVEL)), ',' )
	  IFOUND = IFOUND + ICOMMA
      END DO
C
C
      OPEN(ID_MOD(LEVEL+1), FILE=FILEN(JFILE)
     1     (1:LENSTR(FILEN(JFILE)))//'.EQM', STATUS='OLD')
C
C---  Read a line from the new module.
      CALL READ_MOD(ID_MOD(LEVEL+1),DONE,LEN_ASTRING,ASTRING )
C
C---  Now find the name of the variable in the subroutine statement.
      IF( ICOUNT .GT. 0 ) THEN
	  ISTART = 1
	  DO I = 1, ICOUNT
	    ICOMMA = INDEX( ASTRING(ISTART:LEN_ASTRING), ',' )
	    ISTART = ISTART + ICOMMA
	  ENDDO
	  ISTART = NXT_CHAR( ISTART, ASTRING(1:LEN_ASTRING) )
      ELSE
	  IPAREN = INDEX( ASTRING(1:LEN_ASTRING),'(' )
	  ISTART = NXT_CHAR( IPAREN+1, ASTRING(1:LEN_ASTRING ) )
      ENDIF
C
      ILEN = INDEX( ASTRING(ISTART:LEN_ASTRING), ',' )
      IF( ILEN .EQ. 0 ) THEN
	  IEND = LENSTR( ASTRING(1:LEN_ASTRING-1) )
      ELSE
	 IEND = ISTART + ILEN - 2
      ENDIF
C
      IF( .NOT. FILE_INFO(JFILE,1) .OR.
     1     VAR_NAME(LEVEL)(1:LENSTR(VAR_NAME(LEVEL))) .NE.
     2     ASTRING(ISTART:IEND) ) THEN
	  LEVEL = LEVEL + 1
	  LEV_FILE(LEVEL)  = JFILE
	  VAR_NAME(LEVEL) = ASTRING(ISTART:IEND)
	  REWIND( ID_MOD(LEVEL) )
	  CALL SET_INFO_ARRAYS
      ELSE
	  CLOSE( ID_MOD(LEVEL+1) )
      ENDIF
C
      RETURN
      END
      SUBROUTINE FIND_SUBNAME( L_SUBNAME, SUBNAME, THESTRING)
C-----------------------------------------------------------------------
C     This module finds the subroutine name in a call statement.
C-----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
      CHARACTER SUBNAME*15
C
C---  Get the sub name.
      ISUB = NXT_CHAR(INDEX(THESTRING,'CALL')+LENSTR('CALL'), THESTRING)
      IPAREN    = INDEX( THESTRING, '(' ) - 1
      SUBNAME   = THESTRING(ISUB:IPAREN)
      L_SUBNAME = LENSTR( SUBNAME )
C
      RETURN
      END
      SUBROUTINE GET_FILE_TYPE(IFILE, THESTRING)
C-------------------------------------------------------------------
C     This module determines the format of the variable input file.
C     If the variable file is a HEAD.ASC type file, then 0 is
C     returned.  If the variable file is a ALPHA.DAT type file, then
C     1 is returned.  If the file is a HEAD.ASC then the word
C     SCROLL will be found in the first line of the file and this
C     module will read past the scroll variables, returning the record
C     following the * record (the * record signifies the end of the
C     scroll variables).
C
C     The format of the records following the scroll variables in the
C     HEADER.DAT is:
C        C_LOCATION     VAR_NAME       VAR_COMMENT
C
C     The format of the records in a ALPHA.DAT file is:
C        VAR_NAME       C_LOCATION     VAR_COMMENT
C-------------------------------------------------------------------
C
      COMMON /FILE_IDS/ID_INMOD, ID_OUTMOD, ID_MOD(0:10), ID_OUT, ID_VAR
C
      COMMON /OUTINFO/ OUTSTRING, IOUT, LEN_OUT
      CHARACTER        OUTSTRING*80
C
      CHARACTER*(*) THESTRING
      LOGICAL ASTERIK
C
      ISCROLL = INDEX( THESTRING, 'SCROLL' )
      IF( ISCROLL .EQ. 0 ) ISCROLL = INDEX( THESTRING, 'scroll' )
C
      IF( ISCROLL .EQ. 0 ) THEN
C
C---    The variable file has an ALPHA format.
C
	  IFILE = 1
	  LEN_OUT = 50
	  BACKSPACE( ID_VAR )
C
      ELSE
C
C---    The variable file has a HEAD.ASC format; set the file
C---    type flag and read past the scroll variables by finding
C---    the first record following the record containing an *
C---    in column 1.
C
	  IFILE = 0
	  LEN_OUT = 45
C
C	  WRITE(ID_OUT,'(A)') THESTRING(1:LENSTR(THESTRING))
	  ASTERIK = .FALSE.
	  DO WHILE( .NOT. ASTERIK )
	    READ(ID_VAR,'(A)') THESTRING
C	    WRITE(ID_OUT,'(A)') THESTRING(1:LENSTR(THESTRING))
	    IF( THESTRING(1:1) .EQ. '*' ) ASTERIK = .TRUE.
	  END DO
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE GET_RANGE
C-----------------------------------------------------------------------
C     This module prompts the user to enter a range of c locations
C     to cross reference
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      CHARACTER REPLY*12
C
      CALL CLEAR_SCREEN
C
  100 WRITE(*,200)
  200 FORMAT(//// T15,
     1'Enter range of C locations for variables to cross reference',
     2 //15X,'Valid formats (spaces are ignored):',
     3 / 19X,'100      ( single variable                             )',
     4 / 19X,'- 100    ( all variables up to 100                     )',
     5 / 19X,'100 -    ( all variables greater than or equal to 100  )',
     6 / 19X,'99 -200  ( all variables between 99 and 200, inclusive )',
     7 / 19X,'ALL      ( all variables                               )',
     8 //15X, 'Default = ALL: ' \ )
C
      READ(*,'(A)') REPLY
C
      IF( LENSTR(REPLY) .EQ. 0 .OR.
     1    REPLY .EQ. 'ALL' .OR. REPLY .EQ. 'all' ) THEN  
C     
	  IVAR1 = 1
	  IVAR2 = 3510
C
      ELSE
C
	  IDASH = INDEX( REPLY, '-' )
	  IF( IDASH .EQ. 0 ) THEN
C
C---      A single C location was entered.
	    CALL READ_INTEGER(ITEMP, IERR, REPLY)
	    IF( IERR .GT. 0 ) THEN
	      WRITE(*,'(// T20, A)') '* * * ERROR READING VALUE * * *'
	      GOTO 100
	    ENDIF
	    IVAR1 = ITEMP
	    IVAR2 = IVAR1
C
	  ELSE
C
C---      A range was specified.
C
	    IF( IDASH .EQ. NXT_CHAR(1,REPLY) ) THEN
C
C---        Format of the range was: - 200 (with or without spaces prior
C---        to the dash); print all variables up to 200.
	      IVAR1 = 1
C
	    ELSE
C
C---        Format of the range was: 200-300, 200-; Read first location.
	      CALL READ_INTEGER(ITEMP, IERR, REPLY(1:IDASH-1) )
	      IF( IERR .GT. 0 .OR.
     1            ITEMP .LE. 0 .OR. ITEMP .GT. 3510 ) THEN
	        WRITE(*,'(//T20,A)')'* * * INVALID RANGE * * *'
	        GOTO 100
	      ENDIF
	      IVAR1 = ITEMP
C
	    ENDIF
C
	    IF( IDASH .EQ. LENSTR( REPLY ) ) THEN
C
C---        Check for just a dash being entered.
	      IF( IDASH .EQ. NXT_CHAR(1,REPLY) ) GOTO 100
C
C---        Format of the range was 200-; set the final location to
C---        3510, the size of the C array.
	      IVAR2 = 3510
C
	    ELSE
C
C---        Format of the range was 200-300; read the final location.
	      CALL READ_INTEGER
     1           (ITEMP,IERR,REPLY(IDASH+1:LENSTR(REPLY)))
	      IF( IERR .GT. 0 .OR.
     1           ITEMP .LE. 0 .OR. ITEMP .GT. 3510 ) THEN
	        WRITE(*,'(// T20, A)') '* * * INVALID RANGE * * *'
	        GOTO 100
	      ENDIF
	      IVAR2 = ITEMP
	      IF( IVAR1 .GT. IVAR2 ) THEN
	        ITEMP = IVAR1
	        IVAR1 = IVAR2
	        IVAR2 = ITEMP
	      ENDIF
C
	    ENDIF
C
	  ENDIF
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE GET_VAR_NAME( THESTRING )
C-----------------------------------------------------------------------
C     This module extracts the variable name, the variables c location
C     and any comment contained on the string.
C-----------------------------------------------------------------------
C
      CHARACTER THESTRING*(*)
C
      COMMON /FILE_DAT/ LEV_FILE(10), NFILES, INFILE_TYPE,
     1                  FILE_INFO(100,3), FILEN(100)
      LOGICAL           FILE_INFO
      CHARACTER                          FILEN*8
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
C
      PARAMETER( HEADER=0, ALPHA=1 )
C
      IF( INFILE_TYPE .EQ. HEADER ) THEN
	  VAR_NAME(LEVEL) = THESTRING(13:20)
	  CALL READ_INTEGER( LOC_C, IERR, THESTRING(5:8) )
	  IF( IERR .EQ. 0 ) LOC_IN_C(LEVEL) = LOC_C
      ELSE
	  VAR_NAME(LEVEL) = THESTRING(2:11)
	  CALL READ_INTEGER( LOC_C, IERR, THESTRING(16:19) )
	  IF( IERR .EQ. 0 ) LOC_IN_C(LEVEL) = LOC_C
      ENDIF
C
      LPAREN = INDEX( VAR_NAME(LEVEL), '(' )
      IF( LPAREN .GT. 0 ) VAR_NAME(LEVEL) = VAR_NAME(LEVEL)(1:LPAREN-1)
      CALL STR_UPCASE( VAR_NAME(LEVEL), VAR_NAME(LEVEL) )
C
      RETURN
      END
      SUBROUTINE PREPARE_FILES( GOTO_END )
C-----------------------------------------------------------------------
C     This module prompts for the names of the two input files and
C     opens them.  In addition, it prompts the user to enter the name
C     of the output file.
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      COMMON /FILE_IDS/ID_INMOD, ID_OUTMOD, ID_MOD(0:10), ID_OUT, ID_VAR
C
      CHARACTER MOD_FILE*25, OUT_FILE*25, VAR_FILE*25, DIRPATH*80
C
      INTEGER*4 LENDIR, GETDRIVEDIRQQ
      LOGICAL EXIT_PROG, GOODFILE, GOTO_END
C
      DATA ID_INMOD /31/, ID_OUTMOD /32/, ID_OUT/33/, ID_VAR/34/
      DATA ID_MOD / 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46 /
      DATA LEVEL / 0 /
C
C      CALL SET_DIR
      LENDIR = GETDRIVEDIRQQ( DIRPATH )  
C      
      WRITE(*,'(5X, A)')'EQMAP - Verison 3.0'
C
C---  Prompt for the files
      GOODFILE = .FALSE.
      DO WHILE( .NOT. GOODFILE )
C
	  CALL CLEAR_SCREEN
C
	  WRITE(*,100) DIRPATH
  100   FORMAT( //5X,'Current directory is: ' 
     1               /5X, A,
     1              //5X, 'Enter name of modules file (input)',
     1               /5X, 'Default = MODULE.FOR : ' \)
C
	  READ(*,'(A)' ) MOD_FILE
	  IF( LENSTR(MOD_FILE) .EQ. 0 ) MOD_FILE = 'MODULE.FOR'
	  WRITE(*,'(1X,T25,A)') MOD_FILE
C
	  OPEN(ID_INMOD, FILE=MOD_FILE(1:LENSTR(MOD_FILE)),
     1       FORM='FORMATTED', STATUS='OLD', ERR=200)
	  GOODFILE = .TRUE.
C
  200   CONTINUE
C
	  IF( .NOT. GOODFILE ) THEN
	    GOTO_END = EXIT_PROG()
	    IF( GOTO_END ) RETURN
	  ENDIF

      END DO
C
C---  Prompt for the name of the file containing the variables
      GOODFILE = .FALSE.
      DO WHILE( .NOT. GOODFILE )
C
	  CALL CLEAR_SCREEN
C
	  WRITE(*,300) DIRPATH
  300   FORMAT( //////5X,'Current directory is: ' 
     1           /5X,A,
     2          //5X,'Enter name of file containing variables (input)'
     3           /5X,'Default = HEAD.ASC : ' \)
C
	  READ(*,'(A)' ) VAR_FILE
	  IF( LENSTR(VAR_FILE) .EQ. 0 ) VAR_FILE = 'HEAD.ASC'
	  WRITE(*,'(1X,T25,A)') VAR_FILE
C
	  OPEN(ID_VAR, FILE=VAR_FILE(1:LENSTR(VAR_FILE)),
     1        FORM='FORMATTED', STATUS='OLD', ERR=400)
	  GOODFILE = .TRUE.
C
  400   CONTINUE
C
	  IF( .NOT. GOODFILE ) THEN
	    GOTO_END = EXIT_PROG()
	    IF( GOTO_END ) RETURN
	  ENDIF

      END DO
C
C---  Prompt for the name of the output file
      GOODFILE = .FALSE.
      DO WHILE( .NOT. GOODFILE )
C
	  CALL CLEAR_SCREEN
C
	  WRITE(*,500) DIRPATH
  500   FORMAT( //////5X,'Current directory is: ' 
     1          /5X, A,
     2//5X, 'Enter name of file to contain cross referenced variables',
     3 ' (output)', /5X, 'Default = EQMAP.ASC : ' \)
C
	  READ(*,'(A)' ) OUT_FILE
	  IF( LENSTR(OUT_FILE) .EQ. 0 ) OUT_FILE = 'EQMAP.ASC'
	  WRITE(*,'(1X,T25,A)') OUT_FILE
C
	  OPEN(ID_OUT, FILE=OUT_FILE(1:LENSTR(OUT_FILE)),
     1       FORM='FORMATTED', STATUS='UNKNOWN', ERR=600 )
	  GOODFILE = .TRUE.
C
  600   CONTINUE
C
	  IF( .NOT. GOODFILE ) THEN
	    GOTO_END = EXIT_PROG()
	    IF( GOTO_END ) RETURN
	  ENDIF

      END DO 
C        
C---  Write the definitions of the symbols in the output file.
      WRITE(ID_OUT,700)
  700 FORMAT()      
C
      CALL CLEAR_SCREEN
C
      RETURN
      END
      SUBROUTINE READ_INTEGER( IVALUE, IERR, THESTRING )
C-----------------------------------------------------------------------
C     This module performs an internal read of an integer value from
C     a string.  This module was written to provide ease of transfer to
C     the PC and for modularity.
C-----------------------------------------------------------------------
C  IVALUE - (I) Out.  The integer value read from the string passed
C                     into this module.
C
C  IERR   - (I) Output.  An error indicator flag.  IERR is set to 0 at 
C           the beginning of the module.  If an error occurs during the 
C           internal read, IERR is set to 1.
C
C  STRING- (C) INPUT.  The string that contains the real number to read.
C----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
C
      IERR = 0
C
C---  Create a temp file
      KSCRATCH = 99
      OPEN(KSCRATCH, FORM = 'FORMATTED', STATUS = 'SCRATCH')
C
C---  Write the string to the temp file.
      WRITE(KSCRATCH,'(A)') THESTRING
C
C---  Rewind the file and read the string as a real number.
      REWIND( KSCRATCH )
      READ(KSCRATCH,*,ERR=900) IVALUE
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
      SUBROUTINE READ_MOD( ID_FILE, DONE, LEN_READIN, THESTRING )
C-----------------------------------------------------------------------
C     This module reads a line from the new module, placing all continued
C     lines in the same string.
C-----------------------------------------------------------------------
C
      COMMON /FILE_DAT/ LEV_FILE(10), NFILES, INFILE_TYPE,
     1                  FILE_INFO(100,3), FILEN(100)
      LOGICAL           FILE_INFO
      CHARACTER                          FILEN*8
      CHARACTER THESTRING*1650, ASTRING*512
      LOGICAL DONE
C
      READ( ID_FILE,'(I4, 1X, A)', Err=500) LEN_READIN, ASTRING
C
      IF( LEN_READIN .LT. 500 ) THEN
C
	  THESTRING     = ASTRING
	  LEN_THESTRING = LEN_ASTRING
C
      ELSE
C
C---    Save the first line to THESTRING, removing the continuation
C---    signal: ' &&'.
	  LEN_ASTRING = LENSTR( ASTRING )
	  IFIRST      = 1
	  ILAST       = IFIRST + (LEN_ASTRING-3) - 1
	  THESTRING(IFIRST:ILAST) = ASTRING(1:LEN_ASTRING-3)
C
C---    Read the next line.  If it is also a continuation, save it to
C---    THESTRING and go back and read the next line.
  200   READ( ID_FILE,'(A)', END=500) ASTRING
	  LEN_ASTRING = LENSTR( ASTRING )
C
	  IF( ASTRING(LEN_ASTRING-2:LEN_ASTRING) .EQ. ' &&' ) THEN
	    IFIRST = ILAST + 1
	    ILAST  = IFIRST + (LEN_ASTRING-3) - 1
	    THESTRING(IFIRST:ILAST) = ASTRING(1:LEN_ASTRING-3)
	    GOTO 200
	  ENDIF
C
	  IFIRST = ILAST + 1
	  ILAST  = IFIRST + LEN_ASTRING - 1
	  THESTRING(IFIRST:ILAST) = ASTRING(1:LEN_ASTRING)
C
      ENDIF

      RETURN
      WRITE(*,'(A)') THESTRING
C
  500 CONTINUE
      DONE = .TRUE.
      RETURN
C
      END
      SUBROUTINE SAVE_SUBNAME( L_SUBNAME, SUBNAME )
C-----------------------------------------------------------------------
C     This module determines if the subroutine in the current call
C     statement is already in the SUBS string.  The SUBS string keeps
C     track of the subroutines in the call statements in the current
C     module.  If the subroutine is not in SUBS, it is added.
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      COMMON /SUB_LST/ L_SUBS(0:10), SUBS(0:10)
      CHARACTER                      SUBS*80
C
      CHARACTER SUBNAME*15
      LOGICAL NEW_SUB
C
      NEW_SUB = .TRUE.
      IFIND = INDEX( SUBS(LEVEL), SUBNAME(1:L_SUBNAME) )
      IF( IFIND .NE. 0 ) THEN
C
	  NEW_SUB = .FALSE.
C
C---    If the subroutine is not located at the beginning of the string
C---    and the previous character is not a comma, then this is a new
C---    subroutine.
	  IF( IFIND .GT. 1 ) THEN
	    IF( SUBS(LEVEL)(IFIND-1:IFIND-1) .NE. ',' ) NEW_SUB = .TRUE.
	  ENDIF
C
C---    If the subroutine is not located at the end of the string and
C---    the character following the subroutine name is not a comma, then
C---    this is a new subroutine.
	  IF( .NOT. NEW_SUB ) THEN
	    IEND = IFIND + L_SUBNAME - 1
	    IF( IEND .LT. L_SUBS(LEVEL) ) THEN
	      IF( SUBS(LEVEL)(IEND+1:IEND+1) .NE. ',' ) NEW_SUB = .TRUE.
	    ENDIF
	  ENDIF
C
      ENDIF
C
      IF( NEW_SUB ) THEN
	  IF( L_SUBS(LEVEL) .EQ. 0 ) THEN
	    SUBS(LEVEL) = SUBNAME(1:L_SUBNAME)
	  ELSE
	    SUBS(LEVEL) = SUBS(LEVEL)(1:L_SUBS(LEVEL)) //
     1                        ',' // SUBNAME(1:L_SUBNAME)
	  ENDIF
	  L_SUBS(LEVEL) = LENSTR( SUBS(LEVEL) )
      ENDIF
C
      RETURN
      END
      SUBROUTINE SET_DIR
C-----------------------------------------------------------------------
C     This module lets the user set the directory and drive.
C-----------------------------------------------------------------------
C              
      CHARACTER ANSW*1, CUR_PATH*60, NEW_PATH*60
C           
      INTEGER*4 LENDIR, GETDRIVEDIRQQ 
C
      LOGICAL*4 SUCCESS, CHANGEDIRQQ        
C      
      CALL CLEAR_SCREEN
C      
      LENDIR = GETDRIVEDIRQQ( CUR_PATH )  
C
      WRITE(*,'(/////)')      
C
  100 WRITE(*,'(// 5X, A / 5X, A /// 5X, A / 5X, A \ )' )
     1 'The current directory is:', CUR_PATH,
     2 'Do you wish to change the directory? (Y or N)',
     3 'Default = N : '  
C
      READ(*,'(A)') ANSW
      IF( ANSW .EQ. 'Y' .OR. ANSW .EQ. 'y' ) THEN 
C      
        WRITE(*,'(// 5X, A / 5X, A \)' ) 'Enter new directory', ': '
        READ(*,'(A)') NEW_PATH 
C
        SUCCESS = CHANGEDIRQQ( NEW_PATH ) 
        IF( .NOT. SUCCESS ) THEN
          CALL CLEAR_SCREEN
          WRITE(*,'(/// 5X, A )' ) '* * * INVALID DIRECTORY * * *'
          GOTO 100                                                
        ENDIF
C
      ENDIF      
C                
      RETURN
      END
      SUBROUTINE SET_INFO_ARRAYS
C-----------------------------------------------------------------------
C     Reset the array that indicates what information was found
C     and resets the file information array.
C     There is 10 kinds of information looked for in each file.
C-----------------------------------------------------------------------
C
      COMMON /SUB_LST/ L_SUBS(0:10), SUBS(0:10)
      CHARACTER                      SUBS*80
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      IVAR_INFO( 1,LEVEL) = 0            !  var is argument for subroutine
      IVAR_INFO( 2,LEVEL) = 0            !  var is argument for function
      IVAR_INFO( 3,LEVEL) = 0            !  var dimension  (0=not dimensioned)
      IVAR_INFO( 4,LEVEL) = 0            !  real size      (0=not a real)
      IVAR_INFO( 5,LEVEL) = 0            !  double precision
      IVAR_INFO( 6,LEVEL) = 0            !  integer size   (0=not an int)
      IVAR_INFO( 7,LEVEL) = 0            !  var is equivalenced
      IVAR_INFO( 8,LEVEL) = 0            !  var in data statement
      IVAR_INFO( 9,LEVEL) = 0            !  var in call statement
      IVAR_INFO(10,LEVEL) = 0            !  var is in an IF statement
      IVAR_INFO(11,LEVEL) = 0            !  var is on left side of =
      IVAR_INFO(12,LEVEL) = 0            !  var is on right side of =
C
      L_SUBS(LEVEL) = 0
      SUBS(LEVEL) = '                                        '  //
     1              '                                        '
C
      RETURN
      END
      SUBROUTINE SPLIT_MODULE
C-------------------------------------------------------------------
C     This module splits the MODULE.FOR program into individual
C     files, removing comments.
C-------------------------------------------------------------------
C
      CHARACTER STRING1*1650, STRING2*72, TMPSTRING1*72
      CHARACTER GET_FILENAME*8, TEMP_FILEN*8
C
      COMMON /FILE_IDS/ID_INMOD, ID_OUTMOD, ID_MOD(0:10), ID_OUT, ID_VAR
C
      COMMON /FILE_DAT/ LEV_FILE(10), NFILES, INFILE_TYPE,
     1                  FILE_INFO(100,3), FILEN(100)
      LOGICAL           FILE_INFO
      CHARACTER                          FILEN*8
C
      LOGICAL DOUBLE_FOUND, COMMON_FOUND, WRITE_STRING, MORE_DATA
C
C---  Inform the user the status of the program.
      CALL CLEAR_SCREEN
      WRITE(*,'(/////10X,A)') ' * * * Splitting MODULE.FOR * * *'
C
C---  Initialize variable that keeps track of the number of files created.
      NFILES = 0
C
C---  Initialize the flag that indicates that the variables in the 
C---- current subroutine are implicit double precision.
      DOUBLE_FOUND = .FALSE.
C
C---  Initialize the flag that indicates that the common statement
C---  for the C array was found in the current module.
      COMMON_FOUND = .FALSE.
C
C---  Determine the length of the word SUBROUTINE.
      ISUBLEN   = LENSTR( 'SUBROUTINE' )
C
C---  Read a line from the MODULE.FOR.
  100 READ(ID_INMOD,'(A)',END=9990) STRING1(1:72) 
C
C---  If the line is a comment, ignore it and go back and read the
C---  next line.
      IF( STRING1(1:1) .EQ. 'C' .OR. STRING1(1:1) .EQ. 'c' .OR.
     1    STRING1(1:1) .EQ. '!' .OR. STRING1(1:1) .EQ. '*' ) GOTO 100
C
C---  Find the length of string1
      LEN_STRING1 = LENSTR( STRING1(1:72) )
      IF( LEN_STRING1 .LT. 7 ) GOTO 100
C      
C---  Read the next line and see if its a comment.  If it is, read until
C---  a FORTRAN statement is found.
      READ(ID_INMOD,'(A)',END=200) TMPSTRING1(1:72)
      DO WHILE( TMPSTRING1(1:1) .EQ. 'C' .OR. 
     1          TMPSTRING1(1:1) .EQ. 'c' .OR.
     2          TMPSTRING1(1:1) .EQ. '!' .OR. 
     3          TMPSTRING1(1:1) .EQ. '*' ) 
         READ(ID_INMOD,'(A)',END=200) TMPSTRING1(1:72)
      ENDDO
      BACKSPACE( ID_INMOD )
C            
C---  Check for a tab in columns 1 through 6.
      ITAB = INDEX( STRING1(1:6), CHAR(9) )
      IF( ITAB .GT. 0 ) THEN
C
C---     A tab character was found, replace it with 6 spaces.
	  DO WHILE( ITAB .GT. 0 )
	    STRING1(ITAB:ITAB) = ' '
	    ITAB = INDEX( STRING1(1:6), CHAR(9) )
	  ENDDO
C
	  STRING2 = '     ' // STRING1(1:67)
	  STRING1(1:72) = STRING2
C
      ENDIF
C
      IEXCLAM = INDEX( STRING1(1:72), '!' )
      IF( IEXCLAM .GT. 0 ) STRING1 = STRING1(1:IEXCLAM-1)
C
      LEN_STRING1 = LENSTR( STRING1(1:72) ) 
      IF( LEN_STRING1 .LT. 1 ) GOTO 100     
C
C
C---  Read the next line and make sure its not a continuation.
  110 MORE_DATA = .FALSE.
      READ( ID_INMOD, '(A)', END=200) STRING2
      MORE_DATA = .TRUE.
      IF( STRING2(1:1) .NE. 'C' .AND. STRING2(1:1) .NE. 'c' .AND.
     1    STRING2(1:1) .NE. '!' .AND. STRING2(1:1) .NE. '*' ) THEN
C
C---     The string is not a comment, check for tabs and continuations.
        ITAB = INDEX( STRING2(1:6), CHAR(9) )
	  IF( STRING2(6:6) .EQ. ' ' .OR. ITAB .GT. 0 ) THEN
	    BACKSPACE( ID_INMOD )
	  ELSE
	    IEXCLAM = INDEX( STRING2, '!' )
	    IF( IEXCLAM .GT. 0 ) STRING2 = STRING2(1:IEXCLAM-1)
	    IFIRST  = NXT_CHAR(7,STRING2)
	    ILAST   = LENSTR( STRING2 )
	    STRING1 = STRING1(1:LEN_STRING1 ) // STRING2(IFIRST:ILAST)
	    LEN_STRING1 = LEN_STRING1 + ( ILAST - IFIRST + 1 )
	    GOTO 110
	  ENDIF 
C	   
	ELSE 
C	
        GOTO 110
C
      ENDIF
C
  200 CONTINUE
C
C---  If the line is a a debug line ignore it and go back and read the
C---  next line.
      IF( STRING1(1:1) .EQ. 'D' .OR. STRING1(1:1) .EQ. 'd' ) GOTO 100
C
      WRITE_STRING = .TRUE.
C
C---  Convert the string to all caps.
      CALL STR_UPCASE( STRING1(1:LEN_STRING1), STRING1(1:LEN_STRING1) )
C
C---  Check to see if the word "SUBROUTINE" is in the string.
      IFIRST = NXT_CHAR( 7, STRING1(1:LEN_STRING1) )
      IF( STRING1(IFIRST:IFIRST+ISUBLEN) .EQ. 'SUBROUTINE ' ) THEN
C
C---    If the word SUBROUTINE is the first word in the string then
C---    a new subroutine was started.  Close the last file and open a
C---    new one with the new subroutine name as the file name if the
C---    subroutine is not ARDCMA or ARDCMM.
C
	  IF( NFILES .GT. 0 ) THEN
C
C---      If NFILES is greater than 0, then a file was previously
C---      opened and it needs to be closed.
	    CLOSE( ID_OUTMOD )
C
	    FILE_INFO(NFILES,1) = COMMON_FOUND
	    FILE_INFO(NFILES,2) = DOUBLE_FOUND
	    COMMON_FOUND = .FALSE.
	    DOUBLE_FOUND = .FALSE.
C
	  ENDIF
C
	  TEMP_FILEN = GET_FILENAME( IFIRST, STRING1(1:LEN_STRING1) ) 
c	  WRITE(*,*) TEMP_FILEN  
C
	  IF( TEMP_FILEN .EQ. 'ARDCMA' .OR.
     1      TEMP_FILEN .EQ. 'ARDCMM' ) THEN
C
C---      Don't save these two files because they do not contain
C---      any of the variables we are looking for.  Read past them
C---      to the next subroutine statement.
C
  300     READ(ID_INMOD,'(A)',END=9999) STRING2(1:72)
	    IF(STRING2(1:1) .EQ. 'C' .OR. STRING2(1:1) .EQ. 'c' .OR.
     1       STRING2(1:1) .EQ. '!' .OR. STRING2(1:1) .EQ. '*') GOTO 300
C
	    CALL STR_UPCASE( STRING2, STRING2 )
C
C---      Check to see if a new module is starting.  If not, go
C---      back and read the next line.
	    IFIRST = NXT_CHAR( 7, STRING2 )
	    IF(STRING2(IFIRST:IFIRST+ISUBLEN) .NE. 'SUBROUTINE ') GOTO 300
C
	    TEMP_FILEN = GET_FILENAME( IFIRST, STRING2 )
C
C---      Make sure that the next module is one to save.
	    IF( TEMP_FILEN .EQ. 'ARDCMA' .OR.
     1        TEMP_FILEN .EQ. 'ARDCMM' ) GOTO 300
C
	  ENDIF

	  NFILES = NFILES + 1
	  FILEN(NFILES) = TEMP_FILEN
	  OPEN(ID_OUTMOD,
     1       FILE=FILEN(NFILES)(1:LENSTR(FILEN(NFILES)))//'.EQM',
     2       STATUS='UNKNOWN' )
C
      ELSE
C
C---    Check the string for fortran reserved words.  If the words
C---    COMMON or IMPLICIT DOUBLE PRECISION are found in the string,
C---    set the appropriate flags.  If the string is a RETURN, END,
C---    ENDIF, ELSE, or ELSEIF set the flag not to save them to the
C---    file.  If the string is an IF statement, separate the IF condition
C---    from the statement.
	  CALL CHK_STRING( WRITE_STRING, COMMON_FOUND, DOUBLE_FOUND,
     1                    LEN_STRING1, STRING1(1:LEN_STRING1) )
C
      END IF
C
      IF( WRITE_STRING ) THEN
	  IFIRST = NXT_CHAR( 6, STRING1(1:LEN_STRING1) )
	  CALL WRITE_OUTMOD
     1    ( LEN_STRING1-IFIRST+1, STRING1(IFIRST:LEN_STRING1) )
      ENDIF
C
      IF( MORE_DATA ) GOTO 100
C
  999 CONTINUE
C
C---  Error messages.
 9990 CLOSE( ID_OUTMOD )                        ! Close the last file.
      FILE_INFO(NFILES,1) = COMMON_FOUND
      FILE_INFO(NFILES,2) = DOUBLE_FOUND
C
 9999 CLOSE( ID_INMOD )   ! Close the MODULE.FOR file.
C
      RETURN
      END
      SUBROUTINE VAR_USAGE( THESTRING )
C-----------------------------------------------------------------------
C     This module determines how the variable is used in the string
C     read from the module file.
C-----------------------------------------------------------------------
C
      COMMON /FILE_IDS/ID_INMOD, ID_OUTMOD, ID_MOD(0:10), ID_OUT, ID_VAR
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
C
      CHARACTER*(*) THESTRING
C
C---  Look for key words at the beginning of the line.
      IKEY = LOOK_4_KEYS( THESTRING )
C
      IF( IKEY .GT. 0 ) THEN
C
	  GOTO( 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000 ) IKEY
C
C---    The variable was found in a SUBROUTINE statement.
  100   CALL DO_SUBROUTINE( IKEY, THESTRING )
	  RETURN
C
C---    The variable was found in a FUNCTION statement.
  200   CALL DO_FUNCTION( IKEY )
	  RETURN
C
C---    The variable was found in a DIMENSION statement.  We need
C---    to determine the dimension.
  300   CALL DO_DIMENSION( IKEY, THESTRING )
	  RETURN
C
C---    The variable was found in a REAL statement.  We need to
C---    determine if it was dimensioned in the statement.
  400   CALL DO_REAL( IKEY, THESTRING )
	  RETURN
C
C---    The variable was found in a DOUBLE PRECISION statement.  We
C---    need to determine if it was dimensioned in the statement.
  500   CALL DO_DOUBLE_PREC( IKEY, THESTRING )
	  RETURN
C
C---    The variable was found in a INTEGER statement.  We need to
C---    determine if it was dimensioned in the statement.
  600   CALL DO_INTEGER( IKEY, THESTRING )
	  RETURN
C
C---    The variable was found in a EQUIVALENCE statement.
  700   CALL DO_EQUIVALENCE( IKEY, THESTRING )
	  RETURN
C
C---    The variable was found in a DATA statement.
  800   CALL DO_DATA( IKEY )
	  RETURN
C
C---    The variable was in a subroutine call.
  900   CALL DO_CALL( IKEY, THESTRING )
	  RETURN
C
C---    The variable was in an IF statement.
 1000   CALL DO_IF( IKEY )
	  RETURN
C
      ELSE
C
	  IEQUAL = INDEX( THESTRING, '=' )
	  IF( IEQUAL .GT. 0 ) THEN
	    IF( LOC_IN_STR(LEVEL) .LT. IEQUAL ) THEN
	      IVAR_INFO(11,LEVEL) = 1
	    ELSE
	      IVAR_INFO(12,LEVEL) = 1
	    ENDIF
	  ENDIF
C
      ENDIF

      RETURN
      END
      SUBROUTINE UTILITY_MOD( UTIL_MOD, L_SUBNAME, SUBNAME, THESTRING )
C----------------------------------------------------------------------
C     This module determines if the call statement calls a utility
C     module.  Utility modules start with MAT, VEC, CAD and TAB.  If
C     the module is a utiltity, then it is checked to see if the
C     variable is being initialized by the utility.
C-----------------------------------------------------------------------
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      COMMON /FILE_IDS/ID_INMOD, ID_OUTMOD, ID_MOD(0:10), ID_OUT, ID_VAR
C
      CHARACTER*(*) THESTRING
      CHARACTER SUBNAME*15
      LOGICAL UTIL_MOD
C
      UTIL_MOD = .FALSE.
      IEND = LENSTR( THESTRING )
C
C---  If the module is a utility, determine if the variable is being
C---  initialized in the call statement. If the utility starts with
C---  the letters TAB, the variable is being initialized if it is the
C---  the last argument of the call statement.  For the other utility
C---  modules, the ones that start with MAT, VEC or CAD, variable is
C---  being initialized if the variable is the first argument of the
C---  call statement.
      IF( SUBNAME(1:3) .EQ. 'TAB' ) THEN
C
C---    A table look up utility; if the variable is the last
C---    argument, it is being initialized. If it is the last
C---    argument, there will be no comma after the location of
C---    of the VAR_NAME.
	  UTIL_MOD = .TRUE.
	  ICOMMA = INDEX( THESTRING(LOC_IN_STR(LEVEL):IEND ), ',' )
	  IF( ICOMMA .EQ. 0 ) THEN
	    L_SUBNAME = L_SUBNAME + 1
	    SUBNAME(L_SUBNAME:L_SUBNAME) = '='
	  ENDIF
C
      ELSE IF( SUBNAME(1:3) .EQ. 'MAT' .OR. SUBNAME(1:3) .EQ. 'VEC' .OR.
     1         SUBNAME(1:3) .EQ. 'CAD' ) THEN
C
C---    The remaining utilities; if the variable is the first
C---    argument, it is being initialized. If the variable is
C---    the the first argument, then there should be no commas
C---    prior to the location of the variable in the string.
	  UTIL_MOD = .TRUE.
	  ICOMMA = INDEX( THESTRING(1:LOC_IN_STR(LEVEL) ), ',' )
	  IF( ICOMMA .EQ. 0 ) THEN
	    L_SUBNAME = L_SUBNAME + 1
	    SUBNAME(L_SUBNAME:L_SUBNAME) = '='
	  ENDIF
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE SAVE_INFO
C-----------------------------------------------------------------------
C     This module saves the information obtained for the subroutines
C     called for current variable and file.
C
C     IVAR_INFO(12,10)
C         contains information on the current variable in reference
C         to the current file.
C         IVAR_INFO(1)  = var is argument for subroutine              (0/1)
C         IVAR_INFO(2)  = var is argument for function                (0/1)
C         IVAR_INFO(3)  = var dimension;   (0=not in dimension stmnt) (0/dim)
C         IVAR_INFO(4)  = real size        (0=not a real)             (0/1)
C         IVAR_INFO(5)  = double precision                            (0/1)
C         IVAR_INFO(6)  = integer size     (0=not an int)             (0/1)
C         IVAR_INFO(7)  = var is equivalenced                         (c loc)
C         IVAR_INFO(8)  = var in data statement                       (0/1)
C         IVAR_INFO(9)  = var in call statement                       (sub #)
C         IVAR_INFO(10) = var is in a IF statement                    (0/1)
C         IVAR_INFO(11) = var is on left side of =                    (0/1)
C         IVAR_INFO(12) = var is on right side of =                   (0/1)
C-----------------------------------------------------------------------
C
      COMMON /FILE_DAT/ LEV_FILE(10), NFILES, INFILE_TYPE,
     1                  FILE_INFO(100,3), FILEN(100)
      LOGICAL           FILE_INFO
      CHARACTER                          FILEN*8
C
      COMMON /FILE_IDS/ID_INMOD, ID_OUTMOD, ID_MOD(0:10), ID_OUT, ID_VAR
C
      COMMON /SAVEINFO/ SAVE_STRING(10), ISAVE(10), ISAVE_COUNT
      CHARACTER         SAVE_STRING*80
C
      COMMON /SUB_LST/  L_SUBS(0:10), SUBS(0:10)
      CHARACTER                      SUBS*80
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      CHARACTER TEMP_STRING*80
C
      DATA ISAVE_COUNT / 0 /
C
      TEMP_STRING = '        '
      ITEMP = 1
C
      IF( IVAR_INFO(12,LEVEL) .NE. 0 ) THEN              ! Assignment
	  TEMP_STRING(ITEMP:ITEMP) = '='
	  ITEMP = ITEMP + 1
      ENDIF
C
      IF( IVAR_INFO(10,LEVEL) .NE. 0 ) THEN              ! IF statement
	  TEMP_STRING(ITEMP:ITEMP) = '>'
	  ITEMP = ITEMP + 1
      ENDIF
C
      IF( IVAR_INFO(1,LEVEL) .NE. 0 ) THEN               ! Sub argument
	  TEMP_STRING(ITEMP:ITEMP) = '['
	  ITEMP = ITEMP + 1
      ENDIF
C
      L_FILEN = LENSTR( FILEN(LEV_FILE(LEVEL)) )
      TEMP_STRING(ITEMP:ITEMP+L_FILEN-1) =
     1                       FILEN(LEV_FILE(LEVEL))(1:L_FILEN)
      ITEMP = ITEMP + L_FILEN
C
      IF( VAR_NAME(LEVEL) .NE. VAR_NAME(LEVEL-1) ) THEN
	  LVAR = LENSTR( VAR_NAME(LEVEL) )
	  TEMP_STRING(ITEMP:ITEMP+LVAR+1-1 ) =
     1             ':' // VAR_NAME(LEVEL)(1:LVAR)
	  ITEMP = ITEMP + LVAR + 1
      ENDIF
C
      IF( IVAR_INFO(1,LEVEL) .NE. 0 ) THEN               ! Sub argument
	  TEMP_STRING(ITEMP:ITEMP) = ']'
	  ITEMP = ITEMP + 1
      ENDIF
C
      IF( IVAR_INFO(11,LEVEL) .NE. 0 )  THEN             ! Initialization
	  TEMP_STRING(ITEMP:ITEMP) = '='
	  ITEMP = ITEMP + 1
      ENDIF
C
      IF(     IVAR_INFO(4,LEVEL) .NE. 0 ) THEN           !  Variable type
	  ITEMP = ITEMP + 1
	  TEMP_STRING(ITEMP:ITEMP) = 'R'
	  ITEMP = ITEMP + 1
      ELSEIF( IVAR_INFO(5,LEVEL) .NE. 0 ) THEN
	  ITEMP = ITEMP + 1
	  TEMP_STRING(ITEMP:ITEMP+1) = 'DP'
	  ITEMP = ITEMP + 2
      ELSEIF( IVAR_INFO(6,LEVEL) .NE. 0 ) THEN
	  ITEMP = ITEMP + 1
	  TEMP_STRING(ITEMP:ITEMP) = 'I'
	  ITEMP = ITEMP + 1
      ELSE
	  IF( INDEX( 'IJKLMN', VAR_NAME(LEVEL)(1:1) ) .EQ. 0 ) THEN
	    IF( FILE_INFO(LEV_FILE(LEVEL),2) ) THEN
	      ITEMP = ITEMP + 1
	      TEMP_STRING(ITEMP:ITEMP+1) = 'DP'
	      ITEMP = ITEMP + 2
	    ELSE
	      ITEMP = ITEMP + 1
	      TEMP_STRING(ITEMP:ITEMP) = 'R'
	      ITEMP = ITEMP + 1
	    ENDIF
	  ELSE
	   ITEMP = ITEMP + 1
	   TEMP_STRING(ITEMP:ITEMP) = 'I'
	   ITEMP = ITEMP + 1
	  ENDIF
      ENDIF
C
C
      IF( IVAR_INFO(3,LEVEL) .NE. 0 ) THEN                ! Dimension
	  L_DIM = LENSTR( VAR_DIM(LEVEL) )
	  TEMP_STRING(ITEMP:ITEMP+L_DIM-1) = VAR_DIM(LEVEL)(1:L_DIM)
	  ITEMP = ITEMP + L_DIM
      ENDIF
C
      IF( IVAR_INFO(7,LEVEL) .NE. 0 ) THEN               ! Equivalence
C
	  ITEMP = ITEMP + 1
	  TEMP_STRING(ITEMP:ITEMP+1) = 'C('
	  ITEMP = ITEMP + 2
	  WRITE(TEMP_STRING(ITEMP:ITEMP+3),'(I4.4)' ) IVAR_INFO(7,LEVEL)
	  ITEMP = ITEMP + 4
	  TEMP_STRING(ITEMP:ITEMP) = ')'
	  ITEMP = ITEMP + 1
C	  
      ENDIF
C         
      IF( IVAR_INFO(8,LEVEL) .NE. 0 ) THEN                ! Data statement
	  ITEMP = ITEMP + 1
	  TEMP_STRING(ITEMP:ITEMP) = 'D'
	  ITEMP = ITEMP + 1
      ENDIF
C
      IF( IVAR_INFO(9,LEVEL) .NE. 0 ) THEN        ! Call statements
C
	  ITEMP = ITEMP + 1
	  TEMP_STRING(ITEMP:ITEMP+L_SUBS(LEVEL)) =
     1              SUBS(LEVEL)(1:L_SUBS(LEVEL))
	  ITEMP = ITEMP + L_SUBS(LEVEL)
C
      ENDIF
C
      TEMP_STRING(ITEMP:ITEMP) = ';'
C
      ISAVE_COUNT = ISAVE_COUNT + 1
C
      ISAVE(LEVEL)       = ITEMP
      SAVE_STRING(LEVEL) = TEMP_STRING
C
      RETURN
      END
      SUBROUTINE WRITE_INFO( J )
C-----------------------------------------------------------------------
C     This module write the information obtained for the current
C     variable and file.
C
C     IVAR_INFO(12)
C         contains information on the current variable in reference
C         to the current file.
C         IVAR_INFO(1)  = var is argument for subroutine              (0/1)
C         IVAR_INFO(2)  = var is argument for function                (0/1)
C         IVAR_INFO(3)  = var dimension;   (0=not in dimension stmnt) (0/dim)
C         IVAR_INFO(4)  = real size        (0=not a real)             (0/1)
C         IVAR_INFO(5)  = double precision                            (0/1)
C         IVAR_INFO(6)  = integer size     (0=not an int)             (0/1)
C         IVAR_INFO(7)  = var is equivalenced                         (c loc)
C         IVAR_INFO(8)  = var in data statement                       (0/1)
C         IVAR_INFO(9)  = var in call statement                       (sub #)
C         IVAR_INFO(10) = var is in a IF statement                    (0/1)
C         IVAR_INFO(11) = var is on left side of =                    (0/1)
C         IVAR_INFO(12) = var is on right side of =                   (0/1)
C-----------------------------------------------------------------------
C
      COMMON /FILE_DAT/ LEV_FILE(10), NFILES, INFILE_TYPE,
     1                  FILE_INFO(100,3), FILEN(100)
      LOGICAL           FILE_INFO
      CHARACTER                          FILEN*8
C
      COMMON /FILE_IDS/ID_INMOD, ID_OUTMOD, ID_MOD(0:10), ID_OUT, ID_VAR
C
      COMMON /OUTINFO/  OUTSTRING, IOUT, LEN_OUT
      CHARACTER         OUTSTRING*80
C
      COMMON /SAVEINFO/ SAVE_STRING(10), ISAVE(10), ISAVE_COUNT
      CHARACTER         SAVE_STRING*80
C
      COMMON /SUB_LST/ L_SUBS(0:10), SUBS(0:10)
      CHARACTER                      SUBS*80
C
      COMMON /VAR_DAT/ LEVEL, IVAR1, IVAR2, IVAR_INFO(12,0:10),
     1                 LOC_IN_C(0:10), LOC_IN_STR(0:10),
     2                 VAR_NAME(0:10), VAR_DIM(0:10)
      CHARACTER        VAR_NAME*8,     VAR_DIM*5
C
      PARAMETER( HEADER=0 )
C
      DATA IOUT/ 1 /
      DATA OUTSTRING /
     1'                                                               '/
C
      IF( IVAR_INFO(12,LEVEL) .NE. 0 ) THEN                      ! Assignment
	  OUTSTRING(IOUT:IOUT) = '='
	  IOUT = IOUT + 1
      ENDIF
C
      IF( IVAR_INFO(10,LEVEL) .NE. 0 ) THEN                      ! IF stmnt
	  OUTSTRING(IOUT:IOUT) = '>'
	  IOUT = IOUT + 1
      ENDIF
C
      IF( IVAR_INFO(1,LEVEL) .NE. 0 ) THEN                       ! Sub argument
	  OUTSTRING(IOUT:IOUT) = '['
	  IOUT = IOUT + 1
      ENDIF
C
      L_FILEN = LENSTR( FILEN(J) )
      OUTSTRING(IOUT:IOUT+L_FILEN-1) = FILEN(J)(1:L_FILEN)
      IOUT = IOUT + L_FILEN
C
      IF( IVAR_INFO(1,LEVEL) .NE. 0 ) THEN                       ! Sub argument
	  OUTSTRING(IOUT:IOUT) = ']'
	  IOUT = IOUT + 1
      ENDIF
C
      IF( IVAR_INFO(11,LEVEL) .NE. 0 )  THEN                     ! Initialization
	  OUTSTRING(IOUT:IOUT) = '='
	  IOUT = IOUT + 1
      ENDIF
C
C--   Write the string out if the length is over LEN_OUT characters.
      IF( IOUT .GT. LEN_OUT ) THEN
	  IF( INFILE_TYPE .EQ. HEADER ) THEN
	    WRITE(ID_OUT,'(27X,A)') OUTSTRING(1:IOUT)
	  ELSE  ! INFILE_TYPE .EQ. ALFA
	    WRITE(ID_OUT,'(22X,A)') OUTSTRING(1:IOUT)
	  ENDIF
	  IOUT = 1
	  OUTSTRING = ' '
      ENDIF
C
      IF(     IVAR_INFO(4,LEVEL) .NE. 0 ) THEN                   !  Variable type
C---    Put a space if not a beginning of line.
	  IF( IOUT .GT. 1 ) IOUT = IOUT + 1
	  OUTSTRING(IOUT:IOUT) = 'R'
	  IOUT = IOUT + 1
      ELSEIF( IVAR_INFO(5,LEVEL) .NE. 0 ) THEN
C---    Put a space if not a beginning of line.
	  IF( IOUT .GT. 1 ) IOUT = IOUT + 1
	  OUTSTRING(IOUT:IOUT+1) = 'DP'
	  IOUT = IOUT + 2
      ELSEIF( IVAR_INFO(6,LEVEL) .NE. 0 ) THEN
C---    Put a space if not a beginning of line.
	  IF( IOUT .GT. 1 ) IOUT = IOUT + 1
	  OUTSTRING(IOUT:IOUT) = 'I'
	  IOUT = IOUT + 1
      ELSE
	  IF( INDEX( 'IJKLMN', VAR_NAME(LEVEL)(1:1) ) .EQ. 0 ) THEN
	    IF( FILE_INFO(J,2) ) THEN
C---        Put a space if not a beginning of line.
	      IF( IOUT .GT. 1 ) IOUT = IOUT + 1
	      OUTSTRING(IOUT:IOUT+1) = 'DP'
	      IOUT = IOUT + 2
	    ELSE
C---        Put a space if not a beginning of line.
	      IF( IOUT .GT. 1 ) IOUT = IOUT + 1
	      OUTSTRING(IOUT:IOUT) = 'R'
	      IOUT = IOUT + 1
	    ENDIF
	  ELSE
C---      Put a space if not a beginning of line.
	    IF( IOUT .GT. 1 ) IOUT = IOUT + 1
	    OUTSTRING(IOUT:IOUT) = 'I'
	    IOUT = IOUT + 1
	  ENDIF
      ENDIF
C
C
      IF( IVAR_INFO(3,LEVEL) .NE. 0 ) THEN                       ! Dimension
	  L_DIM = LENSTR( VAR_DIM(LEVEL) )
	  OUTSTRING(IOUT:IOUT+L_DIM-1) = VAR_DIM(LEVEL)(1:L_DIM)
	  IOUT = IOUT + L_DIM
      ENDIF
C
      IF( IVAR_INFO(7,LEVEL) .NE. 0 ) THEN                       ! Equivalence
C
C---    Write the string out if the length is over LEN_OUT characters.
	  IF( IOUT .GT. LEN_OUT ) THEN
	    IF( INFILE_TYPE .EQ. HEADER ) THEN
	      WRITE(ID_OUT,'(27X,A)') OUTSTRING(1:IOUT)
	    ELSE  ! INFILE_TYPE .EQ. ALFA
	      WRITE(ID_OUT,'(22X,A)') OUTSTRING(1:IOUT)
	    ENDIF
	    IOUT = 1
	    OUTSTRING = ' '
	  ENDIF
C
C---    Put a space if not a beginning of line.
	  IF( IOUT .GT. 1 ) IOUT = IOUT + 1
	  OUTSTRING(IOUT:IOUT+1) = 'C('
	  IOUT = IOUT + 2
	  WRITE(OUTSTRING(IOUT:IOUT+3),'(I4.4)' ) IVAR_INFO(7,LEVEL)
	  IOUT = IOUT + 4
	  OUTSTRING(IOUT:IOUT) = ')'
	  IOUT = IOUT + 1
C	  
      ENDIF
C
      IF( IVAR_INFO(8,LEVEL) .NE. 0 ) THEN                       ! Data statement
C---    Put a space if not a beginning of line.
	  IF( IOUT .GT. 1 ) IOUT = IOUT + 1
	  OUTSTRING(IOUT:IOUT) = 'D'
	  IOUT = IOUT + 1
      ENDIF
C
      IF( IVAR_INFO(9,LEVEL) .NE. 0 ) THEN
C
C---    Write the string out if the length is over LEN_OUT characters.
	  IF( IOUT .GT. LEN_OUT ) THEN
	    IF( INFILE_TYPE .EQ. HEADER ) THEN
	      WRITE(ID_OUT,'(27X,A)') OUTSTRING(1:IOUT)
	    ELSE ! INFILE_TYPE .EQ. ALFA
	      WRITE(ID_OUT,'(22X,A)') OUTSTRING(1:IOUT)
	    ENDIF
	    IOUT = 1
	    OUTSTRING = ' '
	  ELSE
C---      Put a space if not a beginning of line.
	    IOUT = IOUT + 1
	  ENDIF
C
  100   CONTINUE
C
	  IF( (IOUT + L_SUBS(LEVEL)) .LE. LEN_OUT ) THEN
C
C---      All the subroutines fit on the output line.
	    OUTSTRING(IOUT:IOUT+L_SUBS(LEVEL)) = 
     1           SUBS(LEVEL)(1:L_SUBS(LEVEL))
	    IOUT = IOUT + L_SUBS(LEVEL)
C
	  ELSE
C
C---      All the subroutines won't fit on the line; split up the
C---      SUBS string adding enough subroutines to the output line
C---      until it is over LEN_OUT characters long.
	    ICOMMA = INDEX( SUBS(LEVEL)(1:L_SUBS(LEVEL)), ',' )
	    IEND   = ICOMMA
	    DO WHILE( (IOUT + IEND) .LT. LEN_OUT .AND. ICOMMA .GT. 0 )
	      ICOMMA = INDEX( SUBS(LEVEL)(IEND+1:L_SUBS(LEVEL)), ',' )
	      IEND   = ICOMMA + IEND
	    ENDDO
C
	    IF( ICOMMA .GT. 0 ) THEN
C
C---        More than one subroutine is left in SUBS.  Add the
C---        ones that will fit and go back and start this block over
C---        in case the remaining subroutines are still longer than
C---        LEN_OUT characters long.
	      OUTSTRING(IOUT:IOUT+IEND) = SUBS(LEVEL)(1:IEND)
	      IF( INFILE_TYPE .EQ. HEADER ) THEN
	        WRITE(ID_OUT,'(27X,A)') OUTSTRING(1:IOUT+IEND)
	      ELSE  ! INFILE_TYPE .EQ. ALFA
	        WRITE(ID_OUT,'(22X,A)') OUTSTRING(1:IOUT+IEND)
	      ENDIF
	      IOUT = 1
	      OUTSTRING = ' '
	      SUBS(LEVEL) = SUBS(LEVEL)(IEND+1:L_SUBS(LEVEL))
	      L_SUBS(LEVEL) = LENSTR( SUBS(LEVEL) )
	      GOTO 100
	    ELSE
	      OUTSTRING(IOUT:IOUT+L_SUBS(LEVEL)-1) =
     1             SUBS(LEVEL)(1:L_SUBS(LEVEL))
	      IOUT = IOUT + L_SUBS(LEVEL)
	    ENDIF
C
	  ENDIF
C
      ENDIF
C
      OUTSTRING(IOUT:IOUT+2) = ';  '
      IOUT = IOUT + 2
C
C---  Write the string out if the length is over LEN_OUT characters.
      IF( IOUT .GT. LEN_OUT ) THEN
	  IF( INFILE_TYPE .EQ. HEADER ) THEN
	    WRITE(ID_OUT,'(27X,A)') OUTSTRING(1:IOUT)
	  ELSE ! INFILE_TYPE .EQ. ALFA
	    WRITE(ID_OUT,'(22X,A)') OUTSTRING(1:IOUT)
	  ENDIF
	  IOUT = 1
	  OUTSTRING = ' '
      ENDIF
C
C---  Write the information for the subroutine calls in the current module.
C---  (Find a good break in the subroutine call data by looking for a space.
C---  If it happens that the first character of the last word in the
C---  SAVE_STRING falls less than the cutoff and the last character falls
C---  after the cutoff, set IEND to the length of SAVE_STRING.)
C
C      12345678901234567890        IOUT     45    45  45
C      [S1]= R C() TEST;           ILEN      6    8   12
C              1234567890123                51    53  57
C
      IF( ISAVE_COUNT .GT. 0 ) THEN
C
	  DO I = 1, ISAVE_COUNT
	    IF( IOUT+ISAVE(I) .GT. LEN_OUT ) THEN
C
            IEND = 0
            ISPACE = 1
            DO WHILE( IOUT+IEND .LT. LEN_OUT .AND. ISPACE .NE. 0)
              ISPACE = INDEX( SAVE_STRING(I)(IEND+1:ISAVE(I)),' ' )
              IEND = IEND + ISPACE
            END DO
C
            IF( ISPACE .GT. 0 ) THEN
              OUTSTRING(IOUT:IOUT+IEND-1) = SAVE_STRING(I)(1:IEND)
              IF( INFILE_TYPE .EQ. HEADER ) THEN
                WRITE(ID_OUT,'(27X,A)') OUTSTRING(1:IOUT+IEND-1)
              ELSE ! INFILE_TYPE .EQ. ALFA
                WRITE(ID_OUT,'(22X,A)') OUTSTRING(1:IOUT+IEND-1)
              ENDIF
              IEND = NXT_CHAR(IEND,SAVE_STRING(I))
              OUTSTRING = SAVE_STRING(I)(IEND:ISAVE(I))
              IOUT = LENSTR( OUTSTRING ) + 1
            ELSE
              OUTSTRING(IOUT:IOUT+ISAVE(I)-1)=SAVE_STRING(I)(1:ISAVE(I))
              IOUT = IOUT + ISAVE(I) + 1
            ENDIF
C
	    ELSE
C
	      OUTSTRING(IOUT:IOUT+ISAVE(I)-1) = SAVE_STRING(I)(1:ISAVE(I))
	      IOUT = IOUT + ISAVE(I) + 1
C
	    ENDIF
C
	  END DO
	  ISAVE_COUNT = 0
C	  
      ENDIF
C
      RETURN
      END
      SUBROUTINE WRITE_OUTMOD( LEN_THESTRING, THESTRING )
C-----------------------------------------------------------------------
C    This module writes THESTRING (where THESTRING is a statement that
C    contains a statemetn to look for during the cross reference) to
C    the new modules.
C-----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
C
      COMMON /FILE_IDS/ID_INMOD, ID_OUTMOD, ID_MOD(0:10), ID_OUT, ID_VAR
C
C
      IF( LEN_THESTRING .LE. 500 ) THEN
C
	  WRITE(ID_OUTMOD,'(I4,1X,A)') LEN_THESTRING, THESTRING
C
      ELSE
C
	  WRITE(ID_OUTMOD,'(I4,1X,A,A)')
     1            LEN_THESTRING, THESTRING(1:500), ' &&'
C
	  IFIRST  = 501
	  ILAST   = MIN( IFIRST+500, LEN_THESTRING )
	  DO WHILE ( ILAST .LT. LEN_THESTRING )
	    WRITE(ID_OUTMOD,'(A,A)') THESTRING(IFIRST:ILAST), ' &&'
	    IFIRST = ILAST + 1
	    ILAST = MIN( IFIRST+500, LEN_THESTRING )
	  END DO
	  WRITE(ID_OUTMOD,'(A)') THESTRING(IFIRST:ILAST)
C
      ENDIF
C
      RETURN
      END
      LOGICAL FUNCTION EXIT_PROG()
C--------------------------------------------------------------------
C     This function issues a message if an error occurs while opening
C     a file and it gives the user the chance to exit the program.
C--------------------------------------------------------------------
C
      CHARACTER REPLY*1
C
      WRITE(*,'(/5X,A//5X,A/5X,A\)' )
     1        '    * * * Invalid Filename * * *',
     2        '       Do you wish to continue? (Y or N)',
     3        '       Default = Y : '
C
      READ(*,'(A)') REPLY
C
      IF( REPLY .EQ. 'Y' .OR. REPLY .EQ. 'y' .OR.
     1    LENSTR(REPLY) .EQ. 0 ) THEN
	  EXIT_PROG = .FALSE.
      ELSE
	  EXIT_PROG = .TRUE.
      ENDIF 
C      
      RETURN
      END
      CHARACTER*8 FUNCTION GET_FILENAME( ISUB, THESTRING )
C--------------------------------------------------------------------
C     This function determines a file name for the subroutine included
C     on the string passed into the function.
C--------------------------------------------------------------------
C
      CHARACTER THESTRING*(*)
C
C---  Find the location of the first and last characters of the 
C---  subroutine name.
C
      ISTART = NXT_CHAR( ISUB + 10, THESTRING )
      ILAST  = INDEX( THESTRING, '(' )
      IF( ILAST .EQ. 0 ) THEN
C
C---    No arguments are included with this module, the last character
C---    of the subroutine name should be the last character on the line.
	  ILAST = LENSTR( THESTRING )
C
      ELSE
C
C---    The subroutine has an argument list, the last character of
C---    the subroutine name is the last character prior to the "(".
	  ILAST = LENSTR( THESTRING(1:ILAST-1) )
C
      ENDIF
C
      GET_FILENAME = THESTRING(ISTART:ILAST)
C
      RETURN
      END
      FUNCTION LENSTR( THESTRING )
C----------------------------------------------------------------------
C  This function searches the text contained in the string variable for
C  the end of the text.  The function returns the character location
C  of the last non-blank character location.  This is useful in
C  locating the end of text within a string.  The module will work
C  with any length input string.
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
      FUNCTION LOCATE_VAR( VNAME, THESTRING )
C-----------------------------------------------------------------------
C     This function determines if a variable name is found in the
C     current string read from the current file.
C-----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
      CHARACTER VNAME*8, TEST_STRING*10
C
      DATA TEST_STRING / ' *()-+=/,.' /
C
      ISTART = 1
      ILAST = LENSTR( THESTRING )
  100 IFIND=INDEX(THESTRING(ISTART:ILAST),VNAME(1:LENSTR(VNAME)))
      IF( IFIND .GT. 0 ) THEN
C---    Find the actual location of the variable in the string. Since
C---    its possible to be looking at a portion of the string, add ISTART-1
C---    to IFIND to get it.
	  IFIND = IFIND + ISTART - 1
C
C---    Find the location in the string of the last character of the variable
	  IEND = IFIND + (LENSTR( VNAME ) - 1)
C
C---    The letters in the variable were found in the string, make sure
C---    its the variable that is in the string.  This means that the only
C---    characters that can come before or after the variable are:
C---    * ( ) - + = / , space
C
	  IPRIOR = 1
	  IF( IFIND .GT. 1 ) IPRIOR =
     1          INDEX( TEST_STRING, THESTRING(IFIND-1:IFIND-1) )
	  IF( IPRIOR .GT. 0 ) THEN
C
C---      The character prior to the variable in the string is valid.
C---      Check the character after the variable:
	    IF( IEND .LT. ILAST ) THEN
C
C---        Need to make sure the character following the varible is valid.
	      IAFTR = INDEX( TEST_STRING, THESTRING(IEND+1:IEND+1) )
	      IF( IAFTR .EQ. 0 ) THEN
C
C---          If it is determined that the occurrence of the letters
C---          of the variable was not a reference to the variable, make
C---          sure that the variable is not found later in the string.
C---          ie, if the current  variable is:  ALPHA
C---          and the string is:                ALPHAP = ALPHA * 2
C---          we need to find the actual occurrence of ALPHA.
		       ISTART = IEND + 1
		       GOTO 100
C
	      ENDIF
C
	    ENDIF
C
	  ELSE
C
C---      If it is determined that the occurrence of the letters
C---      of the variable was not a reference to the variable, make
C---      sure that the variable is not found later in the string.
C---      ie, if the current  variable is:  ALPHA
C---      and the string is:                PALPHA = ALPHA * 2
C---      we need to find the actual occurrence of ALPHA.
	    IF( IEND .LT. ILAST ) THEN
	      ISTART = IEND + 1
	      GOTO 100
	    ELSE
	      IFIND = 0
	    ENDIF
C
	  ENDIF
C
      ENDIF
C
      LOCATE_VAR = IFIND
C
      RETURN
      END
      FUNCTION LOOK_4_KEYS( THESTRING )
C-----------------------------------------------------------------------
C     This function checks the beginning of the string passed in for
C     fortran keywords that can contain reference to the current
C     variable.
C-----------------------------------------------------------------------
C
      CHARACTER*(*) THESTRING
      CHARACTER  KEYWORDS(11)*16
      LOGICAL KEYFOUND
C
      DATA KEYWORDS / 'SUBROUTINE ',  'FUNCTION ', 'DIMENSION ',
     1                'REAL ',        'DOUBLE ',   'INTEGER ',
     2                'EQUIVALENCE ', 'DATA ',     'CALL ',
     2                'IF ', 'IF(' /
      DATA NUM_KEYS / 10 / , IDOUBLE / 5 /

      IBEG = NXT_CHAR( 1, THESTRING )
      J = 0
      KEYFOUND = .FALSE.
      DO WHILE ( J .LT. NUM_KEYS .AND. .NOT. KEYFOUND )
	  J = J + 1
	  KEY_LENGTH = LENSTR( KEYWORDS(J) )
	  IF( (IBEG+KEY_LENGTH) .LT. LENSTR( THESTRING) )
     1        KEYFOUND = ( THESTRING(IBEG:IBEG+KEY_LENGTH) .EQ.
     2                                 KEYWORDS(J)(1:KEY_LENGTH+1) )
      ENDDO
C
      IF( .NOT. KEYFOUND ) THEN
C
C---    Since the IF statement may or may not have a space after it,
C---    check for IF( as the first three characters of the string.
	  KEYFOUND = ( THESTRING(IBEG:IBEG+2) .EQ. 'IF(' )
	  IF( KEYFOUND ) J = 10
C
      ENDIF
C
C
      IF( KEYFOUND ) THEN
C
	  IF( J .EQ. IDOUBLE ) THEN
C---      If the keyword DOUBLE was found, make sure the next
C---      word is PRECISION.
	    IBEG = NXT_CHAR( IBEG+KEY_LENGTH, THESTRING )
	    IF(THESTRING(IBEG:IBEG+8) .EQ. 'PRECISION' ) THEN
	      LOOK_4_KEYS = J
	    ELSE
	      LOOK_4_KEYS = 0
	    ENDIF
	  ELSE
	    LOOK_4_KEYS = J
	  ENDIF
      ELSE
	  LOOK_4_KEYS = 0
      ENDIF
C      
      RETURN
      END
      FUNCTION NXT_CHAR(IN_POS, THESTRING)
C--------------------------------------------------------------------
C  This function returns the location of the next non-blank character
C  in a string.  The module will work with any length input string.
C----------------------------------------------------------------------
C  INSTRING  - (I) Input.  The position in the string to start looking
C                  for the next character.
C  THESTRING - (C) Input.  The character string to be searched for the
C                  next non-blank character.
C  NXT_CHAR -  (I) The location of the next non-blank character in
C                  THESTRING.
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
      SUBROUTINE STR_UPCASE(LINEIN,LINEOUT)
C----------------------------------------------------------------------
C     This function converts a string to all uppercase letters.
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