C***********************************************************************
C************************ CADAC UTILITIES ******************************
C***************************** UTL.FOR *********************************
C***********************************************************************
C***  *
C***  * MATxxx, VECxxx, TABLx,and CADxxx Utility Subroutines.
C***  *
C***  * MODIFICATION HISTORY
C***  * 970430 Modified VECANG to prevent rare math overflow problem, PZi
C***  * 970501 Version 2.0, Released with CADX2.FOR, Version 2
C***  * 970619 Corrections in MATADD and MATSUB, PZi
C***  * 980529 RANF Function added for RANF calls with FNGAUSS routine, bc
C***  * 991122 Added new table look-up TBLPx routines with pointers, PZi
C***  * 000204 Added subroutine US76 std atmosphere, PZi
C***  * 000412 Included CADxx utilities from UTL8.FOR, renamed UTL3.FOR, PZi
C***  *
C***  *****************************************************************
      SUBROUTINE MATADD (C,A,B,N1,N2)
C
C*** C(N1*N2) IS OBTAINED FROM ADDING A(N1*N2) TO B(N1*N2)
C
C970619 K=N2*(J-1)+I relaced by  K=N1*(J-1)+I
C
      REAL A(1),B(1),C(1)

      DO  J=1,N2
         DO  I=1,N1
            K=N1*(J-1)+I
            C(K)=A(K)+B(K)
         ENDDO
      ENDDO
      RETURN
      END
C
      SUBROUTINE MATSUB (C,A,B,N1,N2)
C
C*** C(N1*N2) IS OBTAINED FROM SUBSTRACTING B(N1*N2) FROM A(N1*N2)
C
C970619 K=N2*(J-1)+I relaced by  K=N1*(J-1)+I
C

      REAL A(1),B(1),C(1)
C
      DO  J=1,N2
         DO  I=1,N1
            K=N1*(J-1)+I
            C(K)=A(K)-B(K)
         ENDDO
      ENDDO
      RETURN
      END
C
      SUBROUTINE MATMUL (C,A,B,N1,N2,N3)
C
C*** C(N1*N3) IS OBTAINED FROM MULTIPLYING A(N1*N2) BY B(N2*N3)
C
      REAL A(1),B(1),C(1)
C
      DO I=1,N1
         DO K=1,N3
            N=N1*(K-1)+I
            C(N)=0.0
            DO J=1,N2
               L=N1*(J-1)+I
               M=N2*(K-1)+J
               C(N)=C(N)+A(L)*B(M)
            ENDDO
         ENDDO
      ENDDO
      RETURN
      END
C
      SUBROUTINE MATSCA (D,V1,V2,N)
C
C*** D IS THE SCALAR PRODUCT OF VECTOR V1(N*1) AND V2(N*1)
C
      REAL V1(1),V2(1)
C
      D=0.
      DO 1 I=1,N
1     D=D+V1(I)*V2(I)
      RETURN
      END

C
C          SUBROUTINE MATINV
C
C          PURPOSE
C             COMPUTE THE INVERSE OF A NON-SINGLAR MATRIX
C
C          USAGE
C             CALL MATINV(AI,A,D,N)
C
C          DESCRIPTION OF PARAMETERS
C             A - NAME OF INPUT MATRIX
C             AI- NAME OF OUTPUT MATRIX (MAY BE THE SAME AS A)
C             D - DETERMINANT OF A
C             N - NUMBER OF ROWS AND COLUMNS OF A
C
C          REMARKS
C             ALL MATRICES ARE STORED AS GENERAL MATRICES
C
C          SUBROUTINES AND FUNCTION SUBPROGRAMS RQUIRED
C             SYSTEM ROUTINE -MATRIX-
C
C          METHOD
C        SET UP TO CALL GAUSS-JORDAN INVERSION ROUTINE.
C
C     ..................................................................
      SUBROUTINE MATINV(AI,A,D,N)
      DIMENSION A(1),AI(1)
C
C     SET UP TO CALL ROUTINE THAT ACTUALLY DOES THE CONVERSION
C
CDC      INTEGER ZZZ112(3)
CDC      DATA ZZZ112/L"MATRIX IS ",L"SINGULAR  ",L" "/
      MX = N
      M = N
      EPSIL = 0.000 000 000 1
C
C         COPY A MATRIX INTO AI MATRIX
C
      N2=N*N
      DO 1 I = 1, N2
1     AI(I)=A(I)
C
      CALL MATFS(EPSIL,MX,M,AI,IER,DET)
      D=DET
      IF(IER.EQ.0) RETURN
      D=0
      PRINT *,' MATRIX IS SINGULAR '
      X=-2
      Y=SQRT(X)**2
      RETURN
      END
      SUBROUTINE MATFS(EPSIL,MX,M,A,IER,DET)
C      MATFS IS A SINGLE-PRECISION MATRIX INVERSION SUBROUTINE
C     WRITTEN IN FORTRAN IV  - VARIABLE DIMENSIONING IS USED
C     OF MAX DIMENSIONS OF MATRIX IN CSLLING PROGRAM MUST BE SAME
C     VALUE AS PASSED ON BY CALLING SEQUENCE AS MX.
C     MATRIX CAN BE OF GENERAL FORM. METHOD IS GAUSS-JORDAN.
      DIMENSION F(30),G(30),L(30),BETA(30),TEMP(30),A(MX,MX)
      INTEGER  I,J,K,KP,F,BETA,L,G,PHI,M,FKP,LDA,GKP
      REAL  MU
C     INITIALIZE AND LOGICAL OPERATIONS ON COLUMN K, PIVOT COL.
      DET=1.0
      IER =0
      DO 5 KP =1,M
      F(KP) = 0
      BETA(KP) = 0
      L(KP)=KP
      G(KP)=KP
    5 CONTINUE
C     TRANSFORM BY COLUMNS, K
      DO 32 K = 1, M
C     FIND LARGEST ELEMENT IN COL K WITH BETA STILL ZERO
      MU=0.0
      DO 14 I = 1,M
      IF (BETA(I)) 1000,10,14
   10 IF ( ABS(A(I,K)) -  ABS(MU))  14,14,12
C     LABEL ROW WITH LARGEST ELEMENT (MU) AS PHI
   12 MU = A(I,K)
      PHI = I
   14 CONTINUE
C     IF ELEMENT MU IS TOO SMALL,MATRIX IS SINGULAR-CALL EXIT
   15 IF ( ABS(MU) - EPSIL) 16,16,18
 16   IER =1
      RETURN
   18 DET = MU*DET
C     SETF(K) WITH INDEX OF ROW, BETA OF ROW NOW 1
      F(K) = PHI
      BETA(PHI) = 1
C     ARITHMETIC OPERATIONS ON PIVOT COLUMN,K
C     FIRST SAVE ENTIRE COLUMN IN PRESENT FORM
   20 DO 21 I=1,M
      TEMP(I) = A(I,K)
   21 CONTINUE
C     TRANSFORM COLUMN
      A(PHI,K) = 1.0/MU
      DO 23 I=1,M
      IF (I - PHI)  22,23,22
   22 A(I,K) = -A(I,K)/MU
   23 CONTINUE
C     LOGICAL OPERATIONS ON OTHER COLUMNS    PANEL IV
C     NOTE WE USE ORIG SAVED VALUES FROM PIVOT COLUMN,K
      DO 31 J =1,M
C     TEST TO SKIP COL K, ALREADY TRANSFORMED
      IF (J-K)  25,31,25
C     NOT K   IF VALUE IN ROW PHI IS 0 SKIP COLUMN
   25 IF (A(PHI,J)) 26,31,26
C     NOT ZERO
   26 ALPHA = A(PHI,J)
      A(PHI,J) = ALPHA/MU
C     TRANSFORM IN ONE COL,J, BY ROW INDICES
      DO 30 I =1,M
C     TEST IF ELEMENT IS IN ROW WITH PIVOT ELEMENT
      IF (I-PHI)  28,30,28
C     NO
   28 A(I,J) = A(I,J) - ALPHA*TEMP(I) /MU
C     YES
   30 CONTINUE
   31 CONTINUE
   32 CONTINUE
C     PERMUTATIONS OF ROWS AND COLUMNS
      MM=M-1
      DO 55 KP=1,MM
C     PERMUTATION OF ROWS
      IF(G(KP)-F(KP)) 50,55,50
   50 FKP = F(KP)
      LDA = L(FKP)
      DO 51 J =1,M
      TEMP(J) = A(KP,J)
      A(KP,J) = A(LDA,J)
      A(LDA,J) = TEMP(J)
      DET = -DET
   51 CONTINUE
C     PERMUTATION OF COLUMNS
      GKP = G(KP)
      DO 53 I = 1,M
      TEMP(I) = A(I,FKP)
      A(I,FKP) = A(I,GKP)
      A(I,GKP) = TEMP(I)
C     RESET INDICES
      L(FKP) = K
       L(GKP) = LDA
      G(LDA) = GKP
      G(KP) = FKP
   53 CONTINUE
   55 CONTINUE
      RETURN
 1000  WRITE (6,1001)
      STOP
 1001 FORMAT (22H ERROR, BETA NEGATIVE )
C     INVERTED MATRIX IS IN CELLS ORIGINALLY OCCUPIED BY MATRIX
C     DETERMINANT IS IN CELL DET (NOT RETURNED)
   60 RETURN
      END
C
      SUBROUTINE MATCON (C,CON,A,N1,N2)
C
C*** C(N1*N2) IS OBTAINED FROM MULTIPLYING COSTANT CON BY A(N1*N2)
C
      REAL A(1),C(1)
C
      DO  J=1,N2
         DO  I=1,N1
            K=N2*(J-1)+I
            C(K)=A(K)*CON
         ENDDO
      ENDDO
      RETURN
      END                
C
      SUBROUTINE MATTRA (C,A,N1,N2)
C
C*** C(N2*N1) IS THE TRANSPOSE OF A(N1*N2)
C
      DIMENSION A(N1,N2),C(N2,N1)
C
      DO 1 J=1,N2
      DO 1 I=1,N1
1     C(J,I)=A(I,J)
      RETURN
      END
C
      SUBROUTINE MATTRF(C,T,A,N)
C
C*** C(N*N) IS OBTAINED FROM A(N*N) BY THE ORTHOGONAL TRANSFORMATION
C    MATRIX T(N*N)    C = T * A * T(TRANSPOSE)
C
      DIMENSION C(N,N),T(N,N),A(N,N)
C
      DO 1 I=1,N
      DO 1 J=1,N
      C(I,J)=0.
      DO 1 K=1,N
      DO 1 L=1,N
 1    C(I,J)=C(I,J)+T(I,K)*A(K,L)*T(J,L)
C
      RETURN
      END
C
      SUBROUTINE MATSKS (C,V)
C
C*** SKEW-SYMMETRIC MATRIX C(3*3) IS OBTAINED FROM VECTOR C(3*1)
C
      DIMENSION C(3,3),V(3)
C
      C(1,1)=0.
      C(1,2)=-V(3)
      C(1,3)=V(2)
      C(2,1)=V(3)
      C(2,2)=0.
      C(2,3)=-V(1)
      C(3,1)=-V(2)
      C(3,2)=V(1)
      C(3,3)=0.
C
      RETURN
      END
C
      SUBROUTINE MATVSK (V,A)
C
C*** VECTOR V(3*1) IS OBTAINED FROM SKEW-SYMMETRIC MATRIX A(3*3)
C
      DIMENSION A(3,3),V(3)
C
      V(1)=-A(2,3)
      V(2)=A(1,3)
      V(3)=-A(1,2)
C
      RETURN
      END
C
      SUBROUTINE MATZER (A,N1,N2)
C
C*** CREATION OF A ZERO MATRIX A(N1*N2)
C
      REAL A(1)
C     
      L=N1*N2
      DO  I=1,L 
          A(I)=0.
      ENDDO
      RETURN
      END
C
      SUBROUTINE MATUNI (A,N)
C
C*** CREATION OF A UNIT MATRIX A(N*N)
C
      REAL A(1)
C
      DO  J=1,N
          DO  I=1,N
              K=N*(J-1) + I
              A(K)=0.
          ENDDO
          K=N*(J-1) + J
          A(K)=1.
      ENDDO
      RETURN
      END
C
      SUBROUTINE MATDIA (A,D,N)
C
C*** A(N*N) IS THE DIAGONAL MATRIX OBTAINED FROM VECTOR D(N*1)
C
      DIMENSION D(N),A(N,N)
C
      DO 2 J=1,N
      DO 1 I=1,N
1     A(I,J)=0.
2     A(J,J)=D(J)
      RETURN
      END
C
C
      SUBROUTINE MATVDI (D,A,N)
C
C*** VECTOR D(N*1) IS THE DIAGONAL OF MATRIX A(N*N)
C
      DIMENSION D(N),A(N,N)
C
      DO 1 I=1,N
 1    D(I)=A(I,I)
C
      RETURN
      END
C
      SUBROUTINE MATEQL (C,A,N1,N2)
C
C*** C(N1*N2) IS SET EQUAL TO A(N1*N2)
C
      REAL A(1),C(1)
C     
      L=N1*N2
      DO  I=1,L
          C(I)=A(I)
      ENDDO
      RETURN
      END
C
      SUBROUTINE MATABS (D,V,N)
C
C*** D IS THE ABSOLUTE VALUE OF VECTOR V(N*1)
C
      REAL V(1)
C
      DV=0.
      DO 1 I=1,N
1     DV=DV+V(I)*V(I)
      D=SQRT(DV)
      RETURN
      END        
C
      SUBROUTINE MATCAR(SBEL,DBE,AZBEL,ELBEL)
C
C*** CARTESIAN COORDINATES SBEL(3X1) FROM POLAR COORDINATES
C
      REAL SBEL(1)
C
      CELB=COS(ELBEL)
      SELB=SIN(ELBEL)
      CAZB=COS(AZBEL)
      SAZB=SIN(AZBEL)
C
      SBEL(1)=DBE*CELB*CAZB
      SBEL(2)=DBE*CELB*SAZB
      SBEL(3)=-DBE*SELB
C
      RETURN
      END
      SUBROUTINE MATPOL(DBE,AZBEL,ELBEL,SBEL)
C
C*** POLAR COORDINATES FROM CARTESIAN COORDINATES
C
      REAL SBEL(1)
C
      IF(AMAX1(ABS(SBEL(1)),ABS(SBEL(2))).LT.1E-10) THEN
        AZBEL=0.
        ELBEL=0.
        DUM1=0.
      ELSE
        AZBEL=ATAN2(SBEL(2),SBEL(1))
        DUM1=SBEL(1)*SBEL(1)+SBEL(2)*SBEL(2)
        DUM2=SQRT(DUM1)
        ELBEL=ATAN2(-SBEL(3),DUM2)
      ENDIF
      DUM3=DUM1+SBEL(3)*SBEL(3)
      DBE=SQRT(DUM3)
C
      RETURN
      END
      SUBROUTINE MAT2TR(T,PSI,THT)
C
C*** T IS THE TRANSFORMATION MATRIX OBTAINED FROM TWO
C*** ANGLE TRANSFORMATIONS PSI AND THT
C
      DIMENSION T(3,3)
C
      T(1,3)=-SIN(THT)
      T(2,1)=-SIN(PSI)
      T(2,2)=COS(PSI)
      T(3,3)=COS(THT)
      T(1,1)=T(3,3)*T(2,2)
      T(1,2)=-T(3,3)*T(2,1)
      T(3,1)=-T(1,3)*T(2,2)
      T(3,2)=T(1,3)*T(2,1)
      T(2,3)=0.
C
      RETURN
      END
      SUBROUTINE MAT3TR(T,PSI,THT,PHI)
C
C*** T IS THE TRANSFORMATION MATRIX OBTAINED FROM THREE
C*** ANGLE TRANSFORMATIONS PSI, THT AND PHI (IN THIS SEQUENCE)
C*** EULER ANGLES OF FLIGHT MECHANICS
C
      DIMENSION T(3,3)
C
      CPSI=COS(PSI)
      SPSI=SIN(PSI)
      CTHT=COS(THT)
      STHT=SIN(THT)
      CPHI=COS(PHI)
      SPHI=SIN(PHI)
      T(1,1)=CPSI*CTHT
      T(1,2)=SPSI*CTHT
      T(1,3)=-STHT
      T(2,1)=-SPSI*CPHI+CPSI*STHT*SPHI
      T(2,2)=CPSI*CPHI+SPSI*STHT*SPHI
      T(2,3)=CTHT*SPHI
      T(3,1)=SPSI*SPHI+CPSI*STHT*CPHI
      T(3,2)=-CPSI*SPHI+SPSI*STHT*CPHI
      T(3,3)=CTHT*CPHI
C
      RETURN
      END      
C
      SUBROUTINE MAT3EU(T,PSI,ALAM,OME)
C
C*** TRANSF. MATRIX OF THE THREE EULER ANGLES PSI,ALAM,OME (IN THIS SEQ.)
C*** EULER ANGLES OF GYRODYNAMICS, NOT FLIGHT MECHANICS.
C
      DIMENSION T(3,3)
C
      COME=COS(OME)
      SOME=SIN(OME)
      CALAM=COS(ALAM)
      SALAM=SIN(ALAM)
      CPSI=COS(PSI)
      SPSI=SIN(PSI)

      SC=SOME*CALAM
      CC=COME*CALAM
C
      T(1,1)=COME*CPSI-SC*SPSI
      T(1,2)=COME*SPSI+SC*CPSI
      T(1,3)=SOME*SALAM
      T(2,1)=-SOME*CPSI-CC*SPSI
      T(2,2)=-SOME*SPSI+CC*CPSI
      T(2,3)=COME*SALAM
      T(3,1)=SALAM*SPSI
      T(3,2)=-SALAM*CPSI
      T(3,3)=CALAM
C
      RETURN
      END
C
      SUBROUTINE MATROT(R,A,B)
C
C*** ROTATION TENSOR R(3X3) FROM ANGLE OF ROTATION A AND 
C    VECTOR OF ROTATION B(3X1)
C
      DIMENSION R(3,3),B(3)
C
      CA=COS(A)
      SA=SIN(A)
      OCA=1.-CA
      B12OCA=B(1)*B(2)*OCA
      B13OCA=B(1)*B(2)*OCA
      B23OCA=B(2)*B(3)*OCA
C
      R(1,1)=CA+B(1)*B(1)*OCA
      R(1,2)=B12OCA-B(3)*SA
      R(1,3)=B13OCA+B(2)*SA
      R(2,1)=B12OCA+B(3)*SA
      R(2,2)=CA+B(2)*B(2)*OCA
      R(2,3)=B23OCA-B(1)*SA
      R(3,1)=B13OCA-B(2)*SA
      R(3,2)=B23OCA+B(1)*SA
      R(3,3)=CA+B(3)*B(3)*OCA
C
      RETURN
      END                     
C
C*** CHOLESKY DECOMPOSITION. LOWER TRIANGULAR MATRIX A(N,N) WHICH IS THE 
C    SQUARE ROOT OF MATRIX P(N,N)
C
	SUBROUTINE MATCHO(A,P,N)
C
	DIMENSION A(N,N),P(N,N)
C
	DO 10 I=1,N
	DO 10 J=1,N
C
	IF(J.LT.I)THEN
	SUM=0.
	IF(J.GT.1)THEN
	DO 20 K=1,J-1
 20	SUM=SUM+A(I,K)*A(J,K)
	END IF
	IF(A(J,J).EQ.0.)THEN
	A(I,J)=0.
	ELSE
	A(I,J)=(P(I,J)-SUM)/A(J,J)
	END IF
C
	ELSE IF(J.EQ.I)THEN
	SUM=0.
	IF(I.GT.1)THEN
	DO 30 K=1,I-1
 30	SUM=SUM+A(I,K)**2
	END IF
	A(I,J)=SQRT(P(I,I)-SUM)
C
	ELSE
	A(I,J)=0.
C
	END IF
C
 10	CONTINUE
C
	RETURN
	END
C
C***********************************************************************
C****************** VECTOR UTILITIES ***********************************
C***********************************************************************                               
      SUBROUTINE VECUCR(Z,X,Y)
C
C*** PERFORMS UNIT CROSS PRODUCT OF 3-VECTORS
C     NAMELY: Z=UNIT(X cross Y)
C
      DIMENSION X(3),Y(3),Z(3)
      Z(1) = X(2)*Y(3)-X(3)*Y(2)
      Z(2) = X(3)*Y(1)-X(1)*Y(3)
      Z(3) = X(1)*Y(2)-X(2)*Y(1)
      ZM=FNRSS(Z,3)
      Z(1)=Z(1)/ZM
      Z(2)=Z(2)/ZM
      Z(3)=Z(3)/ZM
      RETURN
      END
C
      SUBROUTINE VECVEC(D,A,B,C)
C
C*** CONSTRUCT VECTOR D FROM ELEMENTS A,B,C
C
      DIMENSION   D(3)
      D(1)=A
      D(2)=B
      D(3)=C
      RETURN
      END
C
      SUBROUTINE VECUVC(D,A,B,C)
C
C*** CONSTRUCT UNIT VECTOR FROM ELEMENTS A,B,C
C
      DIMENSION   D(3)
      CALL VECVEC(D,A,B,C)
      RSSD=FNRSS(D,3)
      D(1)=A/RSSD
      D(2)=B/RSSD
      D(3)=C/RSSD
      RETURN
      END
C
      SUBROUTINE VECANG(A,B,C)
C
C*** ANGLE A(RAD) BETWEEN VECTOR B(3X1) AND C(3X1) 
C
      DIMENSION B(3),C(3)
      SCA=B(1)*C(1)+B(2)*C(2)+B(3)*C(3)
      BSQ=B(1)*B(1)+B(2)*B(2)+B(3)*B(3)
      CSQ=C(1)*C(1)+C(2)*C(2)+C(3)*C(3)
      DUMM=BSQ*CSQ
CZI970430      IF(DUMM.NE.0.) THEN
      IF(DUMM.GT.1.E-5) THEN
         DUM=SCA/SQRT(DUMM)
      ELSE
         DUM=1.
      ENDIF
      IF(ABS(DUM).GT.1.) DUM=SIGN(1.,DUM)
      A=ACOS(DUM)
      RETURN
      END
C
      FUNCTION FNRSS(R,N)
C
C*** FUNCTION CONSTRUCTS ROOT-SUM-SQUARE OF N-VECTOR R
C
      DIMENSION R(N)
      SUM=0.
      DO 1 I=1,N
 1    SUM=SUM+R(I)**2
      FNRSS=SQRT(SUM)
      RETURN
      END
C
C******************************************************************
      FUNCTION FNGAUS(XMEAN,SIGMA)
C
C*** GAUSSIAN DISTRIBUTION 
C
      V1=RANF()
      V2=RANF()
      FNGAUS=SIGMA*SQRT(2.*LOG(1./V1))*COS(6.2831853072*V2)+XMEAN
      END
C
C**********************************************************************
C*** Random Number Function Generator                               ***
C
      REAL*4 FUNCTION RANF()
      CALL RANDOM( RVALUE )
      DO WHILE( RVALUE .EQ. 0.0 )
          CALL RANDOM( RVALUE )
      ENDDO
      RANF = RVALUE
      RETURN
      END
C***********************************************************************
C*************** TABLE LOOK-UP UTILITIES *******************************
C***********************************************************************
      SUBROUTINE TABLE(X,XI,YI,NX,Y)
C
      REAL XI(1), YI(1)
      IF(X.LT.XI(1))X=XI(1)
      IF(X.GT.XI(NX))X=XI(NX)
      DO 10 I = 2, NX
        IF(X .LE. XI(I)) GO TO 20
10      CONTINUE
      I = NX
20    PCT = (X - XI(I-1)) / (XI(I) - XI(I-1))
      Y = YI(I-1) + PCT * (YI(I) - YI(I-1))
      RETURN
      END
C
      SUBROUTINE TABL2 (X,Y,XY,ZI,NXY,Z)
C
      REAL XY(1), ZI(1), T(2)
      INTEGER NXY(2)
      NX = NXY(1)
      NY = NXY(2)
      IF(X.LT.XY(1))X=XY(1)
      IF(X.GT.XY(NX))X=XY(NX)
      IF(Y.LT.XY(NX+1))Y=XY(NX+1)
      IF(Y.GT.XY(NX+NY))Y=XY(NX+NY)
      DO 10 M = 2,NY
        J = NX + M
        IF (Y .LE. XY(J)) GO TO 20
10      CONTINUE
        M = NY
20      PCTY = (Y - XY(J-1)) / (XY(J) - XY(J-1))
      DO 30 I = 2,NX
        IF(X .LE. XY(I)) GO TO 40
30      CONTINUE
        I = NX
40      PCTX = (X - XY(I-1)) / (XY(I) - XY(I-1))
      DO 50 K = 1,2
        KY = NX * (M - K) + I
50      T(K) = ZI(KY-1) + PCTX * (ZI(KY) - ZI(KY-1))
      Z = T(2) + PCTY * (T(1) - T(2))
      RETURN
      END
C
      SUBROUTINE TABL3 (X,Y,Z,XYZ,ZW,NXYZ,W)
C
      REAL XYZ(1), ZW(1), T(2,2), S(2)
      INTEGER NXYZ(3)
      NX = NXYZ(1)
      NY = NXYZ(2)
      NZ = NXYZ(3)
      IF(X.LT.XYZ(1))X=XYZ(1)
      IF(X.GT.XYZ(NX))X=XYZ(NX)
      IF(Y.LT.XYZ(NX+1))Y=XYZ(NX+1)
      IF(Y.GT.XYZ(NX+NY))Y=XYZ(NX+NY)
      IF(Z.LT.XYZ(NX+NY+1))Z=XYZ(NX+NY+1)
      IF(Z.GT.XYZ(NX+NY+NZ))Z=XYZ(NX+NY+NZ)
      DO 10 I = 2,NX
        IF(X .LE. XYZ(I)) GO TO 20
10      CONTINUE
        I = NX
20      PCTX = (X - XYZ(I-1)) / (XYZ(I) - XYZ(I-1))
      DO 30 J = 2,NY
        L = NX + J
        IF(Y .LE. XYZ(L)) GO TO 40
30      CONTINUE
        J = NY
40      PCTY = (Y - XYZ(L-1)) / (XYZ(L) - XYZ(L-1))
      NXY = NX + NY
      DO 50 K = 2,NZ
        M = NXY + K
        IF(Z .LE. XYZ(M)) GO TO 60
50      CONTINUE
        K = NZ
60    IF((XYZ(M)-XYZ(M-1)).GT.1.E-10)GO TO 65
      PCTZ=0.0
      GO TO 67
65      PCTZ = (Z - XYZ(M-1)) / (XYZ(M) - XYZ(M-1))
67    NXNY = NX * NY
      DO 80 JJ = 1,2
        JZ = NXNY * (K - JJ)
        DO 70 KK = 1,2
          KY = JZ + NX * (J - KK) + I
70        T(KK,JJ) = ZW(KY-1) + PCTX * (ZW(KY) - ZW(KY-1))
80        CONTINUE
      DO 90 N = 1,2
90      S(N) = T(2,N) + PCTY * (T(1,N) - T(2,N))
      W = S(2) + PCTZ * (S(1) - S(2))
      RETURN
      END
C
      SUBROUTINE TABL4(X,Y,Z,W,XYZW,WW,NXYZW,ANS)
C
      REAL XYZW(1), WW(1), T(2,2,2), R(2,2), S(2)
      INTEGER NXYZW(4)
      NX = NXYZW(1)
      NY = NXYZW(2)
      NZ = NXYZW(3)
      NW = NXYZW(4)
      IF(X.LT.XYZW(1))X=XYZW(1)
      IF(X.GT.XYZW(NX))X=XYZW(NX)
      IF(Y.LT.XYZW(NX+1))Y=XYZW(NX+1)
      IF(Y.GT.XYZW(NX+NY))Y=XYZW(NX+NY)
      IF(Z.LT.XYZW(NX+NY+1))Z=XYZW(NX+NY+1)
      IF(Z.GT.XYZW(NX+NY+NZ))Z=XYZW(NX+NY+NZ)
      IF(W.LT.XYZW(NX+NY+NZ+1))W=XYZW(NX+NY+NZ+1)
      IF(W.GT.XYZW(NX+NY+NZ+NW))W=XYZW(NX+NY+NZ+NW)
      DO 10 I = 2,NX
        IF( X .LE. XYZW(I)) GO TO 20
10      CONTINUE
        I = NX
20      PCTX = (X - XYZW(I-1)) / (XYZW(I) - XYZW(I-1))
      DO 30 J = 2,NY
        L = NX + J
        IF( Y .LE. XYZW(L)) GO TO 40
30      CONTINUE
        J = NY
40      PCTY = (Y - XYZW(L-1)) / (XYZW(L) - XYZW(L-1))
      NXY = NX + NY
      DO 50 K = 2,NZ
        M = NXY + K
        IF(Z .LE. XYZW(M)) GO TO 60
50      CONTINUE
        K = NZ
60      PCTZ = (Z - XYZW(M-1)) / (XYZW(M) - XYZW(M-1))
      NXYZ = NXY + NZ
      DO 70 L = 2,NW
        N = NXYZ + L
        IF(W .LE. XYZW(N)) GO TO 80
70      CONTINUE
        L = NW
80      PCTW = (W - XYZW(N-1)) / (XYZW(N) - XYZW(N-1))
      NXNY = NX * NY
      NXNZ = NXNY * NZ
      DO 110 LL = 1,2
        LLL = NXNZ * (L-LL)
        DO 100 KK = 1,2
          KKK = LLL + NXNY * (K-KK)
      DO 92 JJ=1,2
            JJJ = KKK + NX * (J-JJ) + I
      T(JJ,KK,LL)=WW(JJJ-1)+PCTX*(WW(JJJ)-WW(JJJ-1))
 92   CONTINUE
100       CONTINUE
110     CONTINUE
      DO 130 LT = 1,2
        DO 120 KT = 1,2
120       R(KT,LT) = T(2,KT,LT) + PCTY * (T(1,KT,LT) - T(2,KT,LT))
130     CONTINUE
      DO 140 N = 1,2
140     S(N) = R(2,N) + PCTZ * (R(1,N) - R(2,N))
      ANS = S(2) + PCTW * (S(1) - S(2))
      RETURN
      END
C
C***********************************************************************
C*************** TABLE LOOK-UP WITH POINTERS ***************************
C***********************************************************************
C
C#######################################################################
      SUBROUTINE TBLP1(X,IX,XT,MX,YT,Y,YD)
C
C     VERSION 1.1   CONFIGURATION # 5.20
C#######################################################################
C
C     TBLP1 IS A GENERAL ONE-DIMENSIONAL TABLE LOOK-UP WITH POINTERS TO
C     ENHANCE SPEED
C
C
C     INPUTS:
C
C       X  - INDEPENDENT VARIABLE
C       IX - POINTER TO LAST PLACE IN TABLE WHERE LOOKUP OCCURED FOR X
C       XT - ARRAY OF INDEPENDENT VARIABLE VALUES (MX VALUES)
C       MX - MAXIMUN OF INDEPENDENT VARIABLE VALUES IN TABLE
C       YT - MATRIX OF DEPENDENT VARIABLES (MX VALUES)
C
C
C     OUTPUTS:
C
C       Y  - DEPENDENT VALUE FROM TABLE LOOK UP
C       YD - SLOPE OF Y WITH RESPECT TO THE INDEPENDENT VARIABLE
C
C#######################################################################
C
      DIMENSION XT(MX),YT(MX)
      LOGICAL ASCND
C
C--FIND POINTER FOR INDEPENDENT VARIABLE
C
      ASCND = XT(MX).GE.XT(1)
      IX = MAX0(1,MIN0(IX,MX))
      IF (X.GT.XT(IX).EQV.ASCND) THEN
C
C--SEARCH UP FOR INDEPENDENT VARIABLE
C
	DO 10  I=IX+1,MX
	  IF (XT(I).GT.X.EQV.ASCND) THEN
	    IX = I-1
	    GO TO 30
	  END IF
   10   CONTINUE
	IX = MX-1
C
C--SEARCH DOWN FOR INDEPENDENT VARIABLE
C
      ELSE
	DO 20  I=IX-1,1,-1
	  IF (XT(I).LE.X.EQV.ASCND) THEN
	    IX = I
	    GO TO 30
	  END IF
   20   CONTINUE
	IX = 1
      END IF
C
C--NOW HAVE UPDATED POINTER -- DEFINE SOME VARIABLES FOR LATER USE
C
   30 DTX = XT(IX)-XT(IX+1)
      DX  = X-XT(IX)
C99      IF (DTX.EQ.0.D0) THEN
C99      CALL ERROR(2010)
C99      ELSE
C
C--COMPUTE DERIVATIVE OF DATA WITH RESPECT TO INDEPENDENT VARIABLE
C
	YD = (YT(IX)-YT(IX+1))/DTX
C
C--INTERPOLATE
C
	Y = YT(IX) + YD*DX
C99      END IF
      RETURN
      END
C#######################################################################
      SUBROUTINE TBLP2(X1,X2,IX1,IX2,XT1,XT2,MX1,MX2,YT,Y,YD1,YD2)
C
C     VERSION 1.1   CONFIGURATION # 5.21
C#######################################################################
C
C     TBLP2 IS A GENERAL TWO-DIMENSIONAL TABLE LOOK-UP WITH POINTERS TO
C     ENHANCE SPEED
C
C     INPUTS:
C
C       X1  - INDEPENDENT VARIABLE 1
C       X2  - INDEPENDENT VARIABLE 2
C       IX1 - POINTER TO LAST TABLE LOOKUP POSITION FOR X1
C       IX2 - POINTER TO LAST TABLE LOOKUP POSITION FOR X2
C       XT1 - ARRAY OF INDEPENDENT VARIABLE 1 VALUES (MX1 VALUES)
C       XT2 - ARRAY OF INDEPENDENT VARIABLE 2 VALUES (MX2 VALUES)
C       YT  - MATRIX OF DEPENDENT VARIABLES (MX1*MX2 VALUES)
C       MX1 - MAXIMUN OF INDEPENDENT VARIABLE 1 VALUES IN TABLE
C       MX2 - MAXIMUN OF INDEPENDENT VARIABLE 2 VALUES IN TABLE
C
C
C     OUTPUTS:
C
C       Y   - DEPENDENT VALUE FROM TABLE LOOK UP
C       YD1 - SLOPE OF Y WITH RESPECT TO INDEPENDENT VARIABLE 1
C       YD2 - SLOPE OF Y WITH RESPECT TO INDEPENDENT VARIABLE 2
C
C#######################################################################
C
      DIMENSION XT1(MX1),XT2(MX2),YT(MX1,MX2)
      LOGICAL ASCND
C
C--FIND POINTER FOR INDEPENDENT VARIABLE 1
C
      ASCND = XT1(MX1).GE.XT1(1)
      IX1 = MAX0(1,MIN0(IX1,MX1))
      IF (X1.GT.XT1(IX1).EQV.ASCND) THEN
C
C--SEARCH UP FOR INDEPENDENT VARIABLE 1
C
	DO 10  I=IX1+1,MX1
	  IF (XT1(I).GT.X1.EQV.ASCND) THEN
	    IX1 = I-1
	    GO TO 30
	  END IF
   10   CONTINUE
	IX1 = MX1-1
C
C--SEARCH DOWN FOR INDEPENDENT VARIABLE 1
C
      ELSE
	DO 20  I=IX1-1,1,-1
	  IF (XT1(I).LE.X1.EQV.ASCND) THEN
	    IX1 = I
	    GO TO 30
	  END IF
   20   CONTINUE
	IX1 = 1
      END IF
C
C--FIND POINTER FOR INDEPENDENT VARIABLE 2
C
   30 ASCND = XT2(MX2).GE.XT2(1)
      IX2 = MAX0(1,MIN0(IX2,MX2))
      IF (X2.GT.XT2(IX2).EQV.ASCND) THEN
C
C--SEARCH UP FOR INDEPENDENT VARIABLE 2
C
	DO 40  I=IX2+1,MX2
	  IF (XT2(I).GT.X2.EQV.ASCND) THEN
	    IX2 = I-1
	    GO TO 60
	  END IF
   40   CONTINUE
	IX2 = MX2-1
C
C--SEARCH DOWN FOR INDEPENDENT VARIABLE 2
C
      ELSE
	DO 50  I=IX2-1,1,-1
	  IF (XT2(I).LE.X2.EQV.ASCND) THEN
	    IX2 = I
	    GO TO 60
	  END IF
   50   CONTINUE
	IX2 = 1
      END IF
C
C--NOW HAVE UPDATED IX1 AND IX2 -- DEFINE SOME VARIABLES FOR LATER USE
C
   60 DTX1 = XT1(IX1)-XT1(IX1+1)
      DTX2 = XT2(IX2)-XT2(IX2+1)
C99      IF (DTX1.EQ.0.D0.OR.DTX2.EQ.0.D0) THEN
C99	CALL ERROR(2011)
C99      ELSE
      IF (DTX1.EQ.0..OR.DTX2.EQ.0.) GOTO 70
	DX1 = X1-XT1(IX1)
	DX2 = X2-XT2(IX2)
	S1  = (YT(IX1,IX2)-YT(IX1+1,IX2))/DTX1
	S3  = (YT(IX1,IX2+1)-YT(IX1+1,IX2+1))/DTX1
	Y1  = YT(IX1,IX2)+S1*DX1
	Y3  = YT(IX1,IX2+1)+S3*DX1
C
C--COMPUTE DERIVATIVES OF DATA WITH RESPECT TO INDEPENDENT VARIABLES
C
	YD2 = (Y1-Y3)/DTX2
C
C--ERROR FIY MG 2-21-90
C
C        YD1 = (S3-S1)/DTX2*DX2+S1
	YD1 = (S1-S3)/DTX2*DX2+S1
C
C--INTERPOLATE
C
	Y = Y1+YD2*DX2
C99      END IF
   70 CONTINUE
      RETURN
      END
C#######################################################################
      SUBROUTINE TBLP3(X1,X2,X3,IX1,IX2,IX3,XT1,XT2,XT3,MX1,MX2,MX3,
     1                 YT,Y,YD1,YD2,YD3)
C
C     VERSION 1.1   CONFIGURATION # 5.22
C#######################################################################
C
C     TBLP3 IS A GENERAL THREE-DIMENSIONAL TABLE LOOK-UP WITH POINTERS
C     TO ENHANCE SPEED
C
C
C     INPUTS:
C
C       X1  - INDEPENDENT VARIABLE 1
C       X2  - INDEPENDENT VARIABLE 2
C       X3  - INDEPENDENT VARIABLE 3
C       IX1 - POINTER TO LAST TABLE LOOKUP POSITION FOR X1
C       IX2 - POINTER TO LAST TABLE LOOKUP POSITION FOR X2
C       IX3 - POINTER TO LAST TABLE LOOKUP POSITION FOR X3
C       XT1 - ARRAY OF INDEPENDENT VARIABLE 1 VALUES (MX1 VALUES)
C       XT2 - ARRAY OF INDEPENDENT VARIABLE 2 VALUES (MX2 VALUES)
C       XT3 - ARRAY OF INDEPENDENT VARIABLE 3 VALUES (MX3 VALUES)
C       YT  - MATRIX OF DEPENDENT VARIABLES (MX1*MX2*MX3 VALUES)
C       MX1 - MAXIMUM OF INDEPENDENT VARIABLE 1 VALUES IN TABLE
C       MX2 - MAXIMUM OF INDEPENDENT VARIABLE 2 VALUES IN TABLE
C       MX3 - MAXIMUM OF INDEPENDENT VARIABLE 3 VALUES IN TABLE
C
C
C     OUTPUTS:
C
C       Y   - DEPENDENT VALUE FROM TABLE LOOK UP
C       YD1 - SLOPE OF Y WITH RESPECT TO INDEPENDENT VARIABLE 1
C       YD2 - SLOPE OF Y WITH RESPECT TO INDEPENDENT VARIABLE 2
C       YD3 - SLOPE OF Y WITH RESPECT TO INDEPENDENT VARIABLE 3
C
C#######################################################################
C
      DIMENSION XT1(MX1),XT2(MX2),XT3(MX3),YT(MX1,MX2,MX3)
      LOGICAL ASCND
C
C--FIND POINTER FOR INDEPENDENT VARIABLE 1
C
      ASCND = XT1(MX1).GE.XT1(1)
      IX1 = MAX0(1,MIN0(IX1,MX1))
      IF (X1.GT.XT1(IX1).EQV.ASCND) THEN
C
C--SEARCH UP FOR INDEPENDENT VARIABLE 1
C
	DO 10  I=IX1+1,MX1
	  IF (XT1(I).GT.X1.EQV.ASCND) THEN
	    IX1 = I-1
	    GO TO 30
	  END IF
   10   CONTINUE
	IX1 = MX1-1
C
C--SEARCH DOWN FOR INDEPENDENT VARIABLE 1
C
      ELSE
	DO 20  I=IX1-1,1,-1
	  IF (XT1(I).LE.X1.EQV.ASCND) THEN
	    IX1 = I
	    GO TO 30
	  END IF
   20   CONTINUE
	IX1 = 1
      END IF
C
C--FIND POINTER FOR INDEPENDENT VARIABLE 2
C
   30 ASCND = XT2(MX2).GE.XT2(1)
      IX2 = MAX0(1,MIN0(IX2,MX2))
      IF (X2.GT.XT2(IX2).EQV.ASCND) THEN
C
C--SEARCH UP FOR INDEPENDENT VARIABLE 2
C
	DO 40  I=IX2+1,MX2
	  IF (XT2(I).GT.X2.EQV.ASCND) THEN
	    IX2 = I-1
	    GO TO 60
	  END IF
   40   CONTINUE
	IX2 = MX2-1
C
C--SEARCH DOWN FOR INDEPENDENT VARIABLE 2
C
      ELSE
	DO 50  I=IX2-1,1,-1
	  IF (XT2(I).LE.X2.EQV.ASCND) THEN
	    IX2 = I
	    GO TO 60
	  END IF
   50   CONTINUE
	IX2 = 1
      END IF
C
C--FIND POINTER FOR INDEPENDENT VARIABLE 3
C
   60 ASCND = XT3(MX3).GE.XT3(1)
      IX3 = MAX0(1,MIN0(IX3,MX3))
      IF (X3.GT.XT3(IX3).EQV.ASCND) THEN
C
C--SEARCH UP FOR INDEPENDENT VARIABLE 3
C
	DO 70  I=IX3+1,MX3
	  IF (XT3(I).GT.X3.EQV.ASCND) THEN
	    IX3 = I-1
	    GO TO 90
	  END IF
   70   CONTINUE
	IX3 = MX3-1
C
C--SEARCH DOWN FOR INDEPENDENT VARIABLE 3
C
      ELSE
	DO 80  I=IX3-1,1,-1
	  IF (XT3(I).LE.X3.EQV.ASCND) THEN
	    IX3 = I
	    GO TO 90
	  END IF
   80   CONTINUE
	IX3 = 1
      END IF
C
C--NOW HAVE UPDATED IX1, IX2, AND IX3 -- DEFINE SOME VARIABLES FOR LATER
C--USE
C
   90 DTX1 = XT1(IX1)-XT1(IX1+1)
      DTX2 = XT2(IX2)-XT2(IX2+1)
      DTX3 = XT3(IX3)-XT3(IX3+1)
C99      IF (DTX1.EQ.0.D0.OR.DTX2.EQ.0.D0.OR.DTX3.EQ.0.D0) THEN
C99	CALL ERROR(2012)
C99      ELSE
	DX1 = X1-XT1(IX1)
	DX2 = X2-XT2(IX2)
	DX3 = X3-XT3(IX3)
	SZ1 = (YT(IX1,IX2,IX3)    -YT(IX1,IX2,IX3+1))/DTX3
	SZ2 = (YT(IX1+1,IX2,IX3)  -YT(IX1+1,IX2,IX3+1))/DTX3
	SZ3 = (YT(IX1+1,IX2+1,IX3)-YT(IX1+1,IX2+1,IX3+1))/DTX3
	SZ4 = (YT(IX1,IX2+1,IX3)  -YT(IX1,IX2+1,IX3+1))/DTX3
	SY1 = (YT(IX1,IX2,IX3)    -YT(IX1+1,IX2,IX3))/DTX1
	SY2 = (YT(IX1,IX2+1,IX3)  -YT(IX1+1,IX2+1,IX3))/DTX1
	SY3 = (YT(IX1,IX2+1,IX3+1)-YT(IX1+1,IX2+1,IX3+1))/DTX1
	SY4 = (YT(IX1,IX2,IX3+1)  -YT(IX1+1,IX2,IX3+1))/DTX1
	SX1 = (YT(IX1,IX2,IX3)    -YT(IX1,IX2+1,IX3))/DTX2
	SX2 = (YT(IX1+1,IX2,IX3)  -YT(IX1+1,IX2+1,IX3))/DTX2
	SX3 = (YT(IX1+1,IX2,IX3+1)-YT(IX1+1,IX2+1,IX3+1))/DTX2
	SX4 = (YT(IX1,IX2,IX3+1)  -YT(IX1,IX2+1,IX3+1))/DTX2
	Z1T = YT(IX1,IX2,IX3)    +SZ1*DX3
	Z2T = YT(IX1+1,IX2,IX3)  +SZ2*DX3
	Z3T = YT(IX1+1,IX2+1,IX3)+SZ3*DX3
	Z4T = YT(IX1,IX2+1,IX3)  +SZ4*DX3
	Y1T = YT(IX1,IX2,IX3)    +SY1*DX1
	Y2T = YT(IX1,IX2+1,IX3)  +SY2*DX1
	Y3T = YT(IX1,IX2+1,IX3+1)+SY3*DX1
	Y4T = YT(IX1,IX2,IX3+1)  +SY4*DX1
	X1T = YT(IX1,IX2,IX3)    +SX1*DX2
	X2T = YT(IX1+1,IX2,IX3)  +SX2*DX2
	X3T = YT(IX1+1,IX2,IX3+1)+SX3*DX2
	X4T = YT(IX1,IX2,IX3+1)  +SX4*DX2
	Y1N = Z1T+(Z1T-Z2T)/DTX1*DX1
	Y3N = Z4T+(Z4T-Z3T)/DTX1*DX1
	X1N = X1T+(X1T-X4T)/DTX3*DX3
	X3N = X2T+(X2T-X3T)/DTX3*DX3
	Z1N = Y1T+(Y1T-Y2T)/DTX2*DX2
	Z3N = Y4T+(Y4T-Y3T)/DTX2*DX2
	SS1 = (SZ1-SZ2)/DTX1
	SS3 = (SZ4-SZ3)/DTX1
	SY1 = SZ1+SS1*DX1
	SY3 = SZ4+SS3*DX1
C
C--COMPUTE DERIVATIVES OF DATA WITH RESPECT TO INDEPENDENT VARIABLES
C
	YD1 = (X1N-X3N)/DTX1
	YD2 = (Y1N-Y3N)/DTX2
	YD3 = (Z1N-Z3N)/DTX3
C
C--INTERPOLATE
C
	Y = Y1N+YD2*DX2
C99      END IF
      RETURN
      END
C#######################################################################
      SUBROUTINE TBLP4(X1,X2,X3,X4,IX1,IX2,IX3,IX4,XT1,XT2,XT3,XT4,
     1                 MX1,MX2,MX3,MX4,YT,Y,SL1,SL2,SL3,SL4)
C
C#######################################################################
C
C     TBLP4 IS A GENERAL FOUR-DIMENSIONAL TABLE LOOK-UP WITH POINTERS
C     TO ENHANCE SPEED
C
C
C     INPUTS:
C
C       X1  - INDEPENDENT VARIABLE 1
C       X2  - INDEPENDENT VARIABLE 2
C       X3  - INDEPENDENT VARIABLE 3
C       X4  - INDEPENDENT VARIABLE 4
C       IX1 - POINTER TO LAST TABLE LOOKUP POSITION FOR X1
C       IX2 - POINTER TO LAST TABLE LOOKUP POSITION FOR X2
C       IX3 - POINTER TO LAST TABLE LOOKUP POSITION FOR X3
C       IX4 - POINTER TO LAST TABLE LOOKUP POSITION FOR X4
C       XT1 - ARRAY OF INDEPENDENT VARIABLE 1 VALUES (MX1 VALUES)
C       XT2 - ARRAY OF INDEPENDENT VARIABLE 2 VALUES (MX2 VALUES)
C       XT3 - ARRAY OF INDEPENDENT VARIABLE 3 VALUES (MX3 VALUES)
C       XT4 - ARRAY OF INDEPENDENT VARIABLE 4 VALUES (MX4 VALUES)
C       YT  - MATRIX OF DEPENDENT VARIABLES (MX1*MX2*MX3*MX4 VALUES)
C       MX1 - MAXIMUM OF INDEPENDENT VARIABLE 1 VALUES IN TABLE
C       MX2 - MAXIMUM OF INDEPENDENT VARIABLE 2 VALUES IN TABLE
C       MX3 - MAXIMUM OF INDEPENDENT VARIABLE 3 VALUES IN TABLE
C       MX4 - MAXIMUM OF INDEPENDENT VARIABLE 4 VALUES IN TABLE
C
C
C     OUTPUTS:
C
C       Y - DEPENDENT VALUE FROM TABLE LOOK UP
C
C#######################################################################
C
      DIMENSION XT1(MX1),XT2(MX2),XT3(MX3),XT4(MX4),YT(MX1,MX2,MX3,MX4)
      LOGICAL ASCND
C
C--FIND POINTER FOR INDEPENDENT VARIABLE 4
C
      ASCND = XT4(MX4).GE.XT4(1)
      IX4 = MAX0(1,MIN0(IX4,MX4))
      IF (X4.GT.XT4(IX4).EQV.ASCND) THEN
C
C--SEARCH UP FOR INDEPENDENT VARIABLE 4
C
        DO 10  I=IX4+1,MX4
          IF (XT4(I).GT.X4.EQV.ASCND) THEN
            IX4 = I-1
            GO TO 30
          END IF
   10   CONTINUE
        IX4 = MX4-1
C
C--SEARCH DOWN FOR INDEPENDENT VARIABLE 4
C
      ELSE
        DO 20  I=IX4-1,1,-1
          IF (XT4(I).LE.X4.EQV.ASCND) THEN
            IX4 = I
            GO TO 30
          END IF
   20   CONTINUE
        IX4 = 1
      END IF
C
C--NOW HAVE UPDATED IX1, IX2, AND IX3 -- DEFINE SOME VARIABLES FOR LATER
C--USE
C
   30 DTX4 = XT4(IX4+1)-XT4(IX4)
C99      IF (DTX4.EQ.0.0D0) THEN
C99        CALL ERROR(3012)
C99      ELSE
        DX4  = X4-XT4(IX4)
        CALL TBLP3(X1,X2,X3,IX1,IX2,IX3,XT1,XT2,XT3,MX1,MX2,MX3,
     1             YT(1,1,1,IX4),Y4,DY1A,DY2A,DY3A)
        CALL TBLP3(X1,X2,X3,IX1,IX2,IX3,XT1,XT2,XT3,MX1,MX2,MX3,
     1             YT(1,1,1,IX4+1),Y4N,DY1B,DY2B,DY3B)
        YD = (Y4N-Y4)/DTX4
        Y  = Y4+YD*DX4
C99      END IF
C
C--COMPUTE SLOPES
C
      PCT = DX4/DTX4
      SL1 = DY1A+PCT*(DY1B-DY1A)
      SL2 = DY2A+PCT*(DY2B-DY2A)
      SL3 = DY3A+PCT*(DY3B-DY3A)
      SL4 = YD
      RETURN
      END
C
C************************************************************************
C************* DOUBLE PRECISION MATRIX UTILITIES ************************
C************************************************************************
      SUBROUTINE DMATDBL (D,S,N1,N2)
C
C*** DOUBLE PRECISION D(N1*N2) IS OBTAINED FROM SINGLE PRECISION S(N1*N2)
C
      DOUBLE PRECISION D(*)
      REAL S(*)      
C     
      L=N1*N2
      DO  I=1,L
          D(I)=DBLE(S(I))
      ENDDO
      RETURN
      END
C
      SUBROUTINE DMATSGL (S,D,N1,N2)
C
C*** SINGLE PRECISION S(N1*N2) IS OBTAINED FROM DOUBLE PRECISION S(N1*N2)
C
      DOUBLE PRECISION D(*)
      REAL S(*)      
C     
      L=N1*N2
      DO  I=1,L
          S(I)=SNGL(D(I))
      ENDDO
      RETURN
      END
C
      SUBROUTINE DMATADD (C,A,B,N1,N2)
C
C*** DOUBLE PRECISION VERSION OF MATADD
C*** C(N1*N2) IS OBTAINED FROM ADDING A(N1*N2) TO B(N1*N2)
C
      DOUBLE PRECISION A(*),B(*),C(*)

      DO  J=1,N2
         DO  I=1,N1
            K=N2*(J-1)+I
            C(K)=A(K)+B(K)
         ENDDO
      ENDDO
      RETURN
      END
C
      SUBROUTINE DMATSUB (C,A,B,N1,N2)
C
C*** DOUBLE PRECISION VERSION OF MATSUB
C*** C(N1*N2) IS OBTAINED FROM SUBSTRACTING B(N1*N2) FROM A(N1*N2)
C
      DOUBLE PRECISION A(*),B(*),C(*)
C
      DO  J=1,N2
         DO  I=1,N1
            K=N2*(J-1)+I
            C(K)=A(K)-B(K)
         ENDDO
      ENDDO
      RETURN
      END
C
      SUBROUTINE DMATABS (D,V,N)
C
C*** DOUBLE PRECISION VERSION OF MATABS
C*** D IS THE ABSOLUTE VALUE OF VECTOR V(N*1)
C
      DOUBLE PRECISION D,DV,V(*)
C
      DV=0.
      DO 1 I=1,N
1     DV=DV+V(I)*V(I)
      D=SQRT(DV)
      RETURN
      END        
C
      SUBROUTINE DMATMUL (C,A,B,N1,N2,N3)
C
C*** DOUBLE PRECISION VERSION OF MATMUL
C*** C(N1*N2) IS OBTAINED FROM MULTIPLYING A(N1*N2) BY B(N2*N3)
C
      DOUBLE PRECISION A(*),B(*),C(*)
C
      DO I=1,N1
         DO K=1,N3
            N=N1*(K-1)+I
            C(N)=0.0
            DO J=1,N2
               L=N1*(J-1)+I
               M=N2*(K-1)+J
               C(N)=C(N)+A(L)*B(M)
            ENDDO
         ENDDO
      ENDDO
      RETURN
      END
C
      SUBROUTINE DMATTRA (C,A,N1,N2)
C
C*** DOUBLE PRECISION VERSION OF MATTRA
C*** C(N2*N1) IS THE TRANSPOSE OF A(N1*N2)
C
      DOUBLE PRECISION A(N1,N2),C(N2,N1)
C
      DO 1 J=1,N2
      DO 1 I=1,N1
1     C(J,I)=A(I,J)
      RETURN
      END
C
C          SUBROUTINE DMATINV
C
C          PURPOSE
C             COMPUTE THE INVERSE OF A NON-SINGLAR MATRIX
C             DOUBLE PRECISION VERSION OF MATINV
C          USAGE
C             CALL DMATINV(AI,A,D,N)
C
C          DESCRIPTION OF PARAMETERS
C             A - NAME OF INPUT MATRIX
C             AI- NAME OF OUTPUT MATRIX (MAY BE THE SAME AS A)
C             D - DETERMINANT OF A
C             N - NUMBER OF ROWS AND COLUMNS OF A
C
C          REMARKS
C             ALL MATRICES ARE STORED AS GENERAL MATRICES
C
C        SET UP TO CALL GAUSS-JORDAN INVERSION ROUTINE.
C
C     ..................................................................
      SUBROUTINE DMATINV(AI,A,D,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(*),AI(*)
C
C     SET UP TO CALL ROUTINE THAT ACTUALLY DOES THE CONVERSION
C
      MX = N
      M = N
      EPSIL = 1.D-10
C
C         COPY A MATRIX INTO AI MATRIX
C
      N2=N*N
      DO 1 I = 1, N2
1     AI(I)=A(I)
C
      CALL DMATFS(EPSIL,MX,M,AI,IER,DET)
      D=DET
      IF(IER.EQ.0) RETURN
      D=0.D0
      PRINT *,' MATRIX IS SINGULAR '
      X=-2
      Y=SQRT(X)**2
      RETURN
      END
C
      SUBROUTINE DMATFS(EPSIL,MX,M,A,IER,DET)
C
C      DMATFS IS A DOUBLE-PRECISION MATRIX INVERSION SUBROUTINE
C     WRITTEN IN FORTRAN IV  - VARIABLE DIMENSIONING IS USED
C     OF MAX DIMENSIONS OF MATRIX IN CSLLING PROGRAM MUST BE SAME
C     VALUE AS PASSED ON BY CALLING SEQUENCE AS MX.
C     MATRIX CAN BE OF GENERAL FORM. METHOD IS GAUSS-JORDAN.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER  I,J,K,KP,JF,IBETA,L,IG,IPHI,M,IFKP,LDA,IGKP
C
      DIMENSION JF(30),IG(30),L(30),IBETA(30),TEMP(30),A(MX,MX)

C     INITIALIZE AND LOGICAL OPERATIONS ON COLUMN K, PIVOT COL.
      DET=1.0D0
      IER =0
      DO 5 KP =1,M
      JF(KP) = 0
      IBETA(KP) = 0
      L(KP)=KP
      IG(KP)=KP
    5 CONTINUE
C     TRANSFORM BY COLUMNS, K
      DO 32 K = 1, M
C     FIND LARGEST ELEMENT IN COL K WITH IBETA STILL ZERO
      RMU=0.0D0
      DO 14 I = 1,M
      IF (IBETA(I)) 1000,10,14
   10 IF ( ABS(A(I,K)) -  ABS(RMU))  14,14,12
C     LABEL ROW WITH LARGEST ELEMENT (RMU) AS IPHI
   12 RMU = A(I,K)
      IPHI = I
   14 CONTINUE
C     IF ELEMENT RMU IS TOO SMALL,MATRIX IS SINGULAR-CALL EXIT
   15 IF ( ABS(RMU) - EPSIL) 16,16,18
 16   IER =1
      RETURN
   18 DET = RMU*DET
C     SETF(K) WITH INDEX OF ROW, IBETA OF ROW NOW 1
      JF(K) = IPHI
      IBETA(IPHI) = 1
C     ARITHMETIC OPERATIONS ON PIVOT COLUMN,K
C     FIRST SAVE ENTIRE COLUMN IN PRESENT FORM
   20 DO 21 I=1,M
      TEMP(I) = A(I,K)
   21 CONTINUE
C     TRANSFORM COLUMN
      A(IPHI,K) = 1.0D0/RMU
      DO 23 I=1,M
      IF (I - IPHI)  22,23,22
   22 A(I,K) = -A(I,K)/RMU
   23 CONTINUE
C     LOGICAL OPERATIONS ON OTHER COLUMNS    PANEL IV
C     NOTE WE USE ORIG SAVED VALUES FROM PIVOT COLUMN,K
      DO 31 J =1,M
C     TEST TO SKIP COL K, ALREADY TRANSFORMED
      IF (J-K)  25,31,25
C     NOT K   IF VALUE IN ROW IPHI IS 0 SKIP COLUMN
   25 IF (A(IPHI,J)) 26,31,26
C     NOT ZERO
   26 ALPHA = A(IPHI,J)
      A(IPHI,J) = ALPHA/RMU
C     TRANSFORM IN ONE COL,J, BY ROW INDICES
      DO 30 I =1,M
C     TEST IF ELEMENT IS IN ROW WITH PIVOT ELEMENT
      IF (I-IPHI)  28,30,28
C     NO
   28 A(I,J) = A(I,J) - ALPHA*TEMP(I) /RMU
C     YES
   30 CONTINUE
   31 CONTINUE
   32 CONTINUE
C     PERMUTATIONS OF ROWS AND COLUMNS
      MM=M-1
      DO 55 KP=1,MM
C     PERMUTATION OF ROWS
      IF(IG(KP)-JF(KP)) 50,55,50
   50 IFKP = JF(KP)
      LDA = L(IFKP)
      DO 51 J =1,M
      TEMP(J) = A(KP,J)
      A(KP,J) = A(LDA,J)
      A(LDA,J) = TEMP(J)
      DET = -DET
   51 CONTINUE
C     PERMUTATION OF COLUMNS
      IGKP = IG(KP)
      DO 53 I = 1,M
      TEMP(I) = A(I,IFKP)
      A(I,IFKP) = A(I,IGKP)
      A(I,IGKP) = TEMP(I)
C     RESET INDICES
      L(IFKP) = K
       L(IGKP) = LDA
      IG(LDA) = IGKP
      IG(KP) = IFKP
   53 CONTINUE
   55 CONTINUE
      RETURN
 1000  WRITE (6,1001)
      STOP
 1001 FORMAT (22H ERROR, IBETA NEGATIVE )
C     INVERTED MATRIX IS IN CELLS ORIGINALLY OCCUPIED BY MATRIX
C     DETERMINANT IS IN CELL DET (NOT RETURNED)
   60 RETURN
      END
C**********************************************************************
      SUBROUTINE US76(RHO,PRESS,TEMPK,BALT)
C**********************************************************************
C*** *
C*** * US Standard Atmosphere 1976
C*** * 
C*** * This module performs the following functions:
C*** *  Calculates the atmospheric properties
C*** *
C*** * Argument Output:
C*** *					RHO=Air density - kg/m^3
C*** *					PRESS= Air static pressure - Pa
C*** *					TEMPK= Air temperature - degKelvin
C*** * Argument Input:
C*** *					BALT= Geometrical altitude above S.L. - m
C*** *
C*** * MODIFICATION HISTORY
C*** * 000204 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
	PARAMETER(REARTH = 6369.0)    ! Radius of the Earth (km)
	PARAMETER(GMR = 34.163195)    ! Gas constant
	PARAMETER(RHOSL=1.22500)	  ! Sea Level density - kg/m^3
	PARAMETER(PRESSL=101325.)	  ! Sea Level pressure - Pa
	PARAMETER(TEMPKSL=288.15)	  ! Sea Level temperature - dK
	PARAMETER(NTAB=8)             ! Number of entries in the defining tables
C
	DIMENSION htab(8),ttab(8),ptab(8),gtab(8)
C
	DATA htab/0.0, 11.0, 20.0, 32.0, 47.0, 51.0, 71.0, 84.852/                         
	DATA ttab
     & /288.15, 216.65, 216.65, 228.65, 270.65, 270.65, 214.65, 186.946/
	DATA ptab
     & /1.0, 2.233611E-1, 5.403295E-2, 8.5666784E-3, 1.0945601E-3,
     & 6.6063531E-4, 3.9046834E-5, 3.68501E-6/
	DATA gtab /-6.5, 0.0, 1.0, 2.8, 0.0, -2.8, -2.0, 0.0/
C
	alt=BALT/1000.
	h=alt*REARTH/(alt+REARTH)! convert geometric to geopotential altitude
C
	i=1
	j=NTAB                   ! setting up for binary search
	DO
	  k=(i+j)/2              ! integer division
	  IF (h.lt.htab(k)) THEN
	    j=k
	  ELSE
	    i=k
	  END IF
	  IF (j.le.(i+1)) EXIT
	END DO
C
	tgrad=gtab(i)            ! i will be in 1...NTAB-1
	tbase=ttab(i)
	deltah=h-htab(i)
	tlocal=tbase+tgrad*deltah
	theta=tlocal/ttab(1)     ! temperature ratio
C
	IF (tgrad.eq.0.0) THEN   ! pressure ratio
	  delta=ptab(i)*EXP(-GMR*deltah/tbase)
	ELSE
	  delta=ptab(i)*(tbase/tlocal)**(GMR/tgrad)
	END IF
C
	sigma=delta/theta        ! density ratio
C
	RHO=RHOSL*sigma
	PRESS=PRESSL*delta
	TEMPK=TEMPKSL*theta
	RETURN
	END
C
C***********************************************************************
C************************ CAD UTILITIES ********************************
C***********************************************************************
      SUBROUTINE CADELP(AMA,AMI,PHI,AA)
C
C*** MAJOR, MINOR AXES OF ELLIPSE AND ROTATION ANGLE 
C    FROM THE SYMMETRICAL POS SEMI-DEFINITE AA(2X2) MATRIX
C
C      Coordinate axes orientation:
C     
C          ^ 1-axis
C          |
C          |---> 2-axis
C
C  ARGUMENT OUTPUT:
C
C          AMA   =Major semi-axis
C          AMI   =Minor semi=axis
C          PHIEV =Angle of major axis wrt first coord axis
C
C  ARGUMENT INPUT:
C
C          AA(2x2) =Real symmetrical pos semi-definite matrix
C
      DIMENSION AA(2,2),X1V(2),X2V(2)
C
C*** MAJOR AND MINOR SEMI-AXES (EIGENVALUES OF SUBMATRIX)
C
      A11=AA(1,1)
      A22=AA(2,2)
      A12=AA(1,2)
      A1133=A11+A22
      AQ1122=A1133**2
      DUM1=AQ1122-4.*(A11*A22-A12**2)
      IF(DUM1.GE.0.) DUM2=SQRT(DUM1)
      AMA=(A1133+DUM2)/2.
      AMI=(A1133-DUM2)/2.
      IF(AMA.EQ.AMI) RETURN
C
C*** PRINCIPAL AXES IN ORIGINAL(V)-COORDINATES (EIGENVECTORS)
C
      IF(A11-AMA.NE.0.) THEN
         DUM1=-A12/(A11-AMA)
         AK1=SQRT(1./(1.+DUM1**2))
         X1V(1)=DUM1*AK1
         X1V(2)=AK1
         DUM=DUM1*AK1
         IF(ABS(DUM).GT.1.) DUM=SIGN(1.,DUM)
         PHI=ACOS(DUM)
      ELSE
         DUM1=-A12/(A22-AMA)
         AK1=SQRT(1./(1.+DUM1**2))
         X1V(1)=AK1
         X1V(2)=DUM1*AK1
         IF(ABS(AK1).GT.1.) AK1=SIGN(1.,AK1)
         PHI=ACOS(AK1)    
      ENDIF
      IF(A11-AMI.NE.0.) THEN
         DUM2=-A12/(A11-AMI)
         AK2=SQRT(1./(1.+DUM2**2))
         X2V(1)=DUM2*AK2
         X2V(2)=AK2
      ELSE
         DUM2=-A12/(A22-AMI)
         AK2=SQRT(1./(1.+DUM2**2))
         X2V(1)=AK2
         X2V(2)=DUM2*AK2
      ENDIF
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE CADORB(E,DE,PERG,AINC,ANTII,ALNGN,OMEG,ANU,ALTII,
     +STII,VTII)
C**********************************************************************        
C*** *
C*** * Orbital elements from inerftial position and velocity
C*** * 
C*** * Argument Output:
C*** *          E(3) =Orbit eccentricity vector - ND
C*** *          DE =Orbit eccentricity - ND
C*** *          PERG =Radius of perigee - m
C*** *          AINC =Inclination of orbit - rad
C*** *          ANTII(3) =Node vector - m^2/s
C*** *          ALNGN =Longitude of the ascending node - rad
C*** *          OMEG =Argument of periapsis - rad
C*** *          ANU =True anomaly - rad
C*** *          ALTII(3) =Angular momentum vector of orbit - m^2/s
C*** * Argument Input:
C*** *          STII(3) =Position of T wrt I in inertial coord - m
C*** *          VTII(3) =Inertial Velocity of T in inertial coor - m/s
C*** *
C*** * MODIFICATION HISTORY
C*** * 840331 Created by Jim Williford
C*** *
C**********************************************************************
C
      DOUBLE PRECISION DANTID,DED,DUM2D,ARG2D,DOMEGD
      COMMON C(3510)
C
      DIMENSION STII(3),VTII(3),SSTII(3,3),ALTII(3),ANTII(3),
     +DUMV1(3),DUMV2(3),DUMV3(3),E(3)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0057),AMU)
C
C*** TARGET ANGULAR MOMENTUM [ALTII] AND NODE [ANTII] VECTORS
C
      CALL MATSKS(SSTII,STII)
      CALL MATMUL(ALTII,SSTII,VTII,3,3,1)
      CALL MATABS(DALTI,ALTII,3)
C
      ANTII(1)=-ALTII(2)
      ANTII(2)=ALTII(1)
      ANTII(3)=0.
      CALL MATABS(DANTI,ANTII,3)
C
C*** ORBIT ECCENTRICITY
C
      CALL MATABS(DTI,STII,3)
      CALL MATSCA(DUM1,STII,VTII,3)
      CALL MATCON(DUMV1,DUM1,VTII,3,1)
      CALL MATABS(DVTI,VTII,3)
      DUM2=DVTI**2.-AMU/DTI
      CALL MATCON(DUMV2,DUM2,STII,3,1)
      CALL MATSUB(DUMV3,DUMV2,DUMV1,3,1)
      DUM2=1./AMU
      CALL MATCON(E,DUM2,DUMV3,3,1)  !!
      CALL MATABS(DE,E,3)
C
C*** TARGET PERIGEE (ABOVE GROUND), ORBIT INCLINATION, AND LONGITUDE 
C    OF ASCENDING NODE
C
      ALATRC=(DALTI**2.)/AMU
      PERI=ALATRC/(1+DE)
      PERG=PERI-REARTH
C
      AINC=ACOS(ALTII(3)/DALTI)
C
      DALNGN=ACOS(ANTII(1)/(SQRT(ANTII(1)**2.+ANTII(2)**2.)))
C
C*** ARGUMENT OF THE PERIGEE AND TRUE ANOMALY
C
      CALL MATSCA(DUM2,ANTII,E,3)
C*** CONVERSION TO DOUBLE PRECISION
	DANTID=DBLE(DANTI)
      DED=DBLE(DE)
      DUM2D=DBLE(DUM2)
C
      ARG2D=DUM2D/(DANTID*DED)
      IF(ARG2D.GT.1.) ARG2D=1.
      IF(ARG2D.LT.-1.) ARG2D=-1.
      DOMEGD=DACOS(ARG2D)
C*** CONVERSION TO SINGLE PRECISION
      DOMEG=SNGL(DOMEGD)
C
      CALL MATSCA(DUM3,E,STII,3)
      ARG=DUM3/(DE*DTI)
      IF(ARG.GT.1.) ARG=1.
      IF(ARG.LT.-1.) ARG=-1.
      DANU=ACOS(ARG)
C
C
C*** QUADRANT ACCOUNTING
C
      TWOPI=360./CRAD
      CALL MATSCA(DUM4,STII,VTII,3)
C
      IF(DUM4.GE.0.) THEN
	ANU=DANU
      ELSE
	ANU=TWOPI-DANU
      END IF
C
      IF(E(3).GE.0.) THEN
	OMEG=DOMEG
      ELSE
	OMEG=TWOPI-DOMEG
      END IF
C
      IF(ANTII(2).GT.0.) THEN
	ALNGN=DALNGN
      ELSE
	ALNGN=TWOPI-DALNGN
      END IF
C
      RETURN
      END
C******************************************************************
      SUBROUTINE CADTGI84(TGI,BLON,BLAT,BALT,ALON0)
C******************************************************************
C*** * Calculates geographic (geocentric) wrt inertial T.M.
C*** *  using the WGS 84 reference ellipsoid
C*** * Reference: Britting,K.R."Inertial Navigation Systems Analysis",
C*** * Wiley, 1971
C*** *
C*** * Argument ouput: TGI(3x3) =Geographic wrt inertial T.M.- ND
C*** * Argument input: BLON =Geodetic longitude - rad
C*** *                 BLAT =Geodetic latitude - rad
c*** *                 BALT =Height above ellipse - m
C*** *                 ALON0=Greenwich celestial longitude at t=0 - rad
C*** *
C*** * MODIFICATION HISTORY
C*** * 990413 Created by Peter Zipfel
C*** * 990625 Use of deflection DD instead of D0, PZi
C*** *
C*** **************************************************************
      COMMON C(3510)
C
      DIMENSION TGI(3,3),TGD(3,3),TDI(3,3)
C
C*** INPUT FROM EXEC
C
      EQUIVALENCE (C(0058),WEII3)
      EQUIVALENCE (C(2000),T)
C
C WEII3 =Earth's mean angular velocity (WGS84) - rad/s
C
C*** WGS 84 Parameters
C
	PARAMETER(EE = 0.003352811)
	PARAMETER(AEL = 6378137.)
C
C EE =flattening of earth's ellipsoid - ND
C AEL =Major axis of earth's ellipsoid - m
C
C*** Celestial longitude of vehicle at time T
C
	BLONC=ALON0+WEII3*T+BLON
C
C*** T.M. of geodetic coord wrt inertial coord., TDI(3x3)
C
      TDI(1,3)=COS(BLAT)
      TDI(3,3)=-SIN(BLAT)
      TDI(2,2)=COS(BLONC)
      TDI(2,1)=-SIN(BLONC)
      TDI(1,1)=TDI(3,3)*TDI(2,2)
      TDI(1,2)=-TDI(3,3)*TDI(2,1)
      TDI(3,1)=-TDI(1,3)*TDI(2,2)
      TDI(3,2)=TDI(1,3)*TDI(2,1)
      TDI(2,3)=0.
C
C*** T.M. of geographic (geocentric) wrt geodetic coord.,	TGD(3x3)
C
	CALL MATUNI(TGD,3)
      R0=AEL*(1.-EE*(1.-COS(2.*BLAT))/2.+5.*EE**2*(1.-COS(4.*BLAT))/16.) !Eq 4-21
	DD=EE*SIN(2.*BLAT)*(1.-EE/2.-BALT/R0) !Eq 4-15
	TGD(1,1)=COS(DD)
	TGD(3,3)=TGD(1,1)
	TGD(3,1)=SIN(DD)
	TGD(1,3)=-TGD(3,1)
C
C*** T.M. of geographic (geocentric) wrt inertial coord.,	TGI(3x3)
C
	CALL MATMUL(TGI,TGD,TDI,3,3,3)
C
      RETURN
      END
C******************************************************************
      SUBROUTINE CADGEO84(BLON,BLAT,BALT,SBII,ALON0)
C******************************************************************
C*** * Calculates geodetic longitude, latitude, and altitude
C*** *  using the WGS 84 reference ellipsoid
C*** * Reference: Britting,K.R."Inertial Navigation Systems Analysis",
C*** * Wiley, 1971
C*** *
C*** * Argument output: BLON =Geodetic longitude - rad
C*** *                  BLAT =Geodetic latitude - rad
C*** *				  BALT =Altitude above ellipsoid - m
C*** * Argument input:  SBII(3x1) =Inertial vehicle position - m
C*** *                  ALON0=Greenwich celestial longitude at t=0 - rad
C*** *
C*** *
C*** * MODIFICATION HISTORY
C*** * 990414 Created by Peter Zipfel
C*** * 990625 Use of deflection DD instead of D0, PZi
C*** **************************************************************
C
      COMMON C(3510)
C
	DIMENSION SBII(3)
C
C*** INPUT FROM EXEC
C
      EQUIVALENCE (C(0058),WEII3)
      EQUIVALENCE (C(2000),T)
C
C WEII3 =Earth's mean angular velocity (WGS84) - rad/s
C
C*** WGS 84 Parameters
C
	PARAMETER(EE = 0.003352811)
	PARAMETER(AEL = 6378137.)
C
C EE =flattening of earth's ellipsoid - ND
C AEL =Major axis of earth's ellipsoid - m
C
C*** Initialize geodetic latitude using geocentric latitude
C
	CALL MATABS(DBI,SBII,3)
	BLATG=ASIN(SBII(3)/DBI)
	BLAT=BLATG
	ICOUNT=0
C
C*** Iterative loop to caculate geodetic latitude and altitude
C
 10	CONTINUE
C
	BLM=BLAT
      R0=AEL*(1.-EE*(1.-COS(2.*BLM))/2.+5.*EE**2*(1.-COS(4.*BLM))/16.) !Eq 4-21
      BALT=DBI-R0
	DD=EE*SIN(2.*BLM)*(1.-EE/2.-BALT/R0) !Eq 4-15
	BLAT=BLATG+DD
	ICOUNT=ICOUNT+1
	IF(ICOUNT.GT.500) THEN
		TYPE*,' *** STOP: 500 iterations in sbr CADGEO84 ***'
		STOP
	ENDIF
C
	IF(ABS(BLAT-BLM).GT.1E-7) GO TO 10
C
C*** Geodetic (identical to geographic(geocentric)) longitude 
C
	BLON=ASIN(SBII(2)/SQRT(SBII(1)**2+SBII(2)**2))-WEII3*T-ALON0 
C
	RETURN
	END


C******************************************************************
      SUBROUTINE CADINE84(SBII,BLON,BLAT,BALT,ALON0)
C******************************************************************
C*** * Calculates inertial position vector from geodetic longitude,
C*** *  latitude, and altitude using the WGS 84 reference ellipsoid
C*** * Reference: Britting,K.R."Inertial Navigation Systems Analysis"
C*** * pp.45-49, Wiley, 1971
C*** *
C*** * Argument ouput: SBII(3x1) =Inertial vehicle position - m
C*** * Argument input: BLON =Geodetic longitude - rad
C*** *                 BLAT =Geodetic latitude - rad
C*** *                 BALT =Altitude above ellipsoid - m
C*** *                 ALON0=Greenwich celestial longitude at t=0 - rad
C*** *
C*** * MODIFICATION HISTORY
C*** * 990413 Created by Peter Zipfel
C*** * 990625 Use of deflection DD instead of D0, PZi
C*** *
C*** **************************************************************
C
      COMMON C(3510)
C
      DIMENSION SBII(3),SBID(3)
C
C*** INPUT FROM EXEC
C
      EQUIVALENCE (C(0058),WEII3)
      EQUIVALENCE (C(2000),T)
C
C WEII3 =Earth's mean angular velocity (WGS84) - rad/s
C
C*** WGS 84 Parameters
C
	PARAMETER(EE = 0.003352811)
	PARAMETER(AEL = 6378137.)
C
C EE =flattening of earth's ellipsoid - ND
C AEL =Major axis of earth's ellipsoid - m
C
C*** Deflection of the normal, DD, and length of earth's radius to ellipse, R0
C
      R0=AEL*(1.-EE*(1.-COS(2.*BLAT))/2.+5.*EE**2*(1.-COS(4.*BLAT))/16.) !Eq 4-21
	DD=EE*SIN(2.*BLAT)*(1.-EE/2.-BALT/R0) !Eq 4-15
C
C*** Vehicles's displacement vector from earth's center SBID(3x1) in geodetic coord.
C
	DBI=R0+BALT
	SBID(1)=-DBI*SIN(DD)
	SBID(2)=0.
	SBID(3)=-DBI*COS(DD)
C
C*** Celestial longitude of vehicle at time T
C
      BLONC=ALON0+WEII3*T+BLON
C
C*** Vehicle's displacement vector in inertial coord. SBII(3x1)=TID(3x3)xSBID(3x3)
C
      SLAT=SIN(BLAT)
	CLAT=COS(BLAT)
	SLON=SIN(BLONC)
	CLON=COS(BLONC)
      SBII(1)=-SLAT*CLON*SBID(1)-CLAT*CLON*SBID(3)
      SBII(2)=-SLAT*SLON*SBID(1)-CLAT*SLON*SBID(3)
      SBII(3)=CLAT*SBID(1)-SLAT*SBID(3)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE CADSPH(BLON,BLAT,BALT,SBII,ALON0)
C
C*** CALCULATES LON,LAT,ALT FROM INERTIAL POSITION VECTOR
C
      COMMON  C(3510)
C
      DIMENSION SBII(3)
C
C*** INPUT FROM EXEC
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0058),WEII3)
      EQUIVALENCE (C(2000),T)
C
C*** LATITUDE
C
      DBII=SQRT(SBII(1)**2+SBII(2)**2+SBII(3)**2)
      DUM1=SBII(3)/DBII
      BLAT=ASIN(DUM1)
      BALT=DBII-REARTH
C
C*** LONGITUDE
C
      DUM3=SBII(2)/SQRT(SBII(1)**2+SBII(2)**2)
      DUM4=ASIN(DUM3)
      IF(SBII(1).GE.0..AND.SBII(2).GE.0.) ALAMDA=DUM4
      IF(SBII(1).LT.0..AND.SBII(2).GE.0.) ALAMDA=180./CRAD-DUM4
      IF(SBII(1).LT.0..AND.SBII(2).LT.0.) ALAMDA=180./CRAD-DUM4
      IF(SBII(1).GT.0..AND.SBII(2).LT.0.) ALAMDA=360./CRAD+DUM4
      BLON=ALON0+ALAMDA-WEII3*T
      IF(BLON.GT.(180./CRAD)) BLON=-((360./CRAD)-BLON)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE CADIP(TIP,AINC,OMEGA,ALONG)
C
C*** THIS SUBROUTINE COMPUTES THE TRANSFORMATION OF INERTIAL RELATIVE TO
C*** PERIFOCAL COORDINATE SYSTEMS
C
      COMMON C(3510)
C
      DIMENSION TIP(3,3)
C
      CALONG=COS(ALONG)
      COMEGA=COS(OMEGA)
      CAINC=COS(AINC)
      SALONG=SIN(ALONG)
      SOMEGA=SIN(OMEGA)
      SAINC=SIN(AINC)
C
      TIP(1,1)=CALONG*COMEGA-SALONG*SOMEGA*CAINC
      TIP(1,2)=-CALONG*SOMEGA-SALONG*COMEGA*CAINC
      TIP(1,3)=SALONG*SAINC
C
      TIP(2,1)=SALONG*COMEGA+CALONG*SOMEGA*CAINC
      TIP(2,2)=-SALONG*SOMEGA+CALONG*COMEGA*CAINC
      TIP(2,3)=-CALONG*SAINC
C
      TIP(3,1)=SOMEGA*SAINC
      TIP(3,2)=COMEGA*SAINC
      TIP(3,3)=CAINC
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE CADPT(TPT,ANU,DECC)
C
C*** THIS SUBROUTINE COMPUTES THE TRANSFORMATION OF PERIFOCAL RELATIVE
C*** TO TARGET CENTERED COORDINATE SYSTEM
C
      DIMENSION TPT(3,3)
C
      DUM4=SIN(ANU)
      DUM5=COS(ANU)+DECC
      PHIO=ATAN2(DUM4,DUM5)
C
      TPT(1,1)=-SIN(PHIO)
      TPT(1,2)=0.
      TPT(1,3)=-COS(PHIO)
C
      TPT(2,1)=-TPT(1,3)
      TPT(2,2)=0.
      TPT(2,3)=TPT(1,1)
C
      TPT(3,1)=0.
      TPT(3,2)=-1.
      TPT(3,3)=0.
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE CADLI(TLI,ST1II,VT1II)
C
C*** TRANSFORMATION MATRIX OF LOCAL VERTICAL WRT. INERTIAL COOR.SYS.
C
C    LOCAL VERTICAL COOR.SYS.: X-> DIRECTION OF RADIUS VECTOR
C			       Z-> DIRECTION OF ANGULAR MOM.VECTOR
C			       Y-> COMPLETES RIGHT HANDED COOR.SYS.
      COMMON C(3510)
C
      DIMENSION TLI(3,3),ST1II(3),VT1II(3)
C
      CALL MATABS(DT1II,ST1II,3)
      CALL MATABS(DVT1II,VT1II,3)
      AL1I1=ST1II(1)/DT1II
      AL1I2=ST1II(2)/DT1II
      AL1I3=ST1II(3)/DT1II
      DUM=DT1II*DVT1II
      AL3I1=(ST1II(2)*VT1II(3)-ST1II(3)*VT1II(2))/DUM
      AL3I2=(ST1II(3)*VT1II(1)-ST1II(1)*VT1II(3))/DUM
      AL3I3=(ST1II(1)*VT1II(2)-ST1II(2)*VT1II(1))/DUM
      AL2I1=(AL3I2*AL1I3-AL3I3*AL1I2)
      AL2I2=(AL3I3*AL1I1-AL3I1*AL1I3)
      AL2I3=(AL3I1*AL1I2-AL3I2*AL1I1)
C
      TLI(1,1)=AL1I1
      TLI(1,2)=AL1I2
      TLI(1,3)=AL1I3
      TLI(2,1)=AL2I1
      TLI(2,2)=AL2I2
      TLI(2,3)=AL2I3
      TLI(3,1)=AL3I1
      TLI(3,2)=AL3I2
      TLI(3,3)=AL3I3
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE CADDIS(DISBWX,BLON,BLAT,WLON,WLAT)
C
C*** GREAT CIRCLE GROUND DISTANCE BETWEEN B AND W IN KM: DISBWX,
C    GIVEN THE LONGITUDES AND LATITUDES OF B AND W
C
      COMMON C(3510)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0051),REARTH)
C
      DUM=SIN(WLAT)*SIN(BLAT)+COS(WLAT)*COS(BLAT)*COS(WLON-BLON)
      IF(ABS(DUM).GT.1.)DUM=SIGN(1.,DUM)
      DISBWX=REARTH*ACOS(DUM)*1.E-3
C
      RETURN
      END
C*******************************************************************
      SUBROUTINE CADTGE(TGE,BLON,BLAT)
C***  **************************************************************
C***  *
C***  * Calculates transformation matrix TGE from longitude and latitude
C***  *
C***  * Argument Output:
C***  *          TGE(3,3) =Transf. of geographic wrt earth coor
C***  * Argument Input:
C***  *          BLON =Missile longitude - rad
C***  *          BLAT =Missile latitude - rad
C***  *
C***  * MODIFICATION HISTORY
c***  * 960628 Created by Peter Zipfel
C***  *
C***  **************************************************************
      COMMON  C(3510)
C
      DIMENSION TGE(3,3)
C
      SLON=SIN(BLON)
      CLON=COS(BLON)
      SLAT=SIN(BLAT)
      CLAT=COS(BLAT)
      TGE(1,1)=-SLAT*CLON
      TGE(1,2)=-SLAT*SLON
      TGE(1,3)=CLAT
      TGE(2,1)=-SLON
      TGE(2,2)=CLON
      TGE(2,3)=0.
      TGE(3,1)=-CLAT*CLON
      TGE(3,2)=-CLAT*SLON
      TGE(3,3)=-SLAT
C 
      RETURN
      END      
C**********************************************************************
      SUBROUTINE CADINE(SBII,BLON,BLAT,BALT,ALON0)
C
C*** CALCULATES INERTIAL POSITION VECTOR FROM LON,LAT,ALT
C
      COMMON C(3510)
C
      DIMENSION SBII(3)
C
C*** INPUT FROM EXEC
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(0058),WEII3)
      EQUIVALENCE (C(2000),T)
C
C*** CELESTIAL LONGITUDE
C
      BLAM=BLON-ALON0+WEII3*T
C
C*** POSITION VECTOR
C
      BRAD=REARTH+BALT
      CBLAT=COS(BLAT)
      SBII(1)=BRAD*CBLAT*COS(BLAM)
      SBII(2)=BRAD*CBLAT*SIN(BLAM)
      SBII(3)=BRAD*SIN(BLAT)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE CADTGI(TGI,ALON,ALAT,ALON0)
C
C*** GEOGRAPHIC TO INERTIAL COOR. TRANSFORMATION MATRIX FROM LAT AND LON
C
      COMMON C(3510)
C
      DIMENSION TGI(3,3)
C
C*** INPUT FROM EXEC
C
      EQUIVALENCE (C(2000),T)
      EQUIVALENCE (C(0058),WEII3)
C
      ALAMB=ALON-ALON0+WEII3*T
C
      TGI(1,3)=COS(ALAT)
      TGI(3,3)=-SIN(ALAT)
      TGI(2,2)=COS(ALAMB)
      TGI(2,1)=-SIN(ALAMB)
      TGI(1,1)=TGI(3,3)*TGI(2,2)
      TGI(1,2)=-TGI(3,3)*TGI(2,1)
      TGI(3,1)=-TGI(1,3)*TGI(2,2)
      TGI(3,2)=TGI(1,3)*TGI(2,1)
      TGI(2,3)=0.
C
      RETURN
      END
