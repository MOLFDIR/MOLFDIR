C
C     =================================================
      SUBROUTINE MFDTRCE(NBASL,NBASS,AR,AI,BR,BI,CR,CI,
     +                    IFI, SUMS ,DR,DI,ER,EI,FR,FI)
C     =================================================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     CALCULATE THE TRACE OF A PRODUCTMATRIX
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
C
      DIMENSION AR(NLLH),AI(NLLH),BR(NS,NL),BI(NS,NL),CR(NSSH),CI(NSSH)
      DIMENSION DR(NLLH),DI(NLLH),ER(NS,NL),EI(NS,NL),FR(NSSH),FI(NSSH)
      DIMENSION SUMS(3)
#if defined (LINUX)
      REAL*8  SUM1, SUM2
#else
      REAL*16 SUM1, SUM2
#endif
C
#if defined (BIT64)
      IF (AND(IFI,1).EQ.0) GOTO 10
#else
      IF (IAND(IFI,1).EQ.0) GOTO 10
#endif
      L=0
      SUM1=0.0D0
      SUM2=0.0D0
      DO 4 I=1,NBASL
      DO 3 J=1,I-1
      L=L+1
      SUM1=SUM1+DR(L)*AR(L)+DI(L)*AI(L)
    3 CONTINUE
      L=L+1
      SUM2=SUM2+DR(L)*AR(L)
    4 CONTINUE
      SUMS(1)=SUM1+SUM1+SUM2
 10   CONTINUE
#if defined (BIT64)
      IF (AND(IFI,2).EQ.0) GOTO 20
#else
      IF (IAND(IFI,2).EQ.0) GOTO 20
#endif
      SUM1=0.0D0
      DO 6 J=1,NBASL
      DO 5 I=1,NBASS
      SUM1=SUM1+ER(I,J)*BR(I,J)+EI(I,J)*BI(I,J)
    5 CONTINUE
    6 CONTINUE
      SUMS(2)=SUM1+SUM1
 20   CONTINUE
#if defined (BIT64)
      IF (AND(IFI,4).EQ.0) RETURN
#else
      IF (IAND(IFI,4).EQ.0) RETURN
#endif
      SUM1=0.0D0
      SUM2=0.0D0
      L=0
      DO 8 I=1,NBASS
      DO 7 J=1,I-1
      L=L+1
      SUM1=SUM1+FR(L)*CR(L)+FI(L)*CI(L)
    7 CONTINUE
      L=L+1
      SUM2=SUM2+FR(L)*CR(L)
    8 CONTINUE
      SUMS(3)=SUM1+SUM1+SUM2
      RETURN
      END
