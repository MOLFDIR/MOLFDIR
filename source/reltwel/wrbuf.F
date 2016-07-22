C
C     ========================================
      SUBROUTINE WRBUF (UNITI,UNITL,ITT,NTYPE)
C     ========================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE 'paracom.inc'
C
      COMMON/BUFR/GINTEG(N2,14)
#if defined (BIT64) 
      COMMON/BUFI/LABEL(N2,14)
#else
      COMMON/BUFI/LABEL(N2,14),LABEL1(N2,14)
#endif
C
      DIMENSION NTYPE(14),FILEND (14)
      INTEGER UNITI,UNITL
      LOGICAL FILEND
      DO 10 ITX = 1, 14
      FILEND (ITX) = .FALSE.
   10 CONTINUE
      IF (ITT.LT.0.OR.ITT.GT.15) THEN
         CALL Q2JOB (6,'WRONG VALUE OF ITT IN WRBUF',' ',0)
      ELSEIF (ITT.EQ.0) THEN
         ITS = 1
         ITF = 14
      ELSEIF (ITT.EQ.15) THEN
         ITS = 1
         ITF = 14
         FILEND (14) = .TRUE.
      ELSE
         ITS = ITT
         ITF = ITT
      ENDIF
      DO 20 ITX = ITS, ITF
      NTT = NTYPE (ITX)
      IF ((NTT.EQ.0).AND.(ITX.NE.14))  GOTO 20
      WRITE(UNITI) NTT,ITX,FILEND(ITX),
     &            (GINTEG(IX,ITX),IX=1,NTT)
#if defined (BIT64) 
      WRITE(UNITL) NTT,ITX,FILEND(ITX),
     &            (LABEL(IX,ITX),IX=1,NTT)
#else
      WRITE(UNITL) NTT,ITX,FILEND(ITX),
     &            (LABEL(IX,ITX),IX=1,NTT),(LABEL1(IX,ITX),IX=1,NTT)
#endif
      NTYPE (ITX) = 0
   20 CONTINUE
      RETURN
      END
