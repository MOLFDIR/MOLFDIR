C
      SUBROUTINE DETIND (ND,IDIND,JDET,JELEC,JSYM,IVWU)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     =========================================
C     CALCULATE ADRESSES OF THE STRINGS IN JDET
C     =========================================
C
      INCLUDE 'param.inc'
      INCLUDE 'files.inc'
      INCLUDE 'symm.inc'
C
      DIMENSION IDIND(*),JDET(*),JELEC(*),JSYM(*)
      DIMENSION IVWU(16,0:NORB,0:NELEC)
C
C
#if defined (CRAY)
      IAND (I, J) = AND (I, J)
      IOR (I, J) = OR (I, J)
      NOT(I) = COMPL (I)
      ISHFT (I, J) = SHIFT (I, J)
#endif
C
      DO 10 ID = 1, ND
      JELEC(ID) = NORBR(1)
      IDIND(ID) = 1
      JSYM(ID) = IRA1
   10 CONTINUE
C
      DO 30 I = 1, NORBR(2)
      MASK = ISHFT(1,I-1)
      II = NORBR(1) + I
      DO 20 ID = 1, ND
      IF (IAND(JDET(ID),MASK).NE.0) THEN
         JELEC(ID) = JELEC(ID) + 1
         JSYM(ID) = MULTB(IRPAMO(II),JSYM(ID))
         IDIND(ID) = IDIND(ID) + IVWU(JSYM(ID),II-1,JELEC(ID))
      ENDIF
   20 CONTINUE
   30 CONTINUE
C
      DO 40 ID = 1, ND
      IF (JSYM(ID).NE.IREP)
     & CALL Q2JOB (2,'TRIAL VECTORS HAVE WRONG SYMMETRY',' ',0)
   40 CONTINUE
C
      RETURN
      END
