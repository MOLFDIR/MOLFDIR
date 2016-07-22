C
      SUBROUTINE VERTEX(IVWU)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ========================================================
C     WALKS THROUGH DIAGRAM OF THIS RESTRICTED ACTIVE SPACE TO
C     CALCULATE VERTEX WEIGHTS
C     ========================================================
C
      INCLUDE 'param.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'general.inc'
C
      DIMENSION IVWU(16,0:NORB,0:NELEC)
C
      DO 30 I = 0, NORB
      DO 20 J = 0, NELEC
      DO 10 IR = 1, NREP
      IVWU (IR,I,J) = 0
   10 CONTINUE
   20 CONTINUE
   30 CONTINUE
C     ===============================
C     DO THE WALK FROM TOP TO BOTTOM
C     ===============================
      ICOL = MAXH1
      IF (NORBR(1).EQ.0) ICOL = NORBR(2) - NELEC + MAXE3
      DO 50 I = 0, ICOL 
      IVWU(IRA1,I,0) = 1
   50 CONTINUE
C
      IORB = 0
      IORBR(0)=0
C
      IF (NORBR(1).EQ.0) GOTO 1101
C
      DO 100 I = 1, NORBR(1)
      IORB = IORB + 1
      JMIN = MAX0( I - MAXH1, 1)
      IORBR(IORB) = MAX0( I - MAXH1, 0)
      JMAX = IORB
      DO 90  J = JMIN, JMAX
      DO 80 IR = 1, NREP
      JR = MULTB(IR,IRPAMO(IORB))
      IVWU(JR,IORB,J) = IVWU(JR,IORB-1,J) + IVWU(IR,IORB-1,J-1)
   80 CONTINUE
   90 CONTINUE
  100 CONTINUE
C
 1101 IF (NORBR(2).EQ.0) GOTO 2101
C
      DO 200 I = 1, NORBR(2)
      IORB = IORB + 1
      JMAX = MIN0 ( NELEC, IORB)
      JMIN = MAX0 ( NELEC - MAXE3 + I - NORBR(2), NORBR(1) - MAXH1)
      IORBR(IORB) = JMIN 
      DO 190  J = JMIN, JMAX
      IF (J.EQ.0) THEN
         IF (NORBR(1).GT.0) IVWU(IRA1,IORB,J) = 1
         GOTO 190
      ENDIF
      DO 180 IR = 1, NREP
      JR = MULTB(IR,IRPAMO(IORB))
      IVWU(JR,IORB,J) = IVWU(JR,IORB-1,J) + IVWU(IR,IORB-1,J-1)
  180 CONTINUE
  190 CONTINUE
  200 CONTINUE
C  
 2101 IF (NORBR(3).EQ.0) GOTO 3001
C
      DO 300 I = 1, NORBR(3)
      IORB = IORB + 1
      JMIN = MAX0 ( NELEC + I - NORBR(3), NELEC - MAXE3)
      IORBR(IORB) = JMIN 
      JMAX = NELEC
      DO 290  J = JMIN, JMAX
      IF (J.EQ.0) THEN
         IF ((NORBR(1).GT.0).OR.(NORBR(2).GT.0)) IVWU(IRA1,IORB,J) = 1
         GOTO 290
      ENDIF
      DO 280 IR = 1, NREP
      JR = MULTB(IR,IRPAMO(IORB))
      IVWU(JR,IORB,J) = IVWU(JR,IORB-1,J) + IVWU(IR,IORB-1,J-1)
  280 CONTINUE
  290 CONTINUE
  300 CONTINUE
      
C     ======================================================
C     DEFINE MAXIMUM NUMBER OF SYMMETRY ADAPTED DETERMINANTS 
C     ======================================================
 3001 NREFDET = IVWU(IREP,NORBR(1)+NORBR(2),NELEC)
      NDET = IVWU(IREP,IORB,NELEC)
      MDET = NDET
      DO 310 IR = 1, NREP
      IF (IVWU(IR,IORB,NELEC).GT.MDET) MDET = IVWU(IR,IORB,NELEC)
  310 CONTINUE
C
      RETURN
      END
