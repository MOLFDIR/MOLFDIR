C
C     ==============================================
      SUBROUTINE SORTEW (D,DS,NDETI,NDETT,NREP,IRPS)
C     ==============================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE 'param.inc'
C
      REAL*8 D(N2,N5),DS(N2*N5)
      INTEGER NDETI(N5),NDETT,IND(N5),IRPS(N2*N5)
C
      INDEX = 1
      IRPMIN = 1
      DO 1 IRP = 2, NREP
    1   IF (D(1,IRP).LT.D(1,IRPMIN)) IRPMIN = IRP
C
      DO 2 IRP = 1, NREP
    2   IND(IRP) = 1
C
      NDETT = 0
      DO 3 IRP = 1, NREP
        NDETT = NDETT + NDETI(IRP)
    3   CONTINUE
C
 10   DS(INDEX)=D(IND(IRPMIN),IRPMIN)
      IRPS(INDEX)=IRPMIN
      INDEX=INDEX+1
      IF (INDEX.GT.NDETT) GOTO 30
      IND(IRPMIN)=IND(IRPMIN)+1
      IF (IND(IRPMIN).GT.NDETI(IRPMIN)) D(IND(IRPMIN),IRPMIN)=1.D12
      DO 20 IRP = 1, NREP
      IF (D(IND(IRPMIN),IRPMIN).GT.D(IND(IRP),IRP)) THEN
        IRPMIN=IRP
      ENDIF
 20   CONTINUE
      GOTO 10
 30   CONTINUE
C
      RETURN
      END
