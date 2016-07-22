C
C     =====================
      FUNCTION IRPDET(JDET)
C     =====================
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
C
      JRRP = 0
      ID=JDET
      DO 10 I=1,NORB
        IF (MOD(ID,2).EQ.1) THEN
          JRRP = MULTB(IRPAMO(I),JRRP)
        ENDIF
        ID=ID/2
 10     CONTINUE
      IRPDET=JRRP
      RETURN
      END
