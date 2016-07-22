C
C     ==================
      SUBROUTINE POPULAT
C     ==================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     =========================================
C     Determine contents of the representations
C     =========================================
C
      INCLUDE 'paracom.inc' 
C
      INBB = 0
      DO 400 IRP = 1, NSYMRP
        NB1 = NBSYM1(IRP)
        NB2 = NBSYM2(IRP)
        NB = NBSYM(IRP)
C
        NGR(IRP) = 0
        DO 100 I = 1, N7
        KNDGR(I,IRP) = 0
        INDGR(I,IRP) = 0
  100   CONTINUE
C
        INBA = INBB + 1
        INBB = INBB + NB
        IF (.NOT.TWOC) THEN
           INBBB = INBB
        ELSE
           INBBB = INBB - NB2
        ENDIF
        DO 300 I = INBA, INBBB
           IT = KSYMF(I)
           IM = INDB(IT)
           II = I - INBA + 1
           KCM = KCOMB(IM)
           INDGR(II,IRP) = INDG(KCM)
 300       CONTINUE
C
        DO 320 II = 1, INBBB - INBA + 1
           DO 310 IGR = 1, NGR(IRP)
              IF (INDGR(II,IRP).EQ.KNDGR(IGR,IRP)) GOTO 320
 310          CONTINUE
           NGR(IRP) = NGR(IRP) + 1
           KNDGR(NGR(IRP),IRP) = INDGR(II,IRP)
 320       CONTINUE
C
 400    CONTINUE
      RETURN
      END
