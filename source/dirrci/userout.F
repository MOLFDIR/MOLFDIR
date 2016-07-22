C
      SUBROUTINE USEROUT
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'files.inc'
      INCLUDE 'iterat.inc'
      INCLUDE 'memory.inc'
      INCLUDE 'mempoint.inc'
C
      DIMENSION OV(N2)
C
      CALL REAVEC (CI(IPVR),CI(IPVI),CI(IPDR),CI(IPDI),CI(IPIL))
C
      DO 40 IROOT = 1, NROOTS
C        ------------------------------------
C        Calculate first order density matrix
C        ------------------------------------
         CALL DENSMT (CI(IPDR),CI(IPDI),CI(IPVR),
     &                CI(IPVI),CI(IPIL),CI(IPIR),
     &                CI(IPJL),CI(IPJR),
     &                CI(IPJV),CI(IIJR),CI(ITNR),
     &                CI(ITNI),CI(IVW1),IROOT,
     &                CI(IIJE+NORB*(IROOT-1)))
C        -------------------------
C        Calculate natural Spinors
C        -------------------------
         IF (MAKENAT) CALL NATORB(IROOT,CI(ITNR), 
     &                            CI(ITNI),CI(IGMR),
     &                            CI(IGMI),CI(IPJV))
   40 CONTINUE
C
      WRITE (6,1020) (IROOT,IROOT=1,NROOTS)
      DO 30 I = 1, NORB
         IRP = IRPAMO(I)
      WRITE (6,1030) I,REPNA(IRP),(CI(IIJE+NORB*(IROOT-1)+I-1),
     &                                      IROOT=1,NROOTS)
  30  CONTINUE
C
      DO 20 IROOT = 1, NROOTS
      ISEL = NSEL(IROOT)
      DO 10 I = 1, NVEC
      OV(I) = EVECR(I,ISEL)*EVECR(I,ISEL) 
      IF (.NOT.REALAR) 
     &   OV(I) = OV(I) + EVECI(I,ISEL)*EVECI(I,ISEL)
   10 CONTINUE
      WRITE (6,1010) IROOT,(OV(I),I=1,NROOTS)
   20 CONTINUE
C
C     Give an overview of the leading determinants
C
      IF (GETDET) CALL LEADING(CI(IVW1))
      WRITE (6,1000)
C
 1000 FORMAT(//10X,'-- Normal end of program --')
 1010 FORMAT(//' Contributions of start vectors to final CI vector',I4
     &/(5(2X,G14.8)))
 1020 FORMAT(/' Occupation numbers : '//' Orbital Representation',
     &5(3X,'Root',I3))
 1030 FORMAT(I6,8X,A4,5X,5(1X,F9.6))
C
      RETURN
      END
