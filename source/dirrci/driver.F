C
      SUBROUTINE DRIVER
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ======================================
C     DRIVES DAVIDSON DIAGONALISATION PROCES
C     ======================================
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'memory.inc'
      INCLUDE 'mempoint.inc'
C
      LOGICAL ENDRUN
c
      MVEC = 0
      NVEC = 0
C
C     Fill Core, and if IGENEX=2 also MRCEXCS
C     with one-electron coupling coefficients
C
      CALL MAKIDS (CI(IPIL),CI(IPIR),CI(IIJR),
     &             CI(IPJL),CI(IPJR),CI(IPJV),
     &             CI(IVW1))
C
C     Construct diagonal elements CI matrix
C
      CALL DIAGELM (CI(IPDR),CI(IPDG),CI(ITNR),
     &              CI(ITNI),CI(IIJE),CI(IGMR),
     &              CI(IGMI),CI(IPIL),CI(IPIR),
     &              CI(IPJL),CI(IPJR),CI(IPJV),
     &              CI(IVW1),CI(IIJR))
C
      CALL INITVEC
C
      IF (RESTART) THEN
         CALL REAVEC (CI(IPVR),CI(IPVI),CI(IPDR),
     &                CI(IPDI),CI(IPIL))
      ELSE
         LCOREX = NCOREX / INTOWP(1) + MOD(NCOREX,INTOWP(1))
         CALL TRIAL (CI(IPVR),CI(IPVI),CI(IPIL),
     &               CI(IPIR),CI(IPJL+LCOREX),
     &               CI(IPJR+LCOREX),CI(IVW1),
     &               CI(IPDG))
      ENDIF
C
      WRITE (6,1000)
C
      DO 200 ITER = 1, MAXITER
         CALL CPUUSED(CPITER)
         CALL EXPAND (CI(IPDR),CI(IPDI),CI(IPVR),
     &                CI(IPVI),CI(IPSR),CI(IPSI),
     &                CI(IPIL),CI(IPIR),CI(IPJL),
     &                CI(IPJR),CI(IPJV),CI(ITNR),
     &                CI(ITNI),CI(IIJE),CI(IGMR),
     &                CI(IGMI),CI(IVW1),
     &                CI(IVW1+IVWW),CI(IIJR))
         CALL INPROD (CI(IPVR),CI(IPVI),CI(IPSR),
     &                CI(IPSI))
         CALL DIAG
         CALL SELECV
         CALL WRIVEC (CI(IPVR),CI(IPVI),CI(IPDR),
     &                CI(IPDI),CI(IPIL),CI(IPIR))
         CALL RESIDUE (CI(IPVR),CI(IPVI),CI(IPSR),
     &                 CI(IPSI))
         CALL CRITER  (ENDRUN)
         IF (ENDRUN) GOTO 300
         CALL PRECOND (CI(IPDG),CI(IPVR),CI(IPVI))
         CALL ORTHN (CI(IPVR),CI(IPVI))
  200 CONTINUE
  300 CONTINUE
      RETURN
C
 1000 FORMAT (//1X,'Iter',1X,'Root',1X,'Isel',13X,'Energy',
     & 7X,'Correlation',5X,'Convergence',3X,'Residue')
C
      END
