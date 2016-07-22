C
      SUBROUTINE CIMEM 
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ========================
C     GENERATE MEMORY POINTERS 
C     ========================
C
      INCLUDE 'param.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'general.inc'
      INCLUDE 'mempoint.inc'
C
      MAXB = NROOTS + 1
      MAXC = NROOTS
      NORBC = NORB**3
C
C     store een deel van de common block data
C
      ITNR = IIJR + NORB**2 / INTOWP(1) + MOD(NORB**2,INTOWP(1))
      ITNR = ITNR + MOD(MDET*(NREP/2),INTOWP(1))
      IF (REALAR) THEN
         ITNI = ITNR 
         IIJE = ITNI + NORBC
         IGMR = IIJE + NORBC
         IGMI = IGMR
      ELSE
         ITNI = ITNR + NORBC
         IIJE = ITNI + NORBC
         IGMR = IIJE + NORBC
         IGMI = IGMR + NORBC
      ENDIF
C
      IPDG = IGMI + NORBC
C
      IPDR = IPDG + NDET
      IF (REALAR) THEN
         IPDI = IPDR
         IPVR = IPDR + MDET*MAXC
         IPVI = IPVR
         IPSR = IPVR + NDET*MAXB
         IPSI = IPSR
      ELSE
         IPDI = IPDR + MDET*MAXC
         IPVR = IPDI + MDET*MAXC
         IPVI = IPVR + NDET*MAXB
         IPSR = IPVI + NDET*MAXB
         IPSI = IPSR + NDET*MAXC
      ENDIF
C   
      IPIL = IPSI + NDET*MAXC
      IPIR = IPIL + NDET / INTOWP(1) + MOD(NDET,INTOWP(1))
C     -----------------------------------------------------------
C     USE FREE MEMORY TO STORE ONE ELECTRON COUPLING COEFFICIENTS
C     -----------------------------------------------------------
      IF ((IPIR+5*(NDET/INTOWP(1)+MOD(NDET,INTOWP(1)))).GT.MAXCORE) 
     &   CALL Q2JOB (6,'MEMORY PROBLEMS 1',' ',0)
C
      LREST = MAXCORE - (IPIR+5*(NDET/INTOWP(1)+MOD(NDET,INTOWP(1))))
      NORBD = (LREST - MOD(LREST,4)) / 4
      NELEM = NORB * NORB
      LNORBC = (NORBD - MOD(NORBD,NELEM)) / NELEM
      LNORBC = LNORBC * INTOWP(1) 
      IF (LNORBC.LT.NORB) CALL Q2JOB (6,'MEMORY PROBLEMS 2',' ',0)
C
      IPJL = IPIR + NDET / INTOWP(1) + MOD(NDET,INTOWP(1))
      IPJR = IPJL + NDET / INTOWP(1) + MOD(NDET,INTOWP(1)) + NORBD
      IPJV = IPJR + NDET / INTOWP(1) + MOD(NDET,INTOWP(1)) + NORBD
C
      RETURN
C
      ENTRY SORTMEM
C
C     MEMORY WILL FIRST BE USED AS SCRATCH SPACE FOR
C     SORTING OF INTEGRALS:
C
C     MEMORY IN 8 BYTE WORDS
C     M1 : LENGTH OF BLOCKS
C     M2 : NUMBER OF BLOCKS
C     M3 : MEMORY ACTUALLY USED
C
      M1 = NORB * NORB
      MORB = NORB + NDELETE
      M1A = MORB * MORB
C
      MCORE = (MAXCORE - 4*M1A)/2
      IF (BREIT) THEN
         M2 = MIN0(MCORE/(2*M1),M1)
         M3 = 2 * M1 * M2
      ELSE
         M2 = MIN0(MCORE/M1,M1)
         M3 = M1 * M2
      ENDIF
      ITR = 1
      ITI = ITR + M1A
      IIK = ITI + M1A
      IIL = IIK + M1A
      ITIR = IIL + M1A
      ITII = ITIR + M3
C
C     Also set pointers for vertex weights, etc.
C
      IVWS = 16 * (NORB + 1) * (NELEC + 1)
      IVWW = IVWS / INTOWP(1) + MOD((IVWS),INTOWP(1))
      IVWI = IVWW * INTOWP(1)
      IVW1 = 1               
      IIJR = IVW1 + 3*IVWW
C
      RETURN
C
      END
C
      INTEGER FUNCTION INTOWP(N)
C
#if defined (BIT64)
      INTOWP = N 
#else
      INTOWP = 2 * N
#endif
C
      RETURN
      END
