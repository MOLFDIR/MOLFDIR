c
C     ================
      BLOCK DATA INITC
C     ================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "files.inc"
C
      DATA MFDSYMC, MFD1OUT, MFDVECA, MFDHER, AOPROPER,MDPROP
     +     /10,11,12,13,14,15/
      DATA FNAME / 'MFDSYMC ','MFD1OUT ',
     +             'MFDVECA ','MFDHERM ',
     +             'AOPROPER','MDPROP  '/
      DATA VERSION /'1.0      '/
C
C     1.0    Quick & dirty adaptation of some ROTRAN routines to get a
C            transformation of HERMIT 1-e integrals into MOLFDIR MO-basis.
      END
