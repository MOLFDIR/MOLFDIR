C
      BLOCK DATA DIRDAT
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'files.inc'
      INCLUDE 'iterat.inc'
C
      DATA EIGEN1/N8 * 0.D0/
      DATA MRCONEE,MDCINT,MDBINT,MRCTRIV,MRCFINV,MRCEXCS,MRCVECS,
     &     MRCTWOE /7,8,9,10,11,12,13,14/
      DATA FNAM /'MRCONEE','MDCINT','MDBINT','MDTRIV','MRCFINV',
     &     'MRCEXCS','MRCVECS','MRCTWOE'/
C
      END
