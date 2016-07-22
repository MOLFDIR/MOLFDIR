C     =================
      BLOCK DATA SCFDAT
C     =================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     INITIALIZE DATA
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
C
#if defined (BIT64)
      DATA C/137.0359895/
#else
      DATA C/137.0359895D0/
#endif
      DATA ONLYONE,ENDRUN/.FALSE.,.FALSE./
      DATA SACK,PRTOD,PRTRD/.FALSE.,.TRUE.,.TRUE./
      DATA FNAMES/'SRTTWLL','SRTTWSL','SRTTWSS',
     &            'SRTLBLL','SRTLBSL','SRTLBSS',
     &            'SRTBREI','SRTBREL'/
      DATA FNAM / 'MFDOEMR', 'MFDVECB', 'MFDONEE','MFDSYMC',
     +            'MFDOEMI', 'MFDENSM', 'SSFOCK',
     +            'MFDVECA', 'SRTPQIL', 'SRTPQLN' /
      DATA INTFILE,LBLFILE/40,41,42,43,50,51,52,53/
      DATA ICYCLS,KCYCLS/0,0/
      DATA VERS0,VERS1,VERS2,T0,T1,T2,D0,D1,D2/9*' UNDEF. '/
      DATA POTNUC,ETOTAL,EPREV,EMDIF,WDIF/0.D0,2*1.D10,2*0.D0/
      DATA MFDSYMC,MFDONE ,MFDOEMR,MFDOEMI,
     &     SRTTWLL,SRTTWSL,SRTTWSS,SRTLBLL,SRTLBSL,SRTLBSS,SRTBREI,
     &     SRTBREL,MFDVECB,MFDENSM,SSFOCK,MFDVECA,SRTPQIL,SRTPQLN
     &     /64,62,60,70,40,41,42,50,51,52,43,53,61,66,71,67,46,47/
      DATA EXTRPOL/'  NONE','AITKEN','  SACK'/
      DATA NREC/13/
      DATA SELECT/.FALSE./
      DATA VERSION/'2.5       '/
C
C======================================================================
C  VERSION 2.2   : Updated default speed of light
C                  previous value 137.03602
C                  present value  137.0359895
C
C                                           LV, 1994-06-01
C======================================================================
C  VERSION 2.2.1 : Commented some obsolete TWOC code in MFDPOEF
C                  Corrected error related with that (diag sSS)
C
C                                           LV, 1994-06-01
C======================================================================
C  VERSION 2.3   : Corrected small bug in Gaunt
C
C                                           LV, 1994-09-08
C======================================================================
C  VERSION 2.4   : Reads variable MFIL (number of Coulomb 2-e files)
C                  Adapted to 2-open shell and all representations SCF
C                                           LV, 1994-09-29
C======================================================================
C  VERSION 2.5   : Changed readin for use with Fortran 90           
C
C                                          BDJ, 1998-04-01
C======================================================================
      END
