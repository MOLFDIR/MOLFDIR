C
C VERSION : $Revision: 1.2 $
C DATE    : $Date: 2001/09/11 12:53:16 $
C FILE    : files.F
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/files.inc,v $
C
C     Files to be used in RELCCSD
      PARAMETER (NCCFIL=11) ! Number of word addressable files
C     Define the unit numbers
      PARAMETER (MSLVOUT = 12) ! Output from slaves
      PARAMETER (MRCONEE = 13) ! Effective one-electron integrals
      PARAMETER (MDINT   = 14) ! Two-electron integrals
      PARAMETER (MDPROP  = 15) ! Property integrals
      PARAMETER (MCCRES  = 16) ! Restart information
      PARAMETER (ITAMPL  = 26) ! T1 and T2 amplitudes
      PARAMETER (ITAMPD  = 27) ! Error vector for T1 and T2 amplitudes
      PARAMETER (IOAMPL  = 28) ! Orbital gradients
      PARAMETER (IOAMPS  = 29) ! Sigma vector for orbital gradients
      PARAMETER (ITAPT   = 40) ! Sorted integral files
      PARAMETER (ITAPL   = 50) ! DZ intermediate for Lambda equations
C     The standard input and output units may be redefined in the code
      COMMON /FILES/ IN,IW
C     Length of the records (integer words) on the restart file
      PARAMETER (RSRCLEN=1024)
      COMMON/RSFILE/ IRECIN,IRECST,IRECEN,IRECT1,NRECT1,IRECT2,NRECT2,
     >               LRSFIL,LCCFIL
      COMMON/PARNAM/ RSBASF,RSFILN,CCBASF,CCFILN
      CHARACTER*6  RSBASF      ! generic name for RESTART file
      CHARACTER*11 RSFILN      ! node-unique RESTART file name of length LRSFIL
      CHARACTER*11 CCBASF      ! generic name for RELCCSD output file
      CHARACTER*15 CCFILN      ! node-unique RELCCSD output file name
