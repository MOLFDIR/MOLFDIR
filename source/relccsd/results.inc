C
C VERSION : $Revision: 1.1 $
C DATE    : $Date: 2001/09/11 12:57:32 $
C FILE    : results.inc
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/results.inc,v $
C
C     Register the status of the independent parts of the code
C
C     Status given by integer : 
C     0 - Never asked for
C     1 - On menu but not yet called
C     2 - Running
C     3 - Completed, restart information available
C     4 - Completed, no restart information
C
C     The indices of the different tasks are :
C      1 - Integral sort up to VOVO integrals
C      2 - Integral sort of VOVV and VVVV integrals
C      3 - Construction of Fock matrix
C      4 - MP2  energy and T1 + T2 amplitude calculation
C      5 - CCSD energy and T1 + T2 amplitude calculation
C      6 - CCSD(T) energy calculation
C      7 - 30 reserved for future use
C
      PARAMETER (NSTATS=30)
      COMMON/STATUS/IMSTAT(NSTATS)
C
C     Gather results of the energy calculations
C     ETOT is the energy at the current highest level of theory
C
      COMMON/ENERG/ECORE,ETOT,ESCF,EMP2,ECCSD,ET1,ET2,ET3
      REAL*8 ECORE,ETOT,ESCF,EMP2,ECCSD,ET1,ET2,ET3
