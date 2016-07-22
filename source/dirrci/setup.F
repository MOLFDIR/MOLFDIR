C
      SUBROUTINE SETUP
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ============================================================
C     SETS UP CI SPACE: CALCULATES DIAGRAM, GENERATES DETERMINANTS
C     MAKES G-MATRIX, REORDERS 2-ELECTRON INTEGRALS
C     ============================================================
C
      INCLUDE 'param.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'general.inc'
      INCLUDE 'memory.inc'
      INCLUDE 'iterat.inc'
      INCLUDE 'mempoint.inc'
C
      CHARACTER*8 TIMEX,DATEX*10
C
      CALL DAYTIME (DATEX,TIMEX)
      WRITE (6,1000) DATEX,TIMEX
      CALL CPUUSED(CPUSET)
C
      CALL ORBSIN
      CALL USERIN
      CALL SORTMEM
      CALL SHUFFLE (CI(ITR),CI(ITI),CI(IIK),
     &              CI(IIL),CI(ITIR),CI(ITII))
      CALL VERTEX (CI(IVW1))
      CALL CIMEM
      CALL MAKEGM (CI(IGMR),CI(IGMI),CI(ITNR),
     &             CI(ITNI),CI(IIJE))
C
      DO 1 I = 1, N2
      DO 1 J = 1, N2
         CMTR(I,J)=0.0D0
         CMTI(I,J)=0.0D0
         EVECR(I,J)=0.0D0
         EVECI(I,J)=0.0D0
    1 CONTINUE
C
      WRITE (6,1010) REPNA(IREP),NREFDET,NDET,NROOTS,
     &               ISTART,SELECT,(NSEL(I),I=1,NROOTS)
      WRITE (6,1011) MAXITER,CONVERE,CONVERR,CPUMAX,RESTART
      IF (GETDET) THEN
	 WRITE (6,1012) GETDET,COMIN,MAKENAT
      ELSE 
	 WRITE (6,1013) GETDET,MAKENAT
      ENDIF
      WRITE (6,1020) NELEC,NORB,NORBR
      IF (NORBR(1).EQ.0) MAXH1=0
      IF (NORBR(3).EQ.0) MAXE3=0
      DO 20 J1H = 0, MAXH1
      J1 = NORBR(1) - J1H
      DO 10 J3 = 0, MAXE3
      J2 = NELEC - J1 - J3
      IF (J2.LT.0.OR.J2.GT.NORBR(2)) GO TO 10
      NCDET = NOVERI(NORBR(1),J1)
      NCDET = NOVERI(NORBR(2),J2) * NCDET
      NCDET = NOVERI(NORBR(3),J3) * NCDET
      WRITE (6,1030) J1,J2,J3,NCDET
   10 CONTINUE
   20 CONTINUE
      IF (IGENEX.EQ.1) THEN
         WRITE (6,1040)
      ELSE IF (IGENEX.EQ.2) THEN
         WRITE (6,1041)
      ELSE
         WRITE (6,1042)
         STOP
      ENDIF
C
      CALL CPUUSED (SEC)
      CPUSET = SEC - CPUSET
      WRITE (6,1050) CPUSET
C
#if defined (CRAY) || defined (LINUX) || defined (SUN)
      CALL FLUSH (6)
#endif
#if defined (IBM) 
      CALL FLUSH_(6)
#endif
C
C     Version 1.0 : Abelian Symmetry
C                   Multiroot Optimization
C                   All CI and Sigma vectors in core memory
C                   Needs trial vectors from GOSCIP
C     Version 1.1 : Generates start vectors by taking lowest diagonal elements
C     Version 1.1.1 : Generates only limited set of 1-electron excitations
C                     in 2-electron part.
C     Version 1.1.2 : Option to write 1-electron excitation to file.
C     Version 1.1.3 : Keeps 1-el. excits in core if possible.
C                     Writes CI-Vectors to file to be able to restart.
C     Version 1.2.0 : Calculates Density Matrix
C                     Code adapted to Hewlett Packard Workstations
C     Version 1.2.1 : Special code for integrals with 3 or more external labels
C     Version 1.2.2 : Dynamic Memory Allocation checking.
C     Version 1.3   : Selecting leading determinants.  
C     Version 1.3.1 : Generates MFDVECA file with natural spinors
C     Version 1.3.2 : BugFix. Now generates totaly symmetric natural spinors 
C     Version 1.3.3 : Option to delete virtual spinors 
C     Version 1.4   : Reduction of memory required : write out CI vectors
C                     Deleted CNVE statements
C     Version 1.5   : Ported to IBM RS6000
C     Version 1.6   : Reads new integral format (reduced list)
C                     MRCTWOE is now a real scratch file
C                     Removed check on short vectors : Non-stride variant
C                     in Expand will always be used.
C     Version 1.6.1 : Real arithmetic versions of EXPAND and TWOLIN
C     Version 1.6.2 : Corrected restart bug introduced in version 1.4
C                     Improved calculation of Davidson correction
C                     Changed input defaults for LEADDET nad NATURAL
C     Version 1.6.3 : Corrected bug in EXPAND (1-electron contribution)
C                     Reinstalled ORBDEL option
C     Version 1.6.4 : Can now run with zero iterations to calculate 
C                     natural spinors after a crash
C     Version 1.6.5 : Installed explicit real diagonalization routine
C     Version 2.0   : Large revisions ; 
C                       Better memory management
C			Storing as much coupling coeff. as possible
C			More efficient expansion algorithm
C			Faster calculation of coupling coeff.
C     Version 2.1   : Made input to compile under Fortran 90
C
 1000 FORMAT (' DIRect Relativistic CI program version 2.1'//
     &' Written by Luuk Visscher ',
     &//' This run started at ', A10,1X,A8)
 1010 FORMAT (/' Abelian Representation :',1X,A4/
     &' Number of reference determinants :',I10/
     &' Total number of determinants :',I10/
     &' Number of roots to be optimized :',I3/
     &' Start vectors method :',I3/
     &' Overlap Selection : ',L1/
     &' Position in COSCI space :',(10I4))
 1011 FORMAT (' Maximum number of iterations :',I5/
     &' Energy convergence required :',1X,G10.1/
     &' Residue convergence required :',G10.1/
     &' Maximum of CPU seconds to be used :',F16.1/
     &' Restart calculation : ',L1)
 1012 FORMAT (' Printing contributions of important spinors : ',L1/
     &' Contributions are printed for coefficients larger then :',G10.1/
     &' Generate natural spinors after CI calculation : ',L1)
 1013 FORMAT (' Printing contributions of important spinors : ',L1/
     &' Generate natural spinors after CI calculation : ',L1)
 1020 FORMAT (/' Total number of active electrons :',I5/
     &' Number of active orbitals :',I5//
     &' RAS1 :',I4/' RAS2 :',I4/' RAS3 :',I4//
     &' Allowed RAS configurations :'//
     &'  RAS1  RAS2  RAS3  Determinants (All Representations)')
 1030 FORMAT (3(2X,I4),I12)
 1040 FORMAT (/' 1 and 2-electron excitations will be evaluated ',
     &'directly')
 1041 FORMAT (/' 1-electron excitations will be written to file')
 1042 FORMAT (/' No valid value for IGENEX : choose 1 (direct) or',
     &' 2 (file)')
 1050 FORMAT (/' Setup Routine completed in ',F8.3,' SECONDS')
C
      RETURN
      END
