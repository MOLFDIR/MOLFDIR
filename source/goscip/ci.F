      PROGRAM CI
C     ===============================================================
C     SMALL CI PROGRAM TO PERFORM COMPLETE OPEN SHELL CI CALCULATIONS
C     METHOD DESCRIBED IN JCP 96, 2910, (1992).
C     FIRST VERSION WRITTEN NOVEMBER 1990 BY O. VISSER
C     THEORETICAL CHEMISTRY DEPARTMENT,
C     UNIVERSITY OF GRONINGEN (RUG), GRONINGEN,THE NETHERLANDS
C     ==========================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
C
      CHARACTER*8 DATEX*10,TIMEX
C
      EXTERNAL INIT
      CALL DAYTIME(DATEX,TIMEX)
      WRITE(6,1000) DATEX,TIMEX
      CALL CPUUSED(SEC)
      CALL READIN
      CALL RDTWO
      DO 10 IRP=1,NREP
        IRRP = IRP
        IF (MOD(NELEC,2).EQ.0) IRRP = IRP + NREP
	CALL CPUUSED(SEC0)
        CALL GENDET(IRP,IRRP)
        IF (NDET.EQ.0) GOTO 10
	CALL CPUUSED(SEC1)
        CALL MAKEH
	CALL CPUUSED(SEC2)
        CALL DIAGH(IRP)
	CALL CPUUSED(SEC3)
        CALL WRVEC(IRP,IRRP)
	CALL CPUUSED(SEC4)
        WRITE (6,1010) SEC1-SEC0,SEC2-SEC1,SEC3-SEC2,SEC4-SEC3
        CALL PRTOUT(IRP,IRRP)
 10     CONTINUE
      INTREP = MOD(NELEC,2)
      CALL PRTOUT(0,INTREP)
      CALL CPUUSED(SEC5)
      WRITE (6,1020) SEC5-SEC
 1000 FORMAT(/1X,'GOSCIP (Version 1.9)'//
     &' Today is :',T15,A10/' The time is :',T17,A8//)
 1010 FORMAT(/' CPU time for this representation '//
     &' Generation of determinants :',T30,F12.4/
     &' Building the CI matrix :',T30,F12.4/
     &' Diagonalization :',T30,F12.4/
     &' Writing CI vectors :',T30,F12.4)
 1020 FORMAT (//'Total CPU time :',T30,F12.4//8X,
     &          '(NORMAL END OF PROGRAM)')
C
C VERSION 0.1: SEEMS TO BE WORKING FOR P-SHELLS OF ATOM
C              POPULATIONS ARE GIVEN ONLY IF $POPAN IS PRESENT
C                                                   OV, 19-11-1990
C VERSION 0.2: COMPRESSED LIST OF EIGENVALUES GIVEN AT THE END OF THE OUTPUT
C                                                   OV, 17-12-1990
C VERSION 0.3: TAKE CARE OF BREIT INTERACTION IN CALCULATION OF ENERGY
C                                                   LV, 21-12-1990
C VERSION 0.4: ADAPTED TO  CRAY
C                                                   LV, 23-1-1991
C VERSION 0.5: GIVE POPULATION ANALYSIS IN DIFFERENT FORM IF DESIRED
C                                                   OV, 7-2-1991
C VERSION 0.6: BUG FIX IN MAKONE (OPEN-CLOSED AND CLOSED-OPEN INTEGRALS)
C                                                   LV, 17-7-1991
C
C VERSION 0.7: READ ONE ELECTRON INTEGRALS AND OTHER INFORMATION FROM
C              MRCONEE                              LV, 10-9-1991
C
C VERSION 0.8: BUGFIX WRITING CI VECTORS (CASE OF 0 DETS IN REPRESENTATION)
C                                                   LV, 25-11-1991
C VERSION 1.0: WORKS WITH ABELIAN SYMMETRY
C                                                   LV, 17-02-1992
C VERSION 1.1: MINOR BUGFIXES LV 2-3-1992
C VERSION 1.2: OPTION TO SPECIFY NUMBER OF ELECTRONS IN IREF (LV 8-9-1992)
C VERSION 1.3: REPLACE ' AND " BY 1 AND 2 IN NAMES OF CIVECFILES (LV SEPT.'92)
C VERSION 1.4: ADAPTED TO HP WORKSTATIONS   (BDJ MAY 1993)
C VERSION 1.5: SX2 CALLS REMOVED. PROGRAM ONLY WORKS FOR HP, CRAY, CONVEX AND
C              CYBER  (BDJ JULY '93)
C VERSION 1.6: Cleaned up, Ported to IBM RS6000
C              LV (April 8, 1994)
C VERSION 1.7: Uses time-reversal unique canonical list of integrals
C              LV (June 28, 1994)
C VERSION 1.8: Now also real groups can be handled al real
C              LV
C VERSION 1.9: Changed input to work under Fortran 90
C              BDJ (April 1, 1998)
      END
