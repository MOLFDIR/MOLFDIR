      PROGRAM PRTRAN
C     =============================================================
C     Transforms property integrals from HERMIT to MOLFDIR MO-basis
C     LUUK VISSCHER
C     Odense Universitet
C     ======================================================
      IMPLICIT INTEGER (A-Z)
      EXTERNAL INITC
C
      CHARACTER*10 DATEX,TIMEX*8
      REAL*8 SEC0
      PARAMETER (NWRK=13000000)
      REAL*8 WRK(NWRK)
C
      INCLUDE "inpt.inc"
      INCLUDE "molf.inc"
      INCLUDE "files.inc"
C
C     Input and Initialization section
C
      CALL DAYTIME (DATEX,TIMEX)
      WRITE (6, 1000) VERSION, DATEX, TIMEX  
      CALL INIT
      CALL READIN 
      CALL GETMOS
      CALL EXPMOS (NHERP)
      IP1 = 1
      IP2 = IP1 + NHERP * NHERP
      IP3 = IP2 + NHERP * (NHERP + 1) / 2
      IP4 = IP3 + NHERP * NSP * 2
      IP5 = IP4 + NHERP * NSP * 2
      ITOP = IP5 + NSP * NSP * 2
      IF (ITOP.GT.NWRK) THEN
         PRINT*, ' Increase nwrk to ',itop
         STOP ' Increase nwrk'
      ENDIF
      CALL TRANSF (NHERP,WRK(IP1),WRK(IP2),WRK(IP3),WRK(IP4),WRK(IP5))
      CALL CPUUSED (SEC0)
      WRITE (6,1011) SEC0
 1000 FORMAT (' ***********************************'/
     +        ' *                                 *'/
     +        ' *  P R T R A N    VS ',A10,   '   *'/
     +        ' *                                 *'/
     +        ' *  Written by :                   *'/
     +        ' *  Luuk Visscher                  *'/
     +        ' *  Odense Universitet             *'/
     +        ' *                                 *'/
     +        ' ***********************************'///
     +        ' Program start: ',A10,2X,A8/)
 1011 Format (/3x,'Total CPU time for this run ',f10.4,' seconds')
      END
