C
C     ===========================
      SUBROUTINE PRTOUT(IRP,IRRP)
C     ===========================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'general.inc'
C
      PARAMETER (EVOLT=27.21139590988D0,CM=219474.63068D0)
C
      CHARACTER*1 N(N1)
C
      INTEGER IRPS(N2*N5),IND(N5)
      REAL*8 DS(N2*N5)
C
      NAMELIST/POPAN/THRESH,DEGEN,SELPOP
C
#if defined (BIT64)
      ZABS(XXX)=ABS(XXX)
#else
      ZABS(XXX)=DABS(XXX)
#endif
C
#if defined (CRAY)
      IAND(I,J)=AND(I,J)
      ISHIFT(I,J)=SHIFT(I,J)
#else
      ISHIFT(I,J)=ISHFT(I,J)
#endif
C
      THRESH=1.0D-3
      SELPOP=1.0D2
      DEGEN=1.0D-10
      NDETI(IRP)=NDET
      CALL LOCATE('POPAN',5,ITEST)
      IF (ITEST.GT.0) READ(5,POPAN)
      IF (IRP.EQ.0) GOTO 110
      IF (IPRNT.LT.5) RETURN
C
C     ------------------------------------------------
C     PRINT POPULATION ANALYSIS FOR REPRESENTATION IRP
C     ------------------------------------------------
C
      IF (IPRNT.GE.20) GOTO 109
      DO 10 I=1,NDET
        DO 20 J=1,NDET
          HR(J,I)=HR(J,I)*HR(J,I)+HI(J,I)*HI(J,I)
 20       CONTINUE
 10     CONTINUE
C
      DO 70 I=1,NDET
        IF (D(I,IRP)-D(1,IRP).LT.SELPOP) MX=I
 70     CONTINUE
C
      WRITE(6,1006) REPNA(IRRP),MX
 1006 FORMAT(/1X,'POPULATION ANALYSIS FOR REPRESENTATION ',A4/
     +        1X,'           THE FIRST ',I4,' VECTORS ARE ANALYZED'//)
C
      DO 30 I=1,MX
        WRITE(6,1000) D(I,IRP)+ECORE
        DO 40 J=1,NDET
          IF (HR(J,I).GT.THRESH) THEN
            DO 50 JJ=1,NORB
              N(JJ)='0'
              IF (IAND(IDET(J),ISHIFT(1,JJ-1)).NE.0) N(JJ)='1'
 50           CONTINUE
            DO 60 JJ=NORB+1,N1
              N(JJ)=' '
 60           CONTINUE
            WRITE(6,1008) J,(N(JJ),JJ=1,N1),HR(J,I)
          ENDIF
 40       CONTINUE
 30     CONTINUE
      RETURN
C
 109  DO 270 I=1,NDET
        IF (D(I,IRP)-D(1,IRP).LT.SELPOP) MX=I
 270    CONTINUE
C
      WRITE(6,1007) REPNA(IRRP),MX
 1007 FORMAT(/1X,'POPULATION ANALYSIS FOR REPRESENTATION ',A4/
     +        1X,'           THE FIRST ',I4,' VECTORS ARE ANALYZED'//)
C
      WRITE(6,'(A)') 'NUMBERS: HR, HI, HR**2+HI**2'
      DO 230 I=1,MX
        WRITE(6,1000) D(I,IRP)+ECORE
        DO 240 J=1,NDET
          IF (ABS(HR(J,I)).GT.THRESH) THEN
            DO 250 JJ=1,NORB
              N(JJ)='0'
              IF (IAND(IDET(J),ISHIFT(1,JJ-1)).NE.0) N(JJ)='1'
 250          CONTINUE
            DO 260 JJ=NORB+1,N1
              N(JJ)=' '
 260          CONTINUE
            WRITE(6,1009) J,(N(JJ),JJ=1,N1),HR(J,I),HI(J,I),
     &                    HR(J,I)*HR(J,I)+HI(J,I)*HI(J,I)
          ENDIF
 240      CONTINUE
 230    CONTINUE
      RETURN
C
C     ------------------------------------
C     PRINT COMPRESSED LIST OF EIGENVALUES
C     ------------------------------------
C
 110  CALL SORTEW (D,DS,NDETI,NDETT,NREP,IRPS)
      WRITE(6,1005)
      ILEVEL=0
      EW=0.0D0
      EWF=DS(1)
      NEW=0
      AVER=0.0D0
      DO 120 I=1,NDETT
        AVER=AVER+DS(I)
        IF (ZABS(DS(I)-EW).GT.DEGEN) THEN
          IF (NEW.NE.0)
     &      WRITE(6,1010) ILEVEL,EW-EWF,EW+ECORE,NEW
          ILEVEL=ILEVEL+1
          NEW=1
          EW=DS(I)
        ELSE
          NEW=NEW+1
          EW=((NEW-1)*EW+DS(I))/DBLE(NEW)
        ENDIF
 120    CONTINUE
      IF (NEW.NE.0)
     &WRITE(6,1010) ILEVEL,EW-EWF,EW+ECORE,NEW
      WRITE(6,1020) AVER/DBLE(NDETT)+ECORE
C
      IF (IRRP.EQ.0) WRITE(6,1025) (REPNA(I),I=NREP+1,2*NREP)
      IF (IRRP.EQ.1) WRITE(6,1025) (REPNA(I),I=1,NREP)
      ILEVEL=0
      EW=0.0D0
      EWF=DS(1)
      NEW=0
      DO 140 I=1,NDETT
        IF (ZABS(DS(I)-EW).GT.DEGEN) THEN
         IF (NEW.NE.0)
     &      WRITE(6,1030) ILEVEL,EVOLT*(EW-EWF),CM*(EW-EWF),
     &                    (IND(JRP),JRP=1,NREP)
          DO 130 JRP = 1, NREP
            IND(JRP) = 0
 130        CONTINUE
          ILEVEL = ILEVEL + 1
          IND(IRPS(I)) = 1
          NEW=1
          EW=DS(I)
        ELSE
          NEW=NEW+1
          IND(IRPS(I)) = IND(IRPS(I)) + 1
          EW=((NEW-1)*EW+DS(I))/DBLE(NEW)
        ENDIF
 140    CONTINUE
      IF (NEW.NE.0) WRITE(6,1030) ILEVEL,EVOLT*(EW-EWF),CM*(EW-EWF),
     &              (IND(JRP),JRP=1,NREP)
 1000 FORMAT(1X,F16.8)
 1008 FORMAT(17X,I8,2X,16(A1),F16.8)
 1009 FORMAT(17X,I8,2X,16(A1),3(F16.8))
 1005 FORMAT(//1X,'ENERGY EIGENVALUES IN ATOMIC UNITS'
     +            //1X,'LEVEL   REL EIGENVALUE     ',
     +            'ABS EIGENVALUE  DEGENERACY'/)
 1010 FORMAT(1X,I4,1X,F16.12,F20.12,1X,'(',I4,' * )')
 1020 FORMAT(//1X,'TOTAL AVERAGE: ',F20.10)
 1025 FORMAT(//1X,'RELATIVE EIGENVALUES IN OTHER UNITS;'/' SYMMETRY',
     +       ' CLASSIFICATION IN THE ABELIAN SUBGROUP',
     +       //1X,'LEVEL  EIGENVALUE (EV) ',
     +      ' EIGENVALUE (CM-1)',2X,8A4/)
 1030 FORMAT(1X,I4,1X,F18.9,F18.6,1X,8(I3,1X))
      RETURN
      END
