C
C     ==========================
      SUBROUTINE PROFIL(IOM,ITP)
C     ==========================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
C
      CHARACTER*5 LS,LTYP*1
      DIMENSION LS(2),LTYP(0:N4),NCFULL(N16)
      DATA LS,LTYP /'LARGE','SMALL','S','P','D','F','G','H'/
C
      WRITE (*,1100) LS(IOM),LTYP(ITP)
      NCON = NCONTR (IOM,ITP)
      NCO = 0
      NPR = NPRIM (IOM,ITP)
      RNT = DBLE (ITP)
C     ------------------------------
C     COPY FUNCTIONS INTO WORK ARRAY
C     ------------------------------
      DO 20 I = 1, NCON
      DO 10 J = 1, NPR
      CR (J,I) = CG (J,I,ITP,IOM)
   10 CONTINUE
   20 CONTINUE
      DO 30 J = 1, NPR
      ER (J) = EG (J,ITP,IOM)
   30 CONTINUE
C     ---------------
C     CALCULATE NORMS
C     ---------------
      IFULL = 0
      DO 100 I = 1, NCON
      CALL NORMF (RNT,NPR,ER,CR(1,I),CNORM)
      IF (CNORM.NE.0.D0) THEN
         IFULL = IFULL + 1
         NCFULL (IFULL) = I
      ENDIF
      IND = INDMO (I,ITP,IOM)
      IF (IND.GT.NMO) THEN
         WRITE (*,1110) I,MOS(IND,1),CNORM
      ELSEIF (IND.GT.0) THEN
         MO = MOS (IND,1)
         WRITE (*,1120) I,MO,ORBENE(MO),CNORM
      ELSE
         IND = - IND
         JTP = IND / (N16 + 1) - 1
         J = MOD (IND,(N16+1))
         WRITE (*,1130) I,LS(1),LTYP(JTP),J,CNORM
      ENDIF
  100 CONTINUE
C     ------------------
C     CALCULATE OVERLAPS
C     ------------------
      WRITE (*,1140)
      IFUL1 = - 5
      IFUL2 =   0
  110 IFUL1 = MIN0 (IFULL,IFUL1 + 6)
      IFUL2 = MIN0 (IFULL,IFUL2 + 6)
      WRITE (*,1150) (NCFULL(IX),IX=IFUL1,IFUL2)
      DO 200 IFU = IFUL1, IFULL
      I = NCFULL (IFU)
      DO 120 JFU = IFUL1,MIN0(IFUL2,IFU)
      J = NCFULL (JFU)
      CALL OVRLAP (RNT,NPR,ER,CR(1,I),CR(1,J),OVLAP)
      DNORM (J) = OVLAP
  120 CONTINUE
      WRITE (*,1160) IFU,(DNORM(J),J=IFUL1,MIN0(IFUL2,IFU))
  200 CONTINUE
      IF (IFUL2.NE.IFULL) GOTO 110
 1100 FORMAT (/3X,'-----------',A5,1X,A1,' PROFILE ------------'//
     &8X,'-- THE FUNCTIONS --',T64,'--NORM--')
 1110 FORMAT (I4,' EXTRA FUNCTION ',I3,T60,F12.6)
 1120 FORMAT (I4,' PART OF VECTOR ',I4,
     &' ( ORB. EN. ',F14.7,' )',T60,F12.6)
 1130 FORMAT (I4,' KINETIC BALANCE ON ',A5,1X,A1,I3,T60,F12.6)
 1140 FORMAT (/8X,'OVERLAP OF THE FUNCTIONS'/)
 1150 FORMAT (/6X,6(3X,I4,5X))
 1160 FORMAT (I5,1X,6(G11.5,1X))
      RETURN
      END
