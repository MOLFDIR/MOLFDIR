C
C     =================
      SUBROUTINE TIMSYM(VR,VI)
C     =================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     FORCE TIME REV SYM
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
C
      REAL*8 VR(NBM,NBM),VI(NBM,NBM),EPSI(N22)
C
      MVBREC=2
      MVAREC = 2*NOCT+2
C
C     ---------------------------------------------------------------------
C     We have 3 ways in which the vectors can be related by time-reversal
C     1-dim irrep : K Psi (Gamma,partner,i) = +/- Psi (Gamma,partner,i+/-1)
C     n-dim irrep : K Psi (Gamma,partner,i) = +/- Psi (Gamma,partner',i)
C     n-dim rep   : K Psi (Gamma,partner,i) = +/- Psi (Gamma',partner',i)
C
C     Furthermore we know that the coefficients for a partners of an irrep
C     are independent of the partner number.
C
C     Now correct and check the vectors obtained previously.
C     ---------------------------------------------------------------------
C     
C
      DO 10 IRP=1,NSYMRP
        DO 20 MO=1,NOC(IRP)
          MVBREC=MVBREC+1
          IF (IKRIRP(IRP).EQ.IRP) THEN
            READ(MFDVECB,REC=MVBREC) IDUM,JDUM,NBAS,EPSI(MO),
     +          (VR(I,MO),VI(I,MO),I=1,NBAS)
          ELSEIF (IKRIRP(IRP).EQ.-IRP) THEN
            IF (MOD(MO,2).NE.0) THEN
               READ(MFDVECB,REC=MVBREC) IDUM,JDUM,NBAS,EPSI(MO),
     +             (VR(I,MO),VI(I,MO),I=1,NBAS)
               EPSI(MO+1) = EPSI(MO)
               DO I = 1, NBAS, 2
                  VR(I+1,MO+1)   =   VR(I  ,MO)
                  VI(I+1,MO+1)   = - VI(I  ,MO)
                  VR(I  ,MO+1)   = - VR(I+1,MO)
                  VI(I  ,MO+1)   =   VI(I+1,MO)
               ENDDO
            ELSE
               WRITE(MFDVECB,REC=MVBREC) IDUM,JDUM,NBAS,EPSI(MO),
     +              (VR(I,MO),VI(I,MO),I=1,NBAS)
            ENDIF
          ELSEIF (IKRIRP(IRP).GT.0) THEN
            WRITE(MFDVECB,REC=MVBREC) IDUM,JDUM,NBAS,EPSI(MO),
     +           (VR(I,MO),VI(I,MO),I=1,NBAS)
          ELSEIF (IKRIRP(IRP).LT.0) THEN
            WRITE(MFDVECB,REC=MVBREC) IDUM,JDUM,NBAS,EPSI(MO),
     +           (VR(I,MO),-VI(I,MO),I=1,NBAS)
          ELSE
            CALL Q2JOB(6,'ERROR IN TIMSYM',' ',1)
          ENDIF
C
 20       CONTINUE
 10   CONTINUE
      DO 40 IRP = 1, NSYMRP
        IF (NBSYM(IRP).GT.N22) CALL Q2JOB(3,'TIMSYM','N22',NBSYM(IRP))
        DO 30 MO=1,NBLR(IRP) + NBSR(IRP)
          MVAREC=MVAREC+1
          IF (IKRIRP(IRP).EQ.IRP) THEN
            READ(MFDVECB,REC=MVAREC) IDUM,JDUM,NBAS,EPSI(MO),
     +          (VR(I,MO),VI(I,MO),I=1,NBAS)
          ELSEIF (IKRIRP(IRP).EQ.-IRP) THEN
            IF (MOD(MO,2).NE.0) THEN
               READ(MFDVECB,REC=MVAREC) IDUM,JDUM,NBAS,EPSI(MO),
     +             (VR(I,MO),VI(I,MO),I=1,NBAS)
               EPSI(MO+1) = EPSI(MO)
               DO I = 1, NBAS, 2
                  VR(I+1,MO+1)   =   VR(I  ,MO)
                  VI(I+1,MO+1)   = - VI(I  ,MO)
                  VR(I  ,MO+1)   = - VR(I+1,MO)
                  VI(I  ,MO+1)   =   VI(I+1,MO)
               ENDDO
            ELSE
               WRITE(MFDVECB,REC=MVAREC) IDUM,JDUM,NBAS,EPSI(MO),
     +              (VR(I,MO),VI(I,MO),I=1,NBAS)
            ENDIF
          ELSEIF (IKRIRP(IRP).GT.0) THEN
            WRITE(MFDVECB,REC=MVAREC) IDUM,JDUM,NBAS,EPSI(MO),
     +           (VR(I,MO),VI(I,MO),I=1,NBAS)
          ELSEIF (IKRIRP(IRP).LT.0) THEN
            WRITE(MFDVECB,REC=MVAREC) IDUM,JDUM,NBAS,EPSI(MO),
     +           (VR(I,MO),-VI(I,MO),I=1,NBAS)
          ELSE
            CALL Q2JOB(6,'ERROR IN TIMSYM',' ',1)
          ENDIF
C
 30       CONTINUE
 40     CONTINUE
      RETURN
      END
