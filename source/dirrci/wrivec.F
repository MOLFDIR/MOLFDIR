C
      SUBROUTINE WRIVEC (VECTR,VECTI,DR,DI,ILIND,IRIND)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'files.inc'
      INCLUDE 'general.inc'
      INCLUDE 'iterat.inc'
C
      DIMENSION VECTR(NDET,*),VECTI(NDET,*),DR(*),DI(*)
      DIMENSION ILIND(*),IRIND(*)
      DIMENSION IDREC(N8)
C
      PARAMETER (NDREC=1023)
#if defined (BIT64)
      PARAMETER (LENREC=24*(NDREC+1))
#else
      PARAMETER (LENREC=20*(NDREC+1))
#endif
C
      OPEN (MRCFINV,FILE=FNAM(5),ACCESS='DIRECT',RECL=LENREC)
      IREC = 3
      IF (ISTART.EQ.1) THEN
         READ(MRCFINV,REC=2) NCOEF,(IRIND(J),J=1,NCOEF)
      ELSE
         READ(MRCFINV,REC=2) NCOEF,(INDMIN(J),J=1,NCOEF)
      ENDIF
      DO IROOT = 1, NROOTS
      IVEC = IROOT
      CALL ZZERO (NDET,VECTR(1,IVEC))
      IF (.NOT.REALAR) CALL ZZERO (NDET,VECTI(1,IVEC))
      ENDDO
C
      KVEC = NROOTS + 1
      DO 20 N = 1, NVEC
      CALL  GETVEC (N,VECTR(1,KVEC),VECTI(1,KVEC))
C
      DO 10 IROOT = 1, NROOTS
      IVEC = IROOT
      ER = EVECR(N,NSEL(IROOT))
#if defined (BIT64)
      CALL SAXPY (NDET,ER,VECTR(1,KVEC),1,VECTR(1,IVEC),1)
#else
      CALL DAXPY (NDET,ER,VECTR(1,KVEC),1,VECTR(1,IVEC),1)
#endif
      IF (.NOT.REALAR) THEN
         EI = EVECI(N,NSEL(IROOT))
#if defined (BIT64)
         CALL SAXPY (NDET,EI,VECTR(1,KVEC),1,VECTI(1,IVEC),1)
         CALL SAXPY (NDET,ER,VECTI(1,KVEC),1,VECTI(1,IVEC),1)
         CALL SAXPY (NDET,-EI,VECTI(1,KVEC),1,VECTR(1,IVEC),1)
#else
         CALL DAXPY (NDET,EI,VECTR(1,KVEC),1,VECTI(1,IVEC),1)
         CALL DAXPY (NDET,ER,VECTI(1,KVEC),1,VECTI(1,IVEC),1)
         CALL DAXPY (NDET,-EI,VECTI(1,KVEC),1,VECTR(1,IVEC),1)
#endif
      ENDIF
   10 CONTINUE
   20 CONTINUE
      DO 30 IROOT = 1, NROOTS
      IVEC = IROOT
      NONZERO = 0
      IDREC(IROOT) = IREC
      DO 11 ID = 1, NDET
         IF (VECTR(ID,IVEC).NE.0.D0.OR.VECTI(ID,IVEC).NE.0.D0) THEN
            IF (NONZERO.EQ.NDREC) THEN
               IF (REALAR) THEN
                  WRITE (MRCFINV,REC=IREC) IREC + 1, NONZERO,
     &                  (ILIND(IDNZ),IDNZ=1,NONZERO),
     &                  (DR(IDNZ),IDNZ=1,NONZERO)
               ELSE
                  WRITE (MRCFINV,REC=IREC) IREC + 1, NONZERO,
     &                  (ILIND(IDNZ),IDNZ=1,NONZERO),
     &                  (DR(IDNZ),DI(IDNZ),IDNZ=1,NONZERO)
               ENDIF
               IREC = IREC + 1
               NONZERO = 0
            ENDIF
            NONZERO = NONZERO + 1
            ILIND(NONZERO) = ID
            DR(NONZERO) = VECTR(ID,IVEC)
            IF (.NOT.REALAR) DI(NONZERO) = VECTI(ID,IVEC)
         ENDIF
   11    CONTINUE
      IF (REALAR) THEN
         WRITE (MRCFINV,REC=IREC) 0, NONZERO,
     &         (ILIND(IDNZ),IDNZ=1,NONZERO),
     &         (DR(IDNZ),IDNZ=1,NONZERO)
      ELSE
         WRITE (MRCFINV,REC=IREC) 0, NONZERO,
     &         (ILIND(IDNZ),IDNZ=1,NONZERO),
     &         (DR(IDNZ),DI(IDNZ),IDNZ=1,NONZERO)
      ENDIF
      IREC = IREC + 1
C
C     Calculate norm reference part op the vector
C
#if defined (BIT64)
      RES = 0.0E0
#else
      RES = 0.0D0
#endif
      IF (ISTART.EQ.1) THEN
        DO 12 IREF = 1, NCOEF
          RES = RES + VECTR(IRIND(IREF),IVEC) * VECTR(IRIND(IREF),IVEC)
          IF (.NOT.REALAR)
     &    RES = RES + VECTI(IRIND(IREF),IVEC) * VECTI(IRIND(IREF),IVEC)
  12    CONTINUE
      ELSE
        RES = RES + VECTR(INDMIN(IVEC),IVEC) * VECTR(INDMIN(IVEC),IVEC)
      ENDIF
      REFVEC(IROOT) = RES
C
   30 CONTINUE
C
      WRITE (MRCFINV,REC=1) LENREC,NDET,NROOTS,
     &                      ((EIGEN(NSEL(IROOT))+ECORE),IROOT=1,NROOTS),
     &                      (IDREC(IROOT),IROOT=1,NROOTS)
      CLOSE (MRCFINV,STATUS='KEEP')
C
      RETURN
      END
