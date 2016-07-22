C
      SUBROUTINE PutVec (ivec,VecR,VecI)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'files.inc'
C
      Real*8 VecR(*),VecI(*),SigR(*),SigI(*)
C
      irec = ivec * 2 - 1
      IF (REALAR) THEN
         Write (MRCVECS,REC=IREC) (VECR(I),I=1,NDET)
      ELSE
         Write (MRCVECS,REC=IREC) (VECR(I),VECI(I),I=1,NDET)
      ENDIF
      Return
C
      ENTRY PutSig (ivec,Sigr,SigI)
      irec = ivec * 2 
      IF (REALAR) THEN
         Write (MRCVECS,REC=IREC) (SIGR(I),I=1,NDET)
      ELSE
         Write (MRCVECS,REC=IREC) (SIGR(I),SIGI(I),I=1,NDET)
      ENDIF
      Return
C
      END
