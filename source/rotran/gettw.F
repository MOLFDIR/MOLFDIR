      LOGICAL FUNCTION GETTW(IFILE,IUNTW,IUNLB,NINT,RINT,
     &                       LABEL1,LABEL2,NG)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      REAL*8 RINT(*)
      INTEGER LABEL1(*),LABEL2(*)
      LOGICAL FINISH
C
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
      READ(IUNTW) NT,IT,FINISH,(RINT(I),I=1,NT)
      IF (NT.GT.NG) STOP 'NT TOO BIG ON SRTTW'
C
C     Read SSLL file as LLSS to transform the SS indices first
C     Do other files the same way
C
#if defined (BIT64) 
      READ(IUNLB) NT,IT,FINISH,(LABEL1(I),I=1,NT)
      DO I = 1, NT
         LABEL1(I) = ISHIFT(LABEL1(I),32)
      ENDDO
#else
      READ(IUNLB) NT,IT,FINISH,(LABEL2(I),I=1,NT),
     +                         (LABEL1(I),I=1,NT)
#endif
C
      NINT=NT
C
      DO 10 I=1,NINT
        IF (IT.EQ.1) THEN
          RINT(I)=RINT(I)*0.125D0
        ELSEIF (IT.EQ.2) THEN
          RINT(I)=RINT(I)*0.5D0
        ELSEIF (IT.EQ.3) THEN
          RINT(I)=RINT(I)*0.25D0
        ELSEIF (IT.EQ.4) THEN
          RINT(I)=RINT(I)*0.5D0
        ELSEIF (IT.LE.8) THEN
          RINT(I)=RINT(I)*0.5D0
        ENDIF
 10     CONTINUE
C
C     ------------------------------------------
C     GAUNT INTEGRALS ARE TO BE MULTIPLIED BY -1
C     ------------------------------------------
C
      IF (IFILE.EQ.4) THEN
         DO 20 I=1,NINT
         RINT(I) = - RINT(I)
   20    CONTINUE
      ENDIF
      GETTW=FINISH
      RETURN
      END
