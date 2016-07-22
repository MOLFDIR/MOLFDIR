      LOGICAL FUNCTION GETSKEL(BREIT,NKR,NSP,I,J,RL,RJ,NRJ,INDRJ,LBRCSK)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "files.inc"
      INCLUDE "vecs.inc"
C
      INTEGER IJ,I,J,NKR,NSP,NRJ
      LOGICAL CONJUG,BREIT
      COMPLEX*16 RJ(*),RL(*)
      INTEGER INDRJ(NRJ),LBRCSK(0:*)
C
      CALL CZERO (NSP*NSP,RL)
      NINT=0
C     Find out to which reduced set this corresponds
      IKR = IKRMO(I)
      JKR = IKRMO(J)
C     Lower triangle in ij is stored : upper triangle related by herm. comjug.
      IF (ABS(IKRMO(I)).GE.ABS(IKRMO(J))) THEN
         IIKR = IKR
         JJKR = JKR
         CONJUG = .FALSE.
      ELSE
         IIKR = JKR
         JJKR = IKR
         CONJUG = .TRUE.
      ENDIF
C     (ibar,j) and (ibar,jbar) are stored.
      ISTRIJ = 1
      IF (IIKR.GT.0) THEN
         ISTRIJ = ISIGN(1,IIKR*JJKR)
         IF (BREIT) ISTRIJ = - ISTRIJ
         IIKR = - IIKR
         JJKR = - JJKR
         CONJUG = .NOT.CONJUG
      ENDIF
C     Calculate record index
      IJBAROFF = NKR*(NKR+1)/2
      IF (JJKR.GT.0) THEN
         IJ=(JJKR-1)*(2*NKR-JJKR)/2-IIKR
      ELSE
         IJ=IJBAROFF-(JJKR+1)*(2*NKR+JJKR)/2-IIKR
      ENDIF
C     write(*,*) 'serial getskel ij:',ij
      IREC=LBRCSK(IJ)
C
 10   IF (IREC.EQ.0) GOTO 20
        READ(INTSKEL,REC=IREC) NT,IREC,(RJ(IT),IT=1,NT),
     +                                 (INDRJ(IT),IT=1,NT)
C
      DO IT = 1, NT
         RL(INDRJ(IT)) = RL(INDRJ(IT)) + RJ(IT)*ISTRIJ
C        write(*,*) it,indrj(it),istrij,RJ(IT),rl(indrj(it))
      ENDDO
      NINT = NINT + NT
      GOTO 10
C
 20   IF (NINT.EQ.0) THEN
        GETSKEL=.FALSE.
      ELSE
        IF (CONJUG) CALL CJMAT(NSP,RL,NSP)
        CALL PHASETR (.TRUE.,NSP,I,J,RL,NSP)
        GETSKEL=.TRUE.
      ENDIF
C
C     write(*,'(A,I4,A,I5,a)') 'for ij pair',ij,
C    +     ' master received',nint,' integrals'

      RETURN
      END
