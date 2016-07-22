C
      SUBROUTINE MAKEGM(GMATR,GMATI,TINTR,TINTI,IJEX)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ================================================
C     READS 1- AND 2-ELECTRON INTEGRALS, MAKES GMATRIX
C     ================================================
C
      INCLUDE 'param.inc'
      INCLUDE 'files.inc'
      INCLUDE 'general.inc'
      INCLUDE 'symm.inc'
C
      DIMENSION GMATR(*),GMATI(*),TINTR(*),TINTI(*),IJEX(*)
      LOGICAL ENDFIL
C
      COMPLEX*16 ONE,RPHASE(N3)
      CHARACTER*10 DATEX,TIMEX*8
C
      OPEN (MRCONEE,FILE=FNAM(1),FORM='UNFORMATTED')
      REWIND (MRCONEE)
      READ (MRCONEE) morb
      READ (MRCONEE)
      READ (MRCONEE)
      READ (MRCONEE)
      READ (MRCONEE)
C     -------------------------------------------------------
C     Read one-electron integrals : use tint as scratch space
C     -------------------------------------------------------
      IF (REALAR) THEN
         READ (MRCONEE) (tintr(JI),tdum,JI=1,morb*morb)
      ELSE
         READ (MRCONEE) (tintr(JI),tinti(JI),JI=1,morb*morb)
      ENDIF
      CLOSE (MRCONEE)
C
      OPEN(MDCINT,FILE=FNAM(2),FORM='UNFORMATTED')
      Read (mdcint,err=10000,end=10000) datex,timex,nkr
      CLOSE (MDCINT,STATUS='KEEP')
C
C     -------------------------------------------------------------
C     Reorder one-electron integrals (some orbitals may be deleted)
C     -------------------------------------------------------------
      jid = 0
      do ii = 1, morb
	 do jj = 1, morb
	    jid = jid + 1
	    i = indmo(ii)
	    j = indmo(jj)
	    If (i.NE.0.AND.j.NE.0) Then
	       ji = norb*(i-1)+j
	       gmatr(ji) = tintr(jid)
	       IF (.NOT.REALAR) gmati(ji) = tinti(jid)
            EndIf
         enddo
      enddo
C
      REWIND (MRCTWOE)
    1 CALL TWOLIN (ENDFIL,TINTR,TINTI,IJEX,KL,NKL)
      IF (ENDFIL) GOTO 201
C
C     -------------------------------------
C     ADD CONTRIBUTION TO G-MATRIX ELEMENTS
C     -------------------------------------
C
      K = (KL-1)/NORB + 1
      L = MOD(KL-1,NORB) + 1
      DO 200 IX = 1, NKL
      IJ = IJEX(IX)
      I = (IJ-1)/NORB + 1
      J = MOD(IJ-1,NORB) + 1
      IF (K.EQ.J) THEN
         IL = (I-1) * NORB + L
         GMATR(IL) = GMATR(IL) - TINTR(IX)
         IF (.NOT.REALAR) GMATI(IL) = GMATI(IL) - TINTI(IX)
      ENDIF
  200 CONTINUE
      GOTO 1
  201 CONTINUE
C
      RETURN
C
10000 STOP 'ERROR READING HEADER OF MDCINT'
      END
