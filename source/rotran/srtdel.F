C
C     =================
      SUBROUTINE SRTDEL
C     =================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "files.inc"
      INCLUDE "inpt.inc"
C
      IFILMN=1
      IFILMX=3
      IF (BREIT) IFILMN=4
      IF (BREIT) IFILMX=4
      IF (TWOC)  IFILMX=1
      DO IFILE=IFILMN,IFILMX
          OPEN (SRTTW(IFILE),FILE=FNAME(IFILE+1),FORM='UNFORMATTED')
          OPEN (SRTLB(IFILE),FILE=FNAME(IFILE+5),FORM='UNFORMATTED')
          CLOSE (SRTTW(IFILE),STATUS='DELETE')
          CLOSE (SRTLB(IFILE),STATUS='DELETE')
      ENDDO
C
      RETURN
      END
