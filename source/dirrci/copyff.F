C        
      SUBROUTINE CopyFF (IUN1,IUN2)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     =====================
C     Copies File1 to File2
C     =====================
C
      Character*132 Card
C
   10 READ (IUN1,'(A)',END=20,ERR=10001) CARD
      WRITE (IUN2,'(A)') CARD
      IF (CARD(1:3).EQ.'END') GOTO 20
      GOTO 10
   20 CONTINUE
      RETURN
10001 Stop 'Cannot read Unit'
      END
