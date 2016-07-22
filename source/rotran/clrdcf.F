      SUBROUTINE CLRDCF(LRECCO,IRDCF)
C 
C *************************************************************** 
C *** 
C *** PURPOSE:  CLEAR THE RECORD DATA CONTROL ARRAY. TO BE DONE
c ***           ONLY ONCE !
C ***
C ***************************************************************
C ***        I/O VARIABLES:
C ***
C *** LRECCO : NUMBER OF ENTRIES TO BE CLEARED
C *** IRDCF:   THE ACTUAL ARRAY
C ***
C ***************************************************************

      IMPLICIT REAL*8 (A-H, O-Z)
 
      INTEGER LRECCO,IRDCF(*)

      DO I=1,LRECCO
        IRDCF(I)=0
      ENDDO
     
      RETURN
      END
