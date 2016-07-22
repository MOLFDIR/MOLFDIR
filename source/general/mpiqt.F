C     ============================
      SUBROUTINE MPIQT(STRING,LEN)
C     ============================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     THIS ROUTINE (MPI QUIT) FINISHES THE CALLING PROGRAM AFTER
C     AN MPI ERROR. THE ARGUMENT GIVEN IS THE ERROR MESSAGE AND ITS
C     LENGTH IS IN LEN.
C
      CHARACTER STRING*(*)
      INTEGER LEN
      
      write(*,*) ' '
      write(*,*) ' ** MPI ERROR ! PROGRAM ABORTED **'
      write(*,*) ' ** LAST MPI CALL: ',STRING
      write(*,*) ' '
C     call MPI_FINALIZE(LEN)
      STOP 'ABNORMAL END OF PROGRAM.'
      END

