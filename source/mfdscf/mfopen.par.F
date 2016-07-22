C     =================
      SUBROUTINE MFOPEN
C     =================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'memory.inc'
      INCLUDE 'mpif.h'
      INCLUDE 'mpidef.inc'
C
      LOGICAL DATOBE,LNTOBE
      CHARACTER*7 FNAME1,FNAME2
      CHARACTER*11 FILNM1, FILNM2
      INTEGER YINTS(2)
C
      YINTS(1) = 1
      YINTS(2) = 1
C
C Check if the direct-access file is existent
C
      FNAME1=FNAM(10)
      FNAME2=FNAM(11)
      IF (mpirnk  .EQ. 0) THEN
         WRITE (FILNM1,'(A7)') FNAME1
         WRITE (FILNM2,'(A7)') FNAME2
      ELSEIF (mpirnk  .LT. 10) THEN
         WRITE (FILNM1,'(A7,A1,I1)') FNAME1,'.',mpirnk
         WRITE (FILNM2,'(A7,A1,I1)') FNAME2,'.',mpirnk
      ELSEIF (mpirnk  .LT. 100) THEN
         WRITE (FILNM1,'(A7,A1,I2)') FNAME1,'.',mpirnk
         WRITE (FILNM2,'(A7,A1,I2)') FNAME2,'.',mpirnk
      ELSE
         WRITE (FILNM1,'(A7,A1,I3)') FNAME1,'.',mpirnk
         WRITE (FILNM2,'(A7,A1,I3)') FNAME2,'.',mpirnk
      ENDIF
      INQUIRE (FILE = FILNM1, EXIST = DATOBE)
      INQUIRE (FILE = FILNM2, EXIST = LNTOBE)
      IF (.NOT.DATOBE) THEN
        PRINT*,' ** DIRECT-ACCESS FILE IS NOT EXISTENT **'
        YINTS(1) = 0
      ENDIF
      IF (.NOT.LNTOBE) THEN
        PRINT*,' ** DA CONTROL FILE IS NOT EXISTENT **'
        YINTS(2) = 0
      ENDIF
C
C Report back to master about integral situation
C
      CALL MPI_SEND(YINTS,2,MPI_INTEGER,0,17,
     &              MPI_COMM_WORLD,IERR)
C
      RETURN
C
      END
