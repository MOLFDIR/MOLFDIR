C
C     ==================
      SUBROUTINE MFDDRIV
C     this code controls the building of the Fock matrices.
C     both master and alsve enter this code and the distinction is
C     made here ! the main work is done by the master. the slaves
C     only communicate local/partial Fock matrices.
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'memory.inc'
      INCLUDE 'densfock.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
      INCLUDE 'mpif.h'
#else
      PARAMETER (mpirnk=0)
      PARAMETER (MPI_STATUS_SIZE=1)
#endif
C
      REAL*8 G (4 * N21)
      REAL*8 H (4 * N21)
      REAL*8 GO (4 * N21)
      EQUIVALENCE (G, FAAR)
      EQUIVALENCE (H, DAAR)
      EQUIVALENCE (GO, FOAAR)
      INTEGER IERR,ISTAT(MPI_STATUS_SIZE)
C
      IF(mpirnk.EQ.0) THEN

C  ****************************************************************
C  *********      M A S T E R    C O D E    ***********************
C  ****************************************************************

      CALL MFDPOEF (G(IP1), G(IP2), G(IP3), G(IP4), G(IP5), G(IP6),
     +              G(IP7), G(IP8), G(IP9), G(IP10), G(IP11))
      CALL MFDENSI
      CALL MFDENSN
      IF (ONLYBRT) THEN
         CALL MFDBREIT
      ELSE
         CALL MFDFOCK
      ENDIF
      IF (BREIT) CALL MFDBREIT
C
      CALL MFDFMCP (FAAR, FBAR, FAAI, FBAI, DAAR, DAAI)
      IF (OPENSH) THEN
        CALL MFDFMCP (FOAAR, FOBAR, FOAAI, FOBAI, DOAAR, DOAAI)
      ENDIF
C
      CALL MFDPROP (G(IPR1), G(IPR2), G(IPR3), G(IPR4), G(IPR5),
     +              G(IPR6), GO(IPR1), GO(IPR2), GO(IPR3), GO(IPR4),
     +              GO(IPR5), GO(IPR6), 1)
      IF (.NOT.ONLYBRT) THEN
         WRITE (6, 1000) ETOTAL
         WRITE(DAYFTXT,1010)  ETOTAL
      ELSE
         WRITE (6, 1001) ELREPC(1)
         WRITE (DAYFTXT, 1011) ELREPC(1)
      ENDIF
C
      CALL MFDTRAN (G(ID1), G(ID2), G(ID3), G(ID4), G(ID5), G(ID11),
     +              G(ID6), G(ID7), G(ID8), G(ID9), G(ID10),
     +              G(ID12), G(ID13), G(ID14), G(ID15))
C
 1000 FORMAT(//' THE CORE ENERGY = ',G29.14E4//)
 1001 FORMAT(//' THE GAUNT CONTRIBUTION TO THE CORE ENERGY=',G29.14E4//)
 1010 FORMAT('E(CORE):',G29.14E4)
 1011 FORMAT('E(CORE,GAUNT):',G29.14E4)

      ELSE

C  ****************************************************************
C  *********      S L A V E    C O D E      ***********************
C  ****************************************************************

C ** provide necessary parital fock/breit matrices to master

      IF (ONLYBRT) THEN
         CALL MFDBSLV
      ELSE
         CALL MFDFSLV
      ENDIF
      IF (BREIT) CALL MFDBSLV

C  ****************************************************************
C  *********      COMMON  SECTION  FROM  HERE    ******************
C  ****************************************************************

      ENDIF

      RETURN
      END
