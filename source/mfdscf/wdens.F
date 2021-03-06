C
      SUBROUTINE WDENS(MCYCL,A,L1,IRP,IOPT)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 A(*)
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
C
C === THIS ROUTINE WRITES THE DENSITY MATRIX TO FILE
C === BECAUSE OF THE LIMITTIONS ON RECORD LENGTH OF SOME
C === SILLY COMPUTERS WE WILL WRITE EACH SYMMETRY BLOCK OF
C === THE DENSITY MATRIX TO A SEPARATE RECORD.
C === IOPT = 1 : STORE CLOSED SHELL
C === IOPT > 1 : STORE OPEN SHELL
C
      IF (IOPT .EQ. 1) THEN
         IREC = (MCYCL - 1) * (NDCREC+NDOREC+NDTREC) + INIRP(IRP)
      ELSE 
	 IF (IOPT .EQ. 2) THEN
         IREC = (MCYCL - 1) * (NDCREC+NDOREC+NDTREC) + NDCREC + 
     +          INIRP(16 + IRP)
	 ELSE
	   IF (IOPT .LE. 3) THEN
	   IREC = (MCYCL-1)*(NDCREC+NDOREC+NDTREC)+NDCREC+NDOREC+
     +           INIRP(32+IRP)
           ELSE
            WRITE(6,*) ' WDENS : IOPT CAN ONLY RUN FROM 1 TO 3'
            STOP
           ENDIF
        ENDIF
      ENDIF
      WRITE(MFDENSM,REC=IREC) (A(I),I=1,L1)
      RETURN
      END
