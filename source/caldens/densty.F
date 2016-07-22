C
C     =================
      SUBROUTINE DENSTY
C     =================
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE "param.inc"
      INCLUDE "comblock.inc"
C
      DIMENSION AA (3), DA (3)
      DIMENSION X(MXPOINTS),Y(MXPOINTS),Z(MXPOINTS)
      DIMENSION DENSL(MXPOINTS),DENSS(MXPOINTS),DENST(MXPOINTS)
C
      IF (NA(3)*NA(2)*NA(1).GT.MXPOINTS) THEN
         WRITE(6,1000) MXPOINTS,NA,NA(1)*NA(2)*NA(3)
         STOP "NUMBER OF GRID POINTS TOO LARGE"
      ENDIF
C
C     -----------------------------------
C     SET UP GRID : CALCULATE COORDINATES
C     -----------------------------------
C
      DO 5 I = 1, 3
      DA (I) = 0.0D0
      AA (I) = 0.0D0
 5    CONTINUE
C
      DO 10 IDIM = 1, NDIM
#if defined (BIT64)
      DA (IDIM) = (AL (IDIM) - AF (IDIM)) / REAL (NA (IDIM)-1)
#else
      DA (IDIM) = (AL (IDIM) - AF (IDIM)) / DBLE (NA (IDIM)-1)
#endif
      AA (IDIM) = AF (IDIM) - DA (IDIM)
 10   CONTINUE
C
      ICOUNT = 0
C
      DO 20 IA3 = 1, NA (3)
      AA (3) = AA (3) + DA (3)
      DO 30 IA2 = 1, NA (2)
      AA (2) = AA (2) + DA (2)
      DO 40 IA1 = 1, NA (1)
      AA (1) = AA (1) + DA (1)
C
      ICOUNT = ICOUNT + 1
      X (ICOUNT) = X0
      Y (ICOUNT) = Y0
      Z (ICOUNT) = Z0
      DO 50 IDIM = 1, NDIM
      X (ICOUNT) = X (ICOUNT) + AA (IDIM) * RX (IDIM)
      Y (ICOUNT) = Y (ICOUNT) + AA (IDIM) * RY (IDIM)
      Z (ICOUNT) = Z (ICOUNT) + AA (IDIM) * RZ (IDIM)
 50   CONTINUE
C
 40   CONTINUE
      AA(1)=AF(1)-DA(1)
 30   CONTINUE
      AA(2)=AF(2)-DA(2)
 20   CONTINUE
C
C
C     -------------------------------------------
C     LOOP OVER GRID (CALCULATE NPOINTS TOGETHER)
C     -------------------------------------------
C
      NCOUNT = ICOUNT
      NBATCH = NCOUNT / NPOINTS
      IF (NBATCH*NPOINTS.LE.NCOUNT) NBATCH = NBATCH + 1
C
CMIC$ DO ALL SHARED(NBATCH,NPOINTS,NCOUNT,X,Y,Z,DENSL,DENSS,DENST)
CMIC$1PRIVATE(IBATCH,IPOINTS,LPOINTS,I)
      DO IBATCH = 1, NBATCH
         IPOINTS = (IBATCH-1) * NPOINTS + 1
         LPOINTS = NPOINTS
         IF (IPOINTS+LPOINTS-1.GT.NCOUNT) LPOINTS = NCOUNT-IPOINTS+1
         CALL CALPART (NPOINTS,X(IPOINTS),Y(IPOINTS),Z(IPOINTS),
     &                 DENSL(IPOINTS),DENSS(IPOINTS),DENST(IPOINTS))
      ENDDO
C
      IF (LOGSCL) THEN
CMIC$ DO ALL SHARED(NCOUNT,DENSL,DENSS,DENST)
CMIC$1PRIVATE(I)
         DO I = 1, NCOUNT
            IF (DENST(I).LT.1.E-15) DENST(I) = 1.E-15
            IF (DENSL(I).LT.1.E-15) DENSL(I) = 1.E-15
            IF (DENSS(I).LT.1.E-15) DENSS(I) = 1.E-15
            DENST(I) = LOG10(DENST(I))
            DENSL(I) = LOG10(DENSL(I))
            DENSS(I) = LOG10(DENSS(I))
         ENDDO
      ENDIF
C
      IF (FORMOUT) THEN
      WRITE(6,1010)
      DO 80 IP = 1, NCOUNT
      WRITE (6, 1020) X(IP),Y(IP),Z(IP),DENST(IP),DENSL(IP),DENSS(IP)
 80   CONTINUE
      ELSE
      OPEN (MFDAVS,FORM='FORMATTED',ACCESS='SEQUENTIAL',FILE='AVS.fld')
      WRITE (MFDAVS,'(a)') '# AVS field'
      WRITE (MFDAVS,'(a)') 'veclen=6'
      WRITE (MFDAVS,'(a,i1)') 'ndim= ',ndim
      WRITE (MFDAVS,'(a,i1)') 'nspace= ',ndim
      WRITE (MFDAVS,'(a)') 'data=float'
      WRITE (MFDAVS,'(a)') 'field=uniform'
      WRITE (MFDAVS,'(a,i6)') 'dim1=',NA(1)
      IF (NDIM.GE.2) WRITE (MFDAVS,'(a,i6)') 'dim2=',NA(2)
      IF (NDIM.GE.3) WRITE (MFDAVS,'(a,i6)') 'dim3=',NA(3)
      WRITE (MFDAVS,*) 'variable 1 file=DENST.data filetype=ASCII'
      WRITE (MFDAVS,*) 'variable 2 file=DENSL.data filetype=ASCII'
      WRITE (MFDAVS,*) 'variable 3 file=DENSS.data filetype=ASCII'
      WRITE (MFDAVS,*) 'variable 4 file=GRIDX.data filetype=ASCII'
      WRITE (MFDAVS,*) 'variable 5 file=GRIDY.data filetype=ASCII'
      WRITE (MFDAVS,*) 'variable 6 file=GRIDZ.data filetype=ASCII'
      CLOSE (MFDAVS,STATUS='KEEP')
C
      OPEN (MFDAVS,FORM='FORMATTED',FILE='GRIDX.data')
      DO I = 1, NCOUNT
      WRITE (MFDAVS,1040) X(I)
      ENDDO
      CLOSE (MFDAVS,STATUS='KEEP')
C
      OPEN (MFDAVS,FORM='FORMATTED',FILE='GRIDY.data')
      DO I = 1, NCOUNT
      WRITE (MFDAVS,1040) Y(I)
      ENDDO
      CLOSE (MFDAVS,STATUS='KEEP')
C
      OPEN (MFDAVS,FORM='FORMATTED',FILE='GRIDZ.data')
      DO I = 1, NCOUNT
      WRITE (MFDAVS,1040) Z(I)
      ENDDO
      CLOSE (MFDAVS,STATUS='KEEP')
C
      OPEN (MFDAVS,FORM='FORMATTED',FILE='DENST.data')
      DO I = 1, NCOUNT
      WRITE (MFDAVS,1040) DENST(I)
      ENDDO
      CLOSE (MFDAVS,STATUS='KEEP')
C
      OPEN (MFDAVS,FORM='FORMATTED',FILE='DENSL.data')
      DO I = 1, NCOUNT
      WRITE (MFDAVS,1040) DENSL(I)
      ENDDO
      CLOSE (MFDAVS,STATUS='KEEP')
C
      OPEN (MFDAVS,FORM='FORMATTED',FILE='DENSS.data')
      DO I = 1, NCOUNT
      WRITE (MFDAVS,1040) DENSS(I)
      ENDDO
      CLOSE (MFDAVS,STATUS='KEEP')
C
      ENDIF
C
      RETURN
 1000 FORMAT (//" Maximum number of gridpoints (mxpoints) is ",I10/
     &" You asked ",I4,1X,' * ',I4,1X,' * ',I4,1X,' = ',I12)
 1010 FORMAT (//T48,'TOTAL DENSITY',T65,'LARGE COMPONENT',
     &T87,'SMALL COMPONENT'/)
 1020 FORMAT (3(2X,F12.8),3(2X,E20.10))
 1040 FORMAT (E13.7)
      END
