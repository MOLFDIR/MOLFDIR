C
C...   Copyright (c) 1998 by the authors of Dirac (see below).
C...   All Rights Reserved.
C...
C...   The source code in this file is part of 
C...   "Dirac, a relativistic ab initio electronic structure program, 
C...   Release 3.1 (1998), written by T. Saue, T. Enevoldsen, T. Helgaker,
C...   H. J. Aa. Jensen, J. Laerdahl, K. Ruud, J. Thyssen, and L. Visscher"
C...
C...   This source code is provided under a written licence and may be 
C...   used, copied, transmitted, or stored only in accord with that
C...   written licence.
C...
C...   In particular, no part of the source code or compiled modules may 
C...   be distributed outside the research group of the licence holder. 
C...   This means also that persons (e.g. post-docs) leaving the research
C...   group of the licence holder may not take any part of Dirac,
C...   including modified files, with him/her, unless that person has
C...   obtained his/her own licence. 
C...
C...   For questions concerning this copyright write to:
C...      dirac-admin@dirac.chem.ou.dk
C...
C...   For information on how to get a licence see:
C...      http://dirac.chem.ou.dk/Dirac
C
C
C VERSION : $Revision: 1.3 $
C DATE    : $Date: 2001/09/14 20:45:54 $
C FILE    : ccxblas.F
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/ccxblas.F,v $
C
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE XAXPY (N,A,X,INCX,Y,INCY)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Generic version of BLAS routines SAXPY, DAXPY, CAXPY, ZAXPY
C     Depending of the value of CARITH and the machine specification
C     the appropriate routine is called
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER N,INCX,INCY
      REAL*8 A,X(*),Y(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IF (CARITH) THEN
#if defined (BIT64)
         CALL CAXPY (N,A,X,INCX,Y,INCY)
#else
         CALL ZAXPY (N,A,X,INCX,Y,INCY)
#endif
      ELSE
#if defined (BIT64)
         CALL SAXPY (N,A,X,INCX,Y,INCY)
#else
         CALL DAXPY (N,A,X,INCX,Y,INCY)
#endif
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE XCOPY (N,X,INCX,Y,INCY)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Generic version of BLAS routines SCOPY, DCOPY, CCOPY, ZCOPY
C     Depending of the value of CARITH and the machine specification
C     the appropriate routine is called
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER N,INCX,INCY
      REAL*8 X(*),Y(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IF (CARITH) THEN
#if defined (BIT64)
         CALL CCOPY (N,X,INCX,Y,INCY)
#else
         CALL ZCOPY (N,X,INCX,Y,INCY)
#endif
      ELSE
#if defined (BIT64)
         CALL SCOPY (N,X,INCX,Y,INCY)
#else
         CALL DCOPY (N,X,INCX,Y,INCY)
#endif
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE XGEMM (TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,
     &                  BETA,C,LDC)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Generic version of BLAS routines SGEMM, DGEMM, CGEMM, ZGEMM
C     Depending of the value of CARITH and the machine specification
C     the appropriate routine is called
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      CHARACTER*1 TRANSA,TRANSB
      INTEGER M,N,K,LDA,LDB,LDC
      REAL*8 ALPHA,BETA,A(*),B(*),C(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      CHARACTER*1 TRANSAL,TRANSBL
C
C---------------Executable code--------------------------------------
C
      IF (M.EQ.0.OR.N.EQ.0.OR.K.EQ.0) RETURN
C
      IF (CARITH) THEN
#if defined (BIT64)
         CALL CGEMM (TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,
#else
         CALL ZGEMM (TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,
#endif
     &               BETA,C,LDC)
      ELSE
         TRANSAL = TRANSA
         TRANSBL = TRANSB
         IF (TRANSA.EQ.'C') TRANSAL = 'T'
         IF (TRANSB.EQ.'C') TRANSBL = 'T'
         IF (TRANSA.EQ.'c') TRANSAL = 't'
         IF (TRANSB.EQ.'c') TRANSBL = 't'
#if defined (BIT64)
         CALL SGEMM (TRANSAL,TRANSBL,M,N,K,ALPHA,A,LDA,B,LDB,
#else
         CALL DGEMM (TRANSAL,TRANSBL,M,N,K,ALPHA,A,LDA,B,LDB,
#endif
     &               BETA,C,LDC)
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE XGEMV (TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Generic version of BLAS routines SGEMV, DGEMV, CGEMV, ZGEMV
C     Depending of the value of CARITH and the machine specification
C     the appropriate routine is called
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      CHARACTER*1 TRANS
      INTEGER M,N,LDA,INCX,INCY
      REAL*8 ALPHA,BETA,A(*),X(*),Y(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      CHARACTER*1 TRANSL
C
C---------------Executable code--------------------------------------
C
      IF (CARITH) THEN
#if defined (BIT64)
         CALL CGEMV (TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#else
         CALL ZGEMV (TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#endif
      ELSE
         TRANSL = TRANS
         IF (TRANS.EQ.'C') TRANSL = 'T'
         IF (TRANS.EQ.'c') TRANSL = 't'
#if defined (BIT64)
         CALL SGEMV (TRANSL,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#else
         CALL DGEMV (TRANSL,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#endif
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE XSCAL (N,A,X,INCX)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Generic version of BLAS routines SSCAL, DSCAL, CSCAL, ZSCAL
C     Depending of the value of CARITH and the machine specification
C     the appropriate routine is called
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER N,INCX
      REAL*8 A,X(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IF (CARITH) THEN
#if defined (BIT64)
         CALL CSCAL (N,A,X,INCX)
#else
         CALL ZSCAL (N,A,X,INCX)
#endif
      ELSE
#if defined (BIT64)
         CALL SSCAL (N,A,X,INCX)
#else
         CALL DSCAL (N,A,X,INCX)
#endif
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      REAL*8 FUNCTION XSUM (N,X,INCX)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Generic version of BLAS routines SSUM, DSUM, CSUM, ZSUM
C     Depending of the value of CARITH and the machine specification
C     the appropriate routine is called.
C     Note that the imaginary part of the result is always assumed to
C     be zero.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER N,INCX
      REAL*8 X(*)
      COMPLEX*16 YSUM,ZSUM,CSUM
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IF (CARITH) THEN
#if defined (BIT64)
         YSUM = CSUM (N,X,INCX)
         IF ( ABS(AIMAG(YSUM)).GT.ACCUR) THEN
            PRINT*, "IMAGINARY PART OF XSUM :",AIMAG(YSUM)
#else
         YSUM = ZSUM (N,X,INCX)
         IF (ABS(DIMAG(YSUM)).GT.ACCUR) THEN
            PRINT*, "IMAGINARY PART OF XSUM :",DIMAG(YSUM)
#endif
c           CALL QUIT("RESULT OF XSUM IS NOT REAL")
         ENDIF
#if defined (BIT64)
         XSUM = REAL(YSUM)
#else
         XSUM = DBLE(YSUM)
#endif
      ELSE
#if defined (BIT64)
         XSUM = SSUM (N,X,INCX)
#else
         XSUM = DSUM (N,X,INCX)
#endif
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#if defined (IBM)
      REAL*8 FUNCTION DSUM (N,X,INCX)
      REAL*8 X(*),SUM
      II = 1
      SUM = 0.D0
      DO I = 1, N
         SUM = SUM + X(II)
         II = II + INCX
      ENDDO
      DSUM = SUM
      RETURN
      END
#endif
C
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE XTRANSP (M,N,AR,BR,AC,BC)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Generic transpose routine.
C     Copies matrix A(M,N) into matrix B(N,M)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER M,N
      REAL*8 AR(M,N),BR(N,M)
      COMPLEX*16 AC(M,N),BC(N,M)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      INTEGER I,J
C
C---------------Executable code--------------------------------------
C
      IF (CARITH) THEN
         DO I = 1, M
            DO J = 1, N
               BC(J,I) = AC(I,J)
            ENDDO
         ENDDO
      ELSE
         DO I = 1, M
            DO J = 1, N
               BR(J,I) = AR(I,J)
            ENDDO
         ENDDO
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

