C
C     ==============================================================
      SUBROUTINE UDPU (NHERP,NSP,N2,PROPAO,UA,UB,WORKA,WORKB,PROPMO)
C     Transform diagonal property operator
C     ==============================================================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
#if defined (BIT64)
      PARAMETER (A0=(0.0,0.0),A1=(1.0,0.0))
#else
      PARAMETER (A0=(0.D0,0.D0),A1=(1.D0,0.D0))
#endif
      REAL*8 PROPAO(NHERP,NHERP)
      COMPLEX*16 PROPMO(NSP,NSP),WORKA(NHERP,NSP),WORKB(NHERP,NSP)
      COMPLEX*16 UA(N2,NSP),UB(N2,NSP)
C
      DO J = 1, NSP
         DO I = 1, NHERP
            WORKA(I,J) = A0
            WORKB(I,J) = A0
         ENDDO
      ENDDO
C
      DO JMO = 1, NSP
         DO K = 1, NHERP
            DO I = 1, NHERP
            WORKA(I,JMO)=WORKA(I,JMO)+PROPAO(I,K)*UA(K,JMO)
            WORKB(I,JMO)=WORKB(I,JMO)+PROPAO(I,K)*UB(K,JMO)
            ENDDO
         ENDDO
      ENDDO
C     print*,'summing half tranformed k prop(1,k) ub(k) prop(1,k)*ub(k)'
C     DO K=1,NHERP
C           print*,k,propao(1,k),ub(k,1),propao(1,k)*ub(k,1)
C     ENDDO
C     print*,'mo 1 half transformed a en b'
C     DO I=1,NHERP
C        print*,worka(I,1),workb(I,1)
C     ENDDO
C
      DO J = 1, NSP
         DO I = 1, NSP
            PROPMO(I,J) = A0
         ENDDO
      ENDDO
C
      DO JMO = 1, NSP
         DO K = 1, NHERP
            DO IMO = 1, NSP
               PROPMO(IMO,JMO) = PROPMO(IMO,JMO) 
#if defined (BIT64)
     &                         + WORKA(K,JMO) * CONJG(UA(K,IMO))
     &                         + WORKB(K,JMO) * CONJG(UB(K,IMO))
#else
     &                         + WORKA(K,JMO) * DCONJG(UA(K,IMO))
     &                         + WORKB(K,JMO) * DCONJG(UB(K,IMO))
#endif
            ENDDO
         ENDDO
      ENDDO
C     prop11=0.0D0
C     WRITE(*,*) '   K     Alpha term          Beta term          
C    &Sum alpha+beta      Property int'
C     DO K=1,NHERP
C        p1=WORKA(K,1) * DCONJG(UA(K,1))
C        p2=WORKB(K,1) * DCONJG(UB(K,1))
C        prop11=prop11+p1+p2
C        WRITE(*,1111) k,p1,p2,p1+p2,prop11
1111     FORMAT(I4,2X,2X,F18.10,2X,F18.10,2X,F18.10,2X,F18.10)
C     ENDDO
C
c--> The SGI ZGEMM routine appears to be buggy, switched off
cC32B  CALL ZGEMM ('C','N',NSP,NSP,NHERP,A1,UA,N2,WORKA,NHERP,
cC64B  CALL CGEMM ('C','N',NSP,NSP,NHERP,A1,UA,N2,WORKA,NHERP,
c     &            A0,PROPMO,NSP)
cC32B  CALL ZGEMM ('C','N',NSP,NSP,NHERP,A1,UB,N2,WORKB,NHERP,
cC64B  CALL CGEMM ('C','N',NSP,NSP,NHERP,A1,UB,N2,WORKB,NHERP,
c     &            A1,PROPMO,NSP)
C
      RETURN
      END
