      SUBROUTINE NATORB(IROOT,DENSR,DENSI,WORK1,WORK2,WORK3)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     =====================================
C     CALCULATES NATURAL ORBITALS (SPINORS)
C     =====================================
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'symm.inc'
C
      DIMENSION DENSR(NORB,NORB),DENSI(NORB,NORB)
      DIMENSION WORK1(*),WORK2(*),WORK3(*)
      Dimension NSinRep(16),Ndeg(16)
      Character*8 FilNNS,FilVCA,FilNSc
C
      Write (6,1000)
      FilVCA = 'MFDVECA'
      FilNSc = 'SCRATCH'
      Open (20,File=FilVCA,Form='Formatted',STATUS='OLD',ERR=110)
      Open (22,File=FilNSc,Form='Formatted')
      MaxSinRep = 0
      Do I = 1, 16
	 NSinRep(I) = 0
	 Ndeg(I) = 0
      EndDo
      Do I = 1,16
        DO m=1,16
          IF (RepN(I)(1:4).EQ.RepN(m)(1:4)) Ndeg(I)=Ndeg(I)+1
	ENDDO
      ENDDO
      Do I = 1, Norb
	 NSinRep(IrpMO(I)) = NSinRep(IrpMO(I)) + 1
      EndDo
      Do I = 1, 16
	 If (MaxSinRep.LT.NSinRep(I)) MaxSinRep = NSinRep(I)
      EndDo
C
      Ioff1 = 1			     
      Ioff5 = Ioff1 + MaxSinRep
      Ioff2 = Ioff5 + MaxSinRep
      Ioff3 = Ioff2 + MaxSinRep
      Ioff4 = Ioff3 + MaxSinRep
      IF (REALAR) THEN
         ISize = Ioff4 + MaxSinRep**2
      ELSE
         ISize = Ioff4 + MaxSinRep
      ENDIF
      IF (ISize.GT.2*NORBD) THEN
         WRITE(6,10000) 
         Close (22,Status='Delete')
         RETURN
      ENDIF
      Ioff6 = Ioff5 + MaxSinRep   	
      Ioff7 = Ioff6 + MaxSinRep
      ISize = Ioff7 + 4*MaxSinRep**2
      IF (ISize.GT.2*NORBD) THEN
         WRITE(6,10000) 
         Close (22,Status='Delete')
         RETURN
      ENDIF
C
      Write (FilNNS,'(A6,I2)') 'MFDNAT',IROOT
      IF (Iroot.lt.10) FilNNS(7:7) = '_'
      Rewind (20)
      Open (21,File=FilNNS,Form='Formatted')
      Call CopyFF (20,21)
      Do 100 Irp = 1, NSymrp
         NS = NSinRep(Irp)
C        Skip degenerate representations.
	 If (Irp.GT.1) Then
	    If (RepN(Irp)(1:4).EQ.RepN(Irp-1)(1:4)) Goto 100
	 EndIf
C        Skip representations without active spinors
	 If (NSinRep(Irp).eq.0) GoTo 100
C        Average with degenerate representations 
	 Do nd=0,Ndeg(Irp)-1
	    k = 0
            iorb = 0
            Do i =1, Norb
               If (Irpmo(i).EQ.Irp+nd) Then
	         Work3(Ioff5+iorb) = Eps(i)
	         iorb = iorb + 1
	         jorb = 0 
	         Do j = 1, Norb
	           If (IrpMO(j).EQ.Irp+nd) Then
		     If (nd.EQ.0) Then
		       Work1(k+1) = 0.0D0
		       IF (.NOT.REALAR) Work2(k+1) = 0.0D0
                     Endif
	             jorb = jorb + 1
C----------------------------------------------------
C Use - (Density matrix) to get eigenvalues in right
C order from the diagonalisation routine
C----------------------------------------------------
	             Work1(k+1) = Work1(k+1) - Densr(j,i)*
     &                            (1.0D0/Ndeg(Irp))
                     IF (.NOT.REALAR) Work2(k+1) = Work2(k+1) -
     &                                Densi(j,i)*(1.0D0/Ndeg(Irp))
	             k = k + 1
                   EndIf
	         EndDo
	       EndIf
            EndDo
         Enddo
         IFAIL=0
         IF (REALAR) THEN
            CALL RS (NS,NS,Work1,Work3(Ioff1),1,Work3(Ioff4),
     &               Work3(Ioff2),Work3(Ioff3),IFAIL)
            DO I = 1, NS*NS
               Work1(I) = Work3(Ioff4+I-1)
            ENDDO
         ELSE
            CALL F01BCF(NS,TOL,Work1,NS,Work2,NS, Work3(Ioff1),
     &                  Work3(Ioff2),Work3(Ioff3),Work3(Ioff4))
            CALL F02AYF(NS,EPSI,Work3(Ioff1),Work3(Ioff2),
     &                  Work1,NS,Work2,NS,IFail)
         ENDIF
         IF (IFAIL.NE.0) STOP'**ERROR-CANNOT DIAGONALISE CMTR**'
	 Do IEIGEN = 1, NS
	    Work3(Ioff1+IEigen-1) = - Work3(Ioff1+IEigen-1)
	 EndDo
	 Write (6,1010) Iroot, RepN(Irp)
	 Write (6,1020) (Work3(Ioff1+IEigen-1),IEigen=1,NS)
	 Call NSRepl (Irp,NS,Work3(Ioff1),Work1,Work2,RepN(Irp),
     &                Work3(Ioff5),Work3(Ioff6),Work3(Ioff7))
  100 Continue 
      Close (21)
      Write (6,1030) Iroot, FilNNS
      Close (22,Status='Delete')
  110 Continue
      RETURN
 1000 Format (//' Diagonalising (active part) of Density Matrix')
 1010 Format (/' Eigenvalues Density Matrix (Root',I3,', Repr. ',
     &         A14,')')
 1020 Format (6F12.8)
 1030 Format (/' Natural Spinors for Root',I3,' written to ',A8)
10000 Format (/' Cannot make the natural spinors due to ',
     &         'lack of memory')
      END
