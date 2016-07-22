C        
      SUBROUTINE NSRepl (Irp,NS,OccNr,VecR,VecI,
     &                   RepName,EigenV,IRecSP,WORK)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C    ===========================================
C     Replaces Active Spinors by Natural Spinors
C    ===========================================
C
      INCLUDE 'param.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'general.inc'
C
      Dimension OccNr(NS),VecR(NS,NS),VecI(NS,NS),EigenV(NS)
      Dimension IRecSp(NS),WORK(*)
      Character Card*132,Format*20,RepName*14
C
      Do IS = 1, NS
	 IRecSp(IS) = 0
      EndDo
C     First Pass : Find the Active Spinors.
      Rewind (21)
      Read (21,'(A)') Card
      Read (21,'(A)') Card
      Read (21,'(A)') Card
      Read (21,'(A)') Card
      Read (21,'(A)') Card
      Read (Card,'(A)') Format
      Read (21,'(A)') Card
      Read (21,'(A)') Card
      Read (21,'(A)') Card
      Irec = 8
      IFound = - 1
   10 Read (21,'(A)',End=30) Card
      Irec = Irec + 1
      If (Card(6:10).EQ.'*****'.AND.Card(12:25).EQ.RepName) Then
         Read (Card(31:38),'(2I4)') NST,Nbas
	 IFound = 0
C
C        Allocate pointers to store two sets of NS vectors of Nbas length
C
         Ioff1 = 1
         Ioff2 = Ioff1 + Nbas * NS
	 Ioff3 = Ioff2 + Nbas * NS
	 Ioff4 = Ioff3 + Nbas * NS
      EndIf
      If (Card(2:3).EQ.'MO'.AND.IFound.GE.0) Then
         Read (Card,'(10X,F20.10)') EV
C        Check whether this is one of the active set.
C        If the representation conatins degenerate functions we make
C        sure that this one is not appointed to a previously found spinor.
         Do IS = 1, NS
            If (EV.EQ.EigenV(IS).AND.IrecSp(IS).EQ.0) Then
               IrecSp(IS) = Irec
	       IFound = Ifound + 1
	       GoTo 20
	     EndIf
          EndDo
   20     If (Ifound.EQ.NS) GoTo 30
       EndIf
       GoTo 10
C
   30 If (Ifound.NE.NS) Then
	 Print*, 'Cannot make natural spinors for repr. ',Repname
	 Print*, 'Make sure that original MFDVECA file is present'
	 Return
      Endif
C
C     Read in Canonical Spinors
C
      Do IS = 1, NS
         Rewind (21)
         Do IR = 1, IrecSp(IS)
            Read (21,'(A)') Card
         EndDo
	 Ioff1a = Ioff1 + (IS-1) * Nbas
	 Ioff2a = Ioff2 + (IS-1) * Nbas
         Read (21,Format) (Work(i+ioff1a-1),Work(i+ioff2a-1),i=1,nbas)
      EndDo
C
C     Make Natural Spinors
C
      Call ZZero (Nbas*Ns,Work(Ioff3))
      Call ZZero (Nbas*Ns,Work(Ioff4))
      Do IS = 1, NS
	 Ioff1a = Ioff1 + (IS-1) * Nbas
	 Ioff2a = Ioff2 + (IS-1) * Nbas
	 Do JS = 1, NS
	    Ioff3a = Ioff3 + (JS-1) * Nbas
	    Ioff4a = Ioff4 + (JS-1) * Nbas
	    Do I = 1, Nbas
	       Work(Ioff3a+i-1) = Work(Ioff3a+i-1)
     &                          + Work(Ioff1a+i-1) * VecR(IS,JS)
               IF (.NOT.REALAR) THEN
               Work(Ioff3a+i-1) = Work(Ioff3a+i-1) 
     &                          - Work(Ioff2a+i-1) * VecI(IS,JS)
	       Work(Ioff4a+i-1) = Work(Ioff4a+i-1)
     &                          + Work(Ioff1a+i-1) * VecI(IS,JS)
     &                          + Work(Ioff2a+i-1) * VecR(IS,JS)
               ENDIF
	     EndDo
          EndDo
      EndDo
C
C     Write the Natural Spinors.
C
      Do IS = 1, NS
         Rewind (21)
         Rewind (22)
         Do IR = 1, IrecSp(IS)-1
            Read (21,'(A)') Card
            Write (22,'(A)') Card
         EndDo
	 Read (21,'(A)') Card
	 Read (Card,'(4X,I2)') MO
	 Write(Card,1000) MO,IS,OccNr(IS),0.0
	 Write (22,'(A)') Card
	 Ioff3a = Ioff3 + (IS-1) * Nbas
	 Ioff4a = Ioff4 + (IS-1) * Nbas
         Write (22,Format) (Work(i+ioff3a-1),Work(i+ioff4a-1),i=1,nbas)
C
C        Skip just as many records on unit 21
C
         Read (21,Format) (Work(ioff1+i-1),i=1,2*nbas)
         Call CopyFF (21,22)
	 Rewind (21)
	 Rewind (22)
         Call CopyFF (22,21)
      EndDo
      Return
 1000 FORMAT(1X,'MO:',I2,' NS:',I2,18X,' OCC:',F15.10,' COPCOF:',
     +       G20.10)
      End
