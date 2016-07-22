      SUBROUTINE INITTWOE (IPRNT,BREIT,NKR,NSP,MDINT,WORK)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "files.inc"
      INCLUDE "vecs.inc"
C
      LOGICAL BREIT
      CHARACTER*8 TIMEX,DATEX*10
      INTEGER NKR,NSP,MDINT,IPRNT
C 
      CALL DAYTIME(DATEX,TIMEX)
C
      if (iprnt.ge.2) write (6,'(/A)') ' Completed CI integrals :'
      If (.NOT.Breit) Then
         open (mdcint,file=fname(14),FORM='UNFORMATTED')
         mdint = mdcint
      Else
         open (mdbint,file=fname(15),FORM='UNFORMATTED')
         mdint = mdbint
      Endif
      write (mdint) datex,timex,nkr,
     &               (imoras(kra(i,3),kra(i,2)),
     &                imoras(krb(i,3),krb(i,2)),i=1,nkr)
      return
      end
