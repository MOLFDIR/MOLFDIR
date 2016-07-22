*colib9.f
*colib part=9 of 9.  obsolete routines
*version=4.1 last modified: 24-nov-90
c
c  RCS $Revision: 1.3 $  $Date: 2001/09/07 02:25:12 $
c
c  see colib1.f for version history info.
c
c  the routines in this file are obsolete.
c  calls to these routines should be removed from existing codes
c  and new codes should not make calls to these routines.
c
*deck zdummy
*mdc*if flag .and. (.not. flag)
*      subroutine zdummy
*c
*c     # dummy routine to keep this file from being empty.
*c
*c     # bummer error types.
*      integer   wrnerr,  nfterr,  faterr
*      parameter(wrnerr=0,nfterr=1,faterr=2)
*c
*c     # this routine should never be called.
*c
*      call bummer( 'zdummy:', 0, faterr )
*c
*      return
*      end
*mdc*endif
*deck rl
      integer function rl(i)
c
c     this function maps real word lengths on integer word lengths
c
c**********************************************************************
c  the use of this function is dangerous, due to machine-dependent data
c  alignment constraints, and should be eliminated in the calling
c  programs. -rls
c**********************************************************************
c
      integer i
c
      integer first
      save    first
      data first / 1 /
c
      if ( first .eq. 1 ) then
c        # print an annoying message the first time through. -rls
         call bummer(' rl(): obsolete function called.'
     &    //' the calling program should be updated', 0, 0)
         first = 0
      endif
c
*mdc*if int64 cray fps164
*      rl= i
*mdc*else
c     # assuming i*4 declarations
      rl=2*i
*mdc*endif
      return
      end
