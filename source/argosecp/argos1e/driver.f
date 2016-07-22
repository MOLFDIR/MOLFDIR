c
c  RCS $Revision: 1.2 $  $Date: 2001/09/04 18:39:13 $
c
*deck driver
c
c*******************************************************************
c
c   this computer program contains work performed partially by the
c   argonne national laboratory theoretical chemistry group under
c   the auspices of the office of basic energy sciences,
c   division of chemical sciences, u.s. department of energy,
c   under contract w-31-109-eng-38.
c
c   these programs may not be (re)distributed without the
c   written consent of the argonne theoretical chemistry group.
c
c   since these programs are under development, correct results
c   are not guaranteed.
c
c*******************************************************************
c
      subroutine driver( a, lcore, mem1, ifirst )
c
c            argonne - ohio state symmetry-adapted,
c       general-contraction integral program.  argos(i60439)
c  this program calculates one- and two-electron integrals over
c  symmetry orbitals of generally contracted gaussian orbitals and
c  forms the symmetry orbital integrals ordered by pk shell-blocks.
c  the point groups are limited to d2h and its subgroups.
c           by russell m. pitzer
c
c  version log:
c  16-aug-97 recur1 replaced by rad1. -rmp
c  21-may-97 many arrays allocated. -rmp
c            limits on quantum numbers removed. -rmp
c            tables replaced by code to generate them. -rmp/zyz
c  17-sep-96 ctss removed.
c  24-apr-92 rs6000 blocks added. -rls
c  11-sep-91 l2opxv() added. final parallel code added. -rls/rjh
c  01-sep-91 array declarations changed to workaround the ibm+convex
c            f77 equivalence bug. -rls/m. schueller/j. bentley
c  07-jul-91 preliminary parallel version using tcgmsg. -rjh
c  04-feb-91 npru changes in socfpd(). other cleanup. -rmp
c  07-dec-90 il(*) allocated from a(*) workspace. array(*,*) added
c            to oneint(). -rls
c  04-dec-90 title(1:5), only1e, and ndp<0 input options added. -rls
c  01-dec-90 SIFS output file written. -rls
c  28-nov-90 rad2() replaced. -rmp/rls
c  12-feb-90 stellar adaptation (rmp).
c  07-jan-90 change aoso2e, mpru test, eta 2-dim. (mn, rmp).
c  09-jul-89 mdc changes, miscellaneous changes (rmp).
c  07-mar-89 mdc changes, port to anl sun, alliant, titan (rls).
c  21-oct-88 dimension error of nprir and ica in seg1mn fixed (eas).
c  19-sep-88 sun version of code added (eas).
c  02-aug-88 rmp changes, mdc references, bummer calls added (rls).
c  05-jul-88 anl unicos version (rls).
c  09-may-88 timer() updated, bummer() included (rmp/rls).
c  20-apr-88 pseud2() and pseud3() modified (rmp/rls).
c  01-feb-88 cray-ctss and fps memory allocation added (rls).
c  17-jan-88 rest of rmp's corrections--common blocks (dcc)
c  17-dec-87 mblu defined in main, rmp's corrections (dcc,rmp)
c  12-oct-87 report max mblu, (*) arrays, cray pack (dcc)
c  21-sep-87 working under unicos (dcc)
c  15-sep-87 cmdc for osuibm and unicos (dcc)
c  11-aug-87 pkflag() and w*() routines added for integral io (rls).
c  07-aug-87 cmdc version (vax, fps, crayanl) from osu version (rls).
c  16-jul-86 dimension parameters added to main and seg1mn (rls/rmp).
c  02-apr-86 iperm(*,*) added to twoint (rls).
c  25-sep-84 a0 initialized in twoint and qpasy (rls/rab).
c  07-may-84 space allocation changes in socpfd (rls/rmp).
c  31-oct-83 fps-164 version changes (rls).
c  28-oct-83 tape read error corrected in routine recur (rls).
c
c  cmdc info:
c  keyword    description
c  -------   -------------
c  vax       vax code.
c  fps       fps code.
c  cray      cray code. includes "d" exponents in constants.
c  ibm       ibm (mainframe) code.
c  sun       sun code.
c  stellar   stellar specific code.
c  craycos   cos specific code.
c  unicos    cray unicos specific code.
c  mvsosu    uses osu local mvs routines.
c  crayanl   cray anl index packing code.
c  crayosu   cray osu index packing code.
c  ibmosu    ibm osu index packing code.
c  sunosu    sun osu index packing code.
c  vaxanl    vax anl index packing code.
c  vaxosu    vax osu index packing code.
c  crayxmp   cray xmp specific code.
c  parallel  tcgmsg parallel code.
c
c    *** things to do in this program ***
c  * eliminate hollerith strings in format statements.
c  * replace real constants with parameters.
c  * simplify the memory allocation method.
c  * improve vectorization (including so transformation).
c  * eliminate unnecessary statement labels (e.g. use if-then-else
c    blocks) to improve scalar and vector optimization.
c
      implicit logical(a-z)
c
c     # the following parameters should agree exactly with those in the
c     # seg1mn() routine.  (rls)
c
c     # msu    = max number of symmetry inequivalent types of atoms.
      integer    msup
      parameter (msup=203)
c
c     # mstu   = max number of irreps.
      integer    mstup
      parameter (mstup=8)
c
c     # mrcru  = max number of members in a contraction set.
      integer    mrcrup
      parameter (mrcrup=8)
c
c     # mconu  = max number of primitives in a contraction.
      integer    mconup
      parameter (mconup=24)
c
c     # mcu    = max number of symmetry equivalent atoms.
      integer    mcup
      parameter (mcup=12)
c
c     # mconsu = max number of contraction sets.
      integer    mconsp
      parameter (mconsp=50)
c
c     # msfu   = max number of function sets.
      integer    msfup
      parameter (msfup=52)
c
      integer    mnsfup
      parameter (mnsfup=(msfup*(msfup+1))/2)
c
c     # mgu    = max number of operators in the
c     #          nuclear interchange group.
      integer    mgup
      parameter (mgup=49)
c
c     # mccu   = max number of symmetry unique center
c     #          combinations from 4 nuclei.
      integer    mccup
      parameter (mccup=182)
c
c     # mblu   = workspace array length.
c     #          (allocated in the main program)
c
      integer   nunits
      parameter(nunits=4)
      integer        iunits
      common /units/ iunits(nunits)
      integer                  nlist
      equivalence ( iunits(1), nlist )
      integer                  aoints
      equivalence ( iunits(2), aoints )
      integer                  aoint2
      equivalence ( iunits(3), aoint2 )
      integer                  ninput
      equivalence ( iunits(4), ninput )
c
c     # filenames.
      character*60    fnames
      common /cfname/ fnames(nunits)
c
      integer         only1e
      common /conly1/ only1e
c
c     # /bufout/ holds some output integral file parameters.
      integer
     & info,     ifmt1,   ifmt2,   itypea,  itypeb,
     & ibuf,     numout,  nrec,    ntape
      common /bufout/
     & info(10), ifmt1,   ifmt2,   itypea,  itypeb,
     & ibuf,     numout,  nrec,    ntape
c
      integer      l2rec
      equivalence( l2rec, info(4) )
      integer      n2max
      equivalence( n2max, info(5) )
c
      real*8         cutoff, tol
      common /parmr/ cutoff, tol
c
      integer
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
      common /parmi/
     & mblu,  mccu, mconu, mcu,  mcxu, mpru, nbft, nnbft,
     & mrcru, mstu, msu,   nblu, ng,   ns,   nst
c
      integer        ipq
      common /dim21/ ipq(256)
c
c     # dummy:
      integer lcore, mem1, ifirst
c
c     # local:
      integer iclock, iargos, ierr, h2o1e, h2o2e
c
      integer
     & nf(msup),      nc(msup),      ncon(mconsp),    nrcr(mconsp),
     & lmnp1(mconsp), ntl(msfup),    ntu(msfup),      nt(msfup),
     & mcons(msfup),  iprst(mnsfup), npair(2,mnsfup), icxst(2,mnsfup),
     & nfct(mccup),   ngw(mccup),    icb(4,24,mccup),
     & ica(mcup,msup,mgup),          nprir(2,mstup,mnsfup)
c
c     # ipt(*) is used for workspace stack pointers.
      integer ipt(55)
c
      real*8
     & a(lcore),       zet(mconup,mconsp),  eta(mrcrup,mconup,mconsp),
     & x(mcup,msup),   y(mcup,msup),        z(mcup,msup)
c
c     # timer operations.
      integer   tmin0
      parameter(tmin0=0)
      integer   tminit,  tmprt,  tmrein,  tmclr,  tmsusp,  tmresm
      parameter(tminit=1,tmprt=2,tmrein=3,tmclr=4,tmsusp=5,tmresm=6)
c
c     # bummer error types.
      integer    wrnerr,  nfterr,  faterr
      parameter (wrnerr=0,nfterr=1,faterr=2)
c
      integer  atebyt, forbyt
      external atebyt, forbyt
c
      call timer(' ', tmin0, iargos, nlist )
c
      msu   = msup
      mstu  = mstup
      mrcru = mrcrup
      mconu = mconup
      mcu   = mcup
      mccu  = mccup
      mblu  = lcore
c
c     # allocate space for lmnv
      ipt(1) = 1
c
      h2o1e = 0
      h2o2e = 0
c
      call timer(' ', tminit, iclock, nlist )
c
c     # read input, compute il(*), cx(*), and the 1-e integrals.
c
c     # initialize the high-water stack.
c     # it is not necessary to push, since no workspace has been
c     # allocated, but do it anyway in case allocation is performed
c     # at this point at a later time.
      call h2oini( 0 )
      call h2opsh( 0 )
c
            call seg1mn( a,      eta,       ica,    icb,    icxst,
     &   iprst,  ipt,    lmnp1,  a(ipt(1)), mcons,  nc,     ncon,
     &   nf,     nfct,   ngw,    npair,     nprir,  nrcr,   nt,
     &   ntl,    ntu,    x,      y,         z,      zet,    lcore,
     &   ifirst, mem1 )
c
c     # read the 1-e high-water mark and check for errors.
      call h2opop
      call h2omtr( h2o1e)
      call h2ofin( ierr )
      if ( ierr .ne. 0 ) then
        call bummer('driver: from 1-e h2ofin, ierr=',ierr,wrnerr)
      endif
c
      call timer('seg1mn required', tmrein, iclock, nlist )
c
*      if ( only1e .ne. 1 ) then
*c
*c       # compute the two-electron integrals.
*c
*c       # assign the output unit ntape, and open the 2-e file.
*c
*        call sifo2f( aoints, iunits(3), fnames(3), info, ntape, ierr )
*        if ( ierr .ne. 0 ) then
*          call bummer('driver: from sifo2f(), ierr=',ierr,faterr)
*        endif
*c
*c       # allocate output record and value buffers.
*c       # ipt(*)-->1:lmnv(1:3,1:lmnvmx), 2:il(1:nnbft), 3:cx(1:mcxu),
*c       #          4:buffer(1:l2rec), 5:values(1:n2max),
*c       #          6:labels(1:4,1:n2max), 7:ibitv(1:n2max), 8:h4(1:nblu
*c
*c       # note that ibitv(*) is rounded up to a multiple of 64.
*c
*        ipt(5) = ipt(4) + atebyt( l2rec )
*        ipt(6) = ipt(5) + atebyt( n2max )
*        ipt(7) = ipt(6) + forbyt( 4*n2max )
*        ipt(8) = ipt(7) + forbyt( ((n2max+63)/64)*64 )
*c
*c       # mblu is the remaining workspace.
*c       mblu    = lcore - ipt(8) + 1
*c
*        if ( mblu .le. 0 ) then
*          call bummer('driver: mblu=',mblu,faterr)
*        endif
*c
*c       # initialize the high-water stack, and push the current
*c       # workspace allocation onto the stack.
*        call h2oini( 0 )
*        call h2opsh( (ipt(8) - 1) )
*c
*            call twoint( a,         a(ipt(3)), eta,       a(ipt(8)),
*     &        ica,       icb,       icxst,     a(ipt(2)), iprst,
*     &        ipt,       lmnp1,     a(ipt(1)), mcons,     nc,
*     &        ncon,      nf,        nfct,      ngw,       npair,
*     &        nprir,     nrcr,      nt,        ntl,       ntu,
*     &        x,         y,         z,         zet,       a(ipt(4)),
*     &        a(ipt(5)), a(ipt(6)), a(ipt(7)) )
*c
*        call timer('twoint required', tmclr, iclock, nlist )
*c
*c       # close the 2-e file.
*c
*        call sifc2f( ntape, info, ierr )
*        if ( ierr .ne. 0 ) then
*          call bummer('driver: from sifc2f(), ierr=',ierr,faterr)
*        endif
*c
*c       # pop back to the initial stack level, read the high-water
*c       # mark for the twoint() section, and check for errors.
*        call h2opop
*        call h2omtr( h2o2e )
*        call h2ofin( ierr )
*        if ( ierr .ne. 0 ) then
*          call bummer(' driver: from h2ofin, ierr=',ierr,wrnerr)
*        endif
*      endif
c
c     # close the 1-e file.
c
      close(unit=aoints)
c
c     # write out the core allocation high-water marks.
      write(nlist, * )
      write(nlist, 6010 ) '1-e integral ', h2o1e
      write(nlist, 6010 ) '2-e integral ', h2o2e
      write(nlist, 6010 ) 'overall argos', max( h2o1e, h2o2e )
c
      call timer('argos required', tmclr, iargos, nlist )
c
      return
6010  format(' driver: ',a,' workspace high-water mark =',i10)
      end
