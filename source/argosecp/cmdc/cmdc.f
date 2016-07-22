*cmdc.f
*cmdc part=1 of 1.  source code converter utility program
*version=4.1 last modified: 08-dec-93
c
c  RCS $Revision: 1.1.1.1 $  $Date: 2001/04/09 08:15:53 $
c
*deck cmdc
      program cmdc
c
c  this program activates blocks of machine-dependent source code
c  based on matching a set of input flags read from a configuration
c  file with flags embedded in the source code.  The blocks of code
c  are delimited with keyword strings that are comments to the fortran
c  complier.  the inactive blocks are not deleted from the output file
c  but are simply "commented out" thereby allowing these blocks to be
c  reactivated at a later time with a different set of input flags.
c
c  this program has the following features and restrictions:
c  * flags and keywords cannot have embedded spaces.
c  * all source-code flags must be on one 72-column line.
c  * the c-character is the first character of the keyword prefix.
c    (default: *)
c  * all character strings, including any delimiting spaces (only for
c    keywords), must contain no more than 16 characters.
c  * the line immediately following a keyword line must not be a
c    comment beginning with the c-character. (this line is used to
c    determine the input block status; the exception is that level
c    0 lines are always assumed to be active in the input, regardless
c    of the first character of the first line.)
c  * all character comparisons are performed on strings converted to
c    upper case.
c  * all fortran statements within a block must be no longer than
c    71 characters in length.  (the c-character is added to the first
c    column in the output file if the block is inactive--the fortran
c    standard mandates that the output line should be no longer than 72
c    characters.)
c  * all keywords must be distinct.
c
c  * source code must be in the following form:
c
c        ...normal lines: state=stnorm
c        if-key flag1 flag2 ...
c          ...conditional code: state=stif
c        elseif-key flag3 flag4 ...
c          ...conditional code: state=stelif
c        elseif-key flag5 flag6 ...
c          ...conditional code: state=stelif
c        else-key
c          ...conditional code: state=stelse
c        endif-key
c        ...normal lines: state=stnorm
c
c  * the set of blocks between the if-key and the endif-key is called a
c     "block structure".
c  * the elseif blocks and else block are optional but, if included,
c    must occur in the above sequence.
c  * the character strings used to define the above keys
c    are input variables.
c    (defaults:  if-key     -- *mdc*if
c                elseif-key -- *mdc*elseif
c                else-key   -- *mdc*else
c                endif-key  -- *mdc*endif )
c  * the interpretation of the flags on a keyword line is:
c      defined(flag1) .or. defined(flag2) .or. ... .or. defined(flagn)
c    In addition, logical expressions of the flags are allowed.
c    Operators include ()s, .NOT., .AND., and .OR. For further details,
c    consult the source code for the logical expression library and
c    the associated documentation.
c  * flags must be distinct from the logical operator strings
c    (for example, do not try to define 'AND' as a flag).
c  * only the first block within a block structure for which a
c    source-flag matches an input-flag is activated.
c  * if no blocks are activated within a block structure, then the
c    else-state code is activated.  The only exception to this is the
c    special case, activ8.ne.0, for which all blocks within a
c    block-structure are activated in the output file.  this option is
c    included to facilitate source code comparisons and other
c    maintenence operations.
c  * block structures may be nested.  the line that follows the
c    endif-key determines the input status of the remainder of
c    the lines within the block structure.  The exception is that
c    level 0 lines are always assumed to be active upon input.
c
c  written  by ron shepard.
c  modified by don comeau
c
c  version log:
c  19-nov-96 nested block structures added. -rls
c  07-dec-93 posix calls changed from f77* to pxf*; minor cleanup. -rls
c  24-apr-92 return block added for ibm rs6000. -rls
c  13-mar-91 who2c() and posix code added. parstk() "or" bug fixed. -rls
c  17-may-90 more situations covered by state machine (dcc)
c  14-may-90 optional: report all errors instead of aborting (dcc)
c  19-apr-90 keywords begin with a common prefix (dcc)
c  18-apr-90 use unicas for consistent case file names (dcc)
c  03-apr-90 provide line and line number for lexp errors (dcc)
c  28-aug-89 logical expressions, simpler flag processing (dcc)
c  21-jul-89 kynorm parameter bug fixed (rls/dcc).
c  23-feb-89 block if-elseif-else-endif processing added (rls).
c            this version is substantially rewritten and uses a
c            finite-state parse table which results in more compact
c            code.
c  previous version log:
c  20-nov-87 uppercase keyword comparisons, add sun+alliant (rls/rab).
c  22-jun-87 check that inactive lines begin with c-char (rls/rab).
c  03-jun-87 use c-character to determine status of input blocks (rls).
c  22-aug-86 allow blank and no-keyword on-string lines (rls/dc).
c  20-aug-86 file consistency checks added (rls/rab).
c  20-aug-86 qinopt added in mdc blocks (rls/don comeau).
c  19-aug-86 print warning for long mdc lines (rls/don comeau).
c  14-mar-86 first version written by ron shepard.
c
c  cmdc flags:
c  pipemode  used on machines that support the idea of standard input
c            and standard output.  these are assumed to be preconnected
c            to unnamed files.  the association between filenames and
c            internal unit numbers is established by the environment.
c            this currently includes unix machines and the macintosh
c            mpw environment.  other machines must explicitly open the
c            input and output files.
c  unix      unix-specific code.
c  vax       vax/vms specific code.
c  posix     for machines that support the f77 posix interface.
c  stripopt  use with caution.  see the code below for details.
c
      implicit none
c
      integer   maxst,    nkey,   flagmx,    maxlin
      parameter(maxst=16, nkey=4, flagmx=50, maxlin=72)
c
      character*(maxst) keywds(nkey), flags(flagmx), prefix
      character*1 cchar
      character*(maxlin) line
      common /ccom/ keywds, flags, line, cchar, prefix
c
      integer          nflags, strip ,activ8, mdcerr, bumerr
      common / cinfo / nflags, strip, activ8, mdcerr, bumerr
c
      integer         nin, nlist, nflin, nflout
      common /cfiles/ nin, nlist, nflin, nflout
c
      integer
     & level,  levmx, nblkst, nlinei,
     & nlineo, nitoi, nitoa,  natoi,  natoa
      common /cstat/
     & level,  levmx, nblkst, nlinei,
     & nlineo, nitoi, nitoa,  natoi,  natoa
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
c     # keys...
      integer   noky,   ifky,   elifky,   elseky,   endky
      parameter(noky=0, ifky=1, elifky=3, elseky=4, endky=2)
      integer     errky,      eofky
      parameter ( errky = -1, eofky = -2 )
c
c     # states...
      integer   sterr,    stnorm,   stif,   stelif,   stelse
      parameter(sterr=-1, stnorm=0, stif=1, stelif=2, stelse=3)
      integer     steof
      parameter ( steof = -2 )
c
      integer ierr, ierrvl, lptr
      integer key
      integer nstate
      logical outblk
      external bummer, cpblk, ibummr, rdline, readin, strtil, unicas
      character*(maxst) unicas
c
      integer   nlevmx
      parameter(nlevmx=50)
      logical output(0:nlevmx), blstat(0:nlevmx)
      integer state(0:nlevmx)
c
c     # state-transition table.  referenced as
c     #             new_state = trans(state,key).
c
      integer trans( steof:stelse, eofky:elseky )
      data trans/
     & steof,  sterr,   steof,  steof,  steof,  steof,
     & steof,  sterr,   sterr,  sterr,  sterr,  sterr,
     & steof,  sterr,   stnorm, stif,   stelif, stelse,
     & steof,  sterr,   stif,   stif,   stif,   stif,
     & steof,  sterr,   sterr,  stnorm, stnorm, stnorm,
     & steof,  sterr,   sterr,  stelif, stelif, sterr,
     & steof,  sterr,   sterr,  stelse, stelse, sterr/
c
      nin    = 20
      nlist  = 21
      nflin  = 5
      nflout = 6
      call ibummr( nlist )
c
      open (unit = nlist, file = unicas('cmdcls'), status = 'unknown',
     & iostat = ierr )
      if ( ierr .ne. 0 ) call bummer(
     & 'cmdc: error opening cmdcls, ierr=', ierr, faterr )
c
      open ( unit = nin, file = unicas('cmdcin'), status = 'old',
     & iostat = ierr )
      if ( ierr .ne. 0 ) call bummer(
     & 'cmdc: error opening cmdcin, ierr=', ierr, faterr )
c
      write(nlist,6010)
6010  format(/' program "cmdc 4.2a1"'
     & //' convert machine-dependent code blocks within fortran',
     & ' source code'
     & //' programmed by: ron shepard'
     & /' modified by:   don comeau'
     & /' version date: 19-nov-96')
c
      call who2c( 'CMDC', nlist )
      call headwr(nlist,'CMDC','5.4.0.2 ') 
c
c     # read and echo input from the configuration file.
c
      call readin
c
c     # initialize some internal stuff...
c
      nlinei = 0
      nlineo = 0
      nitoi  = 0
      nitoa  = 0
      natoi  = 0
      natoa  = 0
      nblkst = 0
      mdcerr = 0

c     # outblk = determines the output status of the current block.
c     # level = the current if-block level.  the intital level is 0.
c     # blstat(level) = .false. until an active output block is found
c                       at that level, and then it is set to .true.
c     # output(level) = the status of the current output block.
c     # state(level) = the state of the current level.
c
      outblk = .true.
      level  = 0
      blstat(level) = .true.
      output(level) = .true.
      state(level) = stnorm
      levmx  = level
c
c     # set up to copy lines to the first keyword line, then
c     # process the keyword line to determine the output block
c     # status.
c
      outblk = .true.
      call cpblk( outblk, key )
c
100   if ( key .ne. eofky ) then
c
         if ( key .eq. ifky ) then
c           # push to a new level.
            if ( level .ge. nlevmx ) then
               call bummer( 'cmdc: level exceeded, nlevmx=',
     &          nlevmx, faterr )
            endif
            level = level + 1
            levmx = max(levmx, level)
            nblkst = nblkst + 1
            call parsel( outblk)
            blstat(level) = outblk
            output(level) = output(level-1) .and. outblk
            nstate = stif
         elseif ( key .eq. elifky ) then
            call parsel( outblk )
            if ( blstat(level) ) then
               output(level) = .false.
            else
               blstat(level) = outblk
               output(level) = output(level-1) .and. outblk
            endif
            nstate = trans( state(level), key )
         elseif ( key .eq. elseky ) then
            if ( blstat(level) ) then
               output(level) = .false.
            else
               output(level) = output(level-1)
            endif
            nstate = trans( state(level), key )
         elseif ( key .eq. endky ) then
            if ( level .le. 0 ) then
               call bummer('cmdc: improper if-block nesting. nlinei=',
     &          nlinei, wrnerr )
               mdcerr = mdcerr + 1
c              # reset level and try to continue.
               level = 1
            endif
            level = level - 1
            nstate = state(level)
         endif
c
         if ( nstate .eq. sterr ) then
            call bummer(
     &       'cmdc: improper keyword order detected. nlinei=',
     &       nlinei, wrnerr )
            mdcerr = mdcerr + 1
c           # reset the current state and try to continue.
            nstate = stnorm
         endif
         state(level) = nstate
c
c        # write out the current keyword line.
         if ( strip .eq. 0 ) then
            call strwr( nflout, line )
            nlineo = nlineo + 1
         endif
c
c        # copy the current block up to the next keyword line.
         outblk = output(level) .or. (activ8 .ne. 0)
         call cpblk( outblk, key )
         goto 100
      endif
c
c     # check for correct level upon exit
      if ( level .ne. 0 ) then
         call bummer('cmdc: at eof, level=', level, wrnerr )
         mdcerr = mdcerr + 1
      endif
c
c     # all done. write out statistics.
c
      write(nlist,6040)
     & nblkst, nitoi, nitoa, natoi, natoa, nlinei, nlineo, levmx
6040  format(' nblkst=',i6,'  nitoi=',i6,'  nitoa=',i6,'  natoi=',i6,
     & '  natoa=',i6/' nlinei=',i6,' nlineo=',i6,'  levmx=',i6)
c
      if ( (strip .eq. 0) .and. (nlinei .ne. nlineo) ) then
         call bummer('cmdc: line count error, diff=',nlinei-nlineo,
     &    bumerr )
         mdcerr = mdcerr + 1
      endif
c
      if ( mdcerr .gt. 0 ) call bummer('cmdc: errors -- ', mdcerr,
     & faterr )
c
c     # for braindamaged unix machines,
c     # keep the stop statement from writing to stdout. -rls
c
      end
      subroutine parsel( outblk )
c
c     # parse the current line to check for errors and to determine
c     # the output status of the subsequent block.
c     #
c     # output:
c     #    outblk = .true. or .false, depending on the current line.
c     #
c     # 19-nov-96 written by ron shepard.
c
      implicit none
      logical outblk
c
      integer   maxst,    nkey,   flagmx,    maxlin
      parameter(maxst=16, nkey=4, flagmx=50, maxlin=72)
c
      character*(maxst) keywds(nkey), flags(flagmx), prefix
      character*1 cchar
      character*(maxlin) line
      common /ccom/ keywds, flags, line, cchar, prefix
c
      integer          nflags, strip ,activ8, mdcerr, bumerr
      common / cinfo / nflags, strip, activ8, mdcerr, bumerr
c
      integer         nin, nlist, nflin, nflout
      common /cfiles/ nin, nlist, nflin, nflout
c
      integer
     & level,  levmx, nblkst, nlinei,
     & nlineo, nitoi, nitoa,  natoi,  natoa
      common /cstat/
     & level,  levmx, nblkst, nlinei,
     & nlineo, nitoi, nitoa,  natoi,  natoa
c
      integer lptr, ierr, ierrvl
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
      lptr = 1
      call strtil( line, lptr, ' ' )
      call lexp( line(lptr+1:), nflags, flags, outblk,
     & ierr, ierrvl )
c
      if ( ierr .ne. 0 ) then
         write(nlist, '(1x,a,i8)' )
     &    'cmdc: logical expression error, line=', nlinei
         call strwr( nlist, line )
         call lexpe( bumerr, ierr, ierrvl )
         mdcerr = mdcerr + 1
      endif
c
      return
      end
*deck readin
      subroutine readin()
c
c     read user input.
c
c     version log:
c     13-mar-91 posix code added. -rls
c     14-may-90 read abort flag (dcc)
c     19-apr-90 read keyword prefix (dcc)
c     27-aug-89 no not keywords, don't keep length of flags (dcc)
c     23-feb-89 written by ron shepard.
c
      implicit none
c
      integer   maxst,    nkey,   flagmx,    maxlin
      parameter(maxst=16, nkey=4, flagmx=50, maxlin=72)
c
      character*(maxst) keywds(nkey), flags(flagmx), prefix
      character*1 cchar
      character*(maxlin) line
      common /ccom/ keywds, flags, line, cchar, prefix
c
      integer          nflags, strip ,activ8, mdcerr, bumerr
      common / cinfo / nflags, strip, activ8, mdcerr, bumerr
c
      integer         nin, nlist, nflin, nflout
      common /cfiles/ nin, nlist, nflin, nflout
c
      integer
     & level,  levmx, nblkst, nlinei,
     & nlineo, nitoi, nitoa,  natoi,  natoa
      common /cstat/
     & level,  levmx, nblkst, nlinei,
     & nlineo, nitoi, nitoa,  natoi,  natoa
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
c     # keys...
      integer   noky,   ifky,   elifky,   elseky,   endky
      parameter(noky=0, ifky=1, elifky=3, elseky=4, endky=2)
c
      character*60 filein, filout
      integer abort, i, ierr, itemp, prelen
      character*(maxst) tflag
c
      external bummer, strtok, strupp
      integer  strlen
      external strlen
c
c     # set default input:
c     # activ8 .ne. 0 => active all blocks in the output file
c     # abort  .ne. 0 => stop immediately on errors.
c     # strip  .ne. 0 => strip all keyword lines and inactive blocks
c     #                  from the output file.
c     # keywds(i) should correspond to key=i.
c
      activ8    = 0
      abort     = 0
      strip     = 0
      prefix    = '*mdc*'
      keywds(1) = 'if'
      keywds(2) = 'endif'
      keywds(3) = 'elseif'
      keywds(4) = 'else'
      cchar     = prefix(1:1)
      nflags    = 0
c
      do 10 i = 1, flagmx
         flags(i) = ' '
10    continue
c
c     # optional flags(*) from the command line should be set here. -rls
c
c     ......
c
      filein = 'input'
      filout = 'output'
c
      write(nlist,6010) 'input options:'
      read(nin,*,iostat=ierr) activ8, abort, strip
      if ( ierr .ne. 0 )
     & call bummer('readin: option ierr=',ierr,faterr)
*mdc*if stripopt
*c     # special version should be required to enable stripping since
*c     # this can introduce irreversible source code changes. -rls
*      write(nlist,'(1x,a,i4,a,i4,a,i4)')
*     & 'activ8=', activ8, ' abort = ', abort, ' strip=', strip
*mdc*else
      strip = 0
      write(nlist,'(1x,a,i4,a,i4)')
     & 'activ8=', activ8, ' abort = ', abort
*mdc*endif
      if ( abort .ne. 0 ) then
         bumerr = faterr
      else
         bumerr = wrnerr
      endif
c
c     # read the keywords.
c
      write(nlist,6010)
     & 'input keywords (prefix,if,end,elseif,else):'
      read(nin,*,iostat=ierr) prefix, keywds
      if ( ierr .ne. 0 )
     & call bummer('readin: keyword ierr=',ierr,faterr)
c
c     # determine keyword lengths with trailing space delimiter.
c
      if ( prefix .eq. ' ' ) call bummer( 'readin: null prefix', 0,
     &     faterr )
      cchar  = prefix(1:1)
      prelen = strlen( prefix )
      call strupp( prefix(1:prelen) )
      write(nlist,6010) 'keywords(*)='
      do 20 i = 1, nkey
         if ( keywds(i) .eq. ' ' ) call bummer
     &    ('readin: null keyword, i=',i,faterr)
         call strupp( keywds(i) )
         write ( line, '(1x,i3,'': '',a)' ) i, prefix(1:prelen) //
     &        keywds(i)
         call strwr( nlist, line )
20    continue
c
      write(nlist,6010) 'c-char=' // cchar
c
c     # read the flags.
c
      write(nlist,6010) 'input flags(*):'
      read(nin,*,iostat=ierr)flags
c
c     # count the number of input flags.
c     # filein(*) is used for scratch.
c
      nflags = 0
      write(nlist,6010) 'flags(*)='
      do 30 i = 1, flagmx
         if ( flags(i) .eq. ' ' ) go to 31
         nflags = i
         itemp  = 1
c        # error conditions already checked for
         call strtok( flags(i), itemp, tflag, ' ', ierr )
         flags(i) = tflag
         write(nlist,'(1x,i3,'': '',a)')i,flags(i)(:strlen(flags(i)))
30    continue
31    continue
c
c     # pipemode: read from stdin and write to stdout.  units are
c     # assumed to be preconnected, so don't bother with the filenames
c     # and open statements.
c
*mdc*if pipemode .and. posix
*c
*c     # reset the nflin and nflout units if necessary.
*c     # this mdc block should be activated only on posix machines
*c     # that support the 1003.9 fortran interface.
*c
*      line = 'stdin_unit'
*      call allcap( line )
*      call pxfconst( line, nflin, ierr )
*      if ( ierr .ne. 0 )call bummer('readin: stdin ierr=',ierr,faterr)
*c
*      line = 'stdout_unit'
*      call allcap( line )
*      call pxfconst( line, nflout, ierr )
*      if ( ierr .ne. 0 )call bummer('readin: stdout ierr=',ierr,faterr)
*mdc*endif
*mdc*if pipemode
      write(nlist,6010)
     & 'source code read from stdin and written to stdout.'
c
c     # redundant if() is included to eliminate compiler warnings.
c     # this return should always be taken for pipemode machines. -rls
      if ( (nlist .gt. 0) .or. (nlist .le. 0) ) return
*mdc*endif
c
c     # get the source code file names.
c
      write(nlist,6010)
     & 'input source filename and destination filename:'
      read(nin,*,iostat=ierr) filein, filout
      if ( ierr .ne. 0 )
     & call bummer('readin: file name, ierr=',ierr,faterr)
      open( unit=nflin, file=filein, status='old' )
      inquire( unit=nflin, name=filein )
*mdc*if vax
*c     # special open parameter is required to get the right
*c     # text file type.
*      open(unit=nflout,file=filout,status='new',carriagecontrol='list')
*mdc*else
      open( unit=nflout, file=filout, status='unknown' )
*mdc*endif
      inquire( unit=nflout, name=filout )
      write(nlist,6010) ' input source file:', filein
      write(nlist,6010) 'output source file:', filout
c
      return
6010  format(1x,a)
      end
*deck cpblk
      subroutine cpblk( outblk, key )
c
      implicit none
c
      logical outblk
      integer key
c
c     process a machine-dependent code block.
c
c     input:
c     outblk = output block status.
c
c     output:
c     line, key = info for the last line of the block.
c
c     version log:
c     28-feb-87 level added for nested blocks. -rls
c     17-may-90 do not print mdc lines (mdc.F will) (dcc)
c     10-may-90 single loop for all active/inactive cases (dcc)
c     27-aug-89 lenfcm not used, consistent with other changes (dcc)
c     21-jul-89 kynorm defined (rls/dcc)
c     23-feb-89 written by ron shepard.
c
      integer   maxst,    nkey,   flagmx,    maxlin
      parameter(maxst=16, nkey=4, flagmx=50, maxlin=72)
c
c     # keys...
      integer   noky,   ifky,   elifky,   elseky,   endky
      parameter(noky=0, ifky=1, elifky=3, elseky=4, endky=2)
      integer     errky,      eofky
      parameter ( errky = -1, eofky = -2 )
c
      character*(maxst) keywds(nkey), flags(flagmx), prefix
      character*1 cchar
      character*(maxlin) line
      common /ccom/ keywds, flags, line, cchar, prefix
c
      integer          nflags, strip ,activ8, mdcerr, bumerr
      common / cinfo / nflags, strip, activ8, mdcerr, bumerr
c
      integer         nin, nlist, nflin, nflout
      common /cfiles/ nin, nlist, nflin, nflout
c
      integer
     & level,  levmx, nblkst, nlinei,
     & nlineo, nitoi, nitoa,  natoi,  natoa
      common /cstat/
     & level,  levmx, nblkst, nlinei,
     & nlineo, nitoi, nitoa,  natoi,  natoa
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
      external bummer
c
      logical inblk
      character*1 fchar
c
      external rdline, strupp
c
c     # read the first line of the block and determine the
c     # input block status for this block.
c
      call rdline( key )
c
      if ( level .gt. 0 ) then
c
c        # determine the output status from the first line.
c        # cchar was strupp-ed earlier
         fchar = line(1:1)
         call strupp(fchar)
         inblk = fchar .ne. cchar
      else
c
c        # at level 0, input is always assumed to be active.
         inblk = .true.
      endif
c
c     # loop over lines until an mdc keyword found
c
100   continue
c
      if ( key .eq. noky ) then
c
c        # how (or if) should line be printed?
c
         if ( strip .eq. 0  .or.  outblk ) then
            nlineo = nlineo + 1
            if ( inblk .and. outblk ) then
c              # active -to- active
               natoa = natoa + 1
               call strwr( nflout, line )
            elseif ( inblk .and. .not. outblk ) then
c              # active -to- inactive
               natoi = natoi + 1
               call strwr( nflout, cchar//line )
            elseif ( .not. inblk .and. outblk ) then
c              # inactive -to- active
               nitoa = nitoa + 1
               call strwr( nflout, line(2:) )
            elseif ( .not. inblk .and. .not. outblk ) then
c              # inactive -to- inactive
               nitoi = nitoi + 1
               call strwr( nflout, line )
            endif
         endif
c
c        # check for ...
c
         if ( inblk ) then
c           # ... long lines ...
            if ( (level .gt. 0) .and. (line(72:72) .ne. ' ') ) then
               call bummer( 'long line within mdc block, nlinei=',
     &              nlinei, wrnerr )
               call strwr( nlist, ' '//line )
            endif
         else
c           # ... or missing cchar.
            fchar = line(1:1)
            call strupp(fchar)
            if ( fchar .ne. cchar ) then
               write(nlist,'(1x,a,i8)')
     &          'inactive line does not begin with '
     &          // cchar // ', nlinei=', nlinei
               write(nlist,'(1x,a)')line
               call bummer('cpblk: cchar error, nlinei=', nlinei,
     &              bumerr)
               mdcerr = mdcerr + 1
            endif
         endif
c
c        # read the next line.
c
         call rdline( key )
c
         go to 100
      endif
c
      return
      end
*deck rdline
      subroutine rdline( key )
c
      implicit none
c
      integer key
c
c     read a line of source code and check for keywords.
c
c     output:
c     line(*) = unmodified line from the input file.
c     key     = integer label for matching keyword.
c
c     version log:
c     14-may-90  don't abort on errors (dcc)
c     27-aug-89  lenfcm/lenlin not needed, strndx used to identify
c                key (dcc
c     23-feb-89  written by ron shepard.
c
      integer   maxst,    nkey,   flagmx,    maxlin
      parameter(maxst=16, nkey=4, flagmx=50, maxlin=72)
c
      character*(maxst) keywds(nkey), flags(flagmx), prefix
      character*1 cchar
      character*(maxlin) line
      common /ccom/ keywds, flags, line, cchar, prefix
c
      integer          nflags, strip ,activ8, mdcerr, bumerr
      common / cinfo / nflags, strip, activ8, mdcerr, bumerr
c
      integer         nin, nlist, nflin, nflout
      common /cfiles/ nin, nlist, nflin, nflout
c
      integer
     & level,  levmx, nblkst, nlinei,
     & nlineo, nitoi, nitoa,  natoi,  natoa
      common /cstat/
     & level,  levmx, nblkst, nlinei,
     & nlineo, nitoi, nitoa,  natoi,  natoa
c
c     # keys...
      integer   noky,   ifky,   elifky,   elseky,   endky
      parameter(noky=0, ifky=1, elifky=3, elseky=4, endky=2)
      integer     errky,      eofky
      parameter ( errky = -1, eofky = -2 )
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
      external bummer
c
      integer error
      integer itemp
      integer lstat
      character*(maxst) tempky
c
      external streqs, strlen, strndx
      logical  streqs
      integer  strlen, strndx
      external strtok
c
      error = 0
      tempky = ' '
c
      read(nflin,'(a)',iostat=lstat)line
      if ( lstat .gt. 0 ) then
         write(nlist,'(1x,a,i8)')
     &    'error reading input line, nlinei=',nlinei
         call bummer('cmdc: nlinei=',nlinei,faterr)
      endif
      if ( lstat .ne. 0 ) then
         key = eofky
         return
      endif
      nlinei = nlinei + 1
c
      key = noky
      if ( streqs( prefix, line ) ) then
         itemp = strlen(prefix) + 1
         call strtok( line, itemp, tempky, ' ', error )
c        # if long token, no possible match
         key = strndx( nkey, tempky, keywds, 1 )
         if ( error .ne. 0  .or. key .eq. noky ) then
            call strwr( nlist, line )
            call bummer( 'cmdc: unknown keyword, line = ', nlinei,
     &           bumerr )
            mdcerr = mdcerr + 1
            key = errky
         endif
      endif
c
      return
      end



