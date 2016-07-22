*colib7.f
*colib part=7 of 9.  miscellaneous utilities.
*version=4.1 last modified: 24-apr-92
c
c  RCS $Revision: 1.3 $  $Date: 2001/09/07 02:25:12 $
c
c  see colib1.f for version history info.
c
c  this file is used by cmdc.
c
*deck bummer
*deck ibummr
      subroutine bummer( text, ierr, errtyp )
c
c  process a program error.
c  input:
c  text  = character string to be printed.
c  ierr  = internal program error to be printed.
c  errtyp = 0 for warning.  traceback may be generated. execution
c             continues.
c         = 1 for nonfatal error.  traceback may be generated.
c             execution is stopped. jcl condition code is set to allow
c             subsequent program steps to continue if possible.
c         = 2 for fatal error.  traceback may be generated.
c             execution is stopped. jcl condition code is set to abort
c             subsequent program steps if possible.
c
c  entry ibummr must be called prior to bummer() to set the output
c  unit and to perform any additional initialization.
c
c  version log:
c  24-apr-92 %val() blocks added for ibm rs6000. -rls
c  11-sep-91 parerr() calls added. -rjh
c  13-mar-91 posix code added. -rls
c  01-mar-89 write(stderr,*) and call exit() for unix machines (rls).
c  05-jul-88 unicos version (rls).
c  03-nov-87 ibm version (dcc).
c  10-sep-87 written by ron shepard.
c
      implicit integer(a-z)
c
      character*(*) text
      integer iunit, ierr, errtyp
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
      integer f77err
      integer nlist,        stderr
      save    nlist,        stderr
      data    nlist / 6 /,  stderr / 0 /
c
c*******************************************************************
      if ( errtyp .eq. wrnerr ) then
c*******************************************************************
c
c        # print a warning message and continue execution.
c
         write(nlist,6010) 'bummer (warning):', text, ierr
*mdc*if posix
*         write(stderr,6010) 'bummer (warning):', text, ierr
*mdc*elseif unicos
*         write(stderr,6010) 'bummer (warning):', text, ierr
*         call tracebk( nlist )
*mdc*elseif unix
         write(stderr,6010) 'bummer (warning):', text, ierr
*mdc*endif
         return
c
c*******************************************************************
      elseif ( errtyp .eq. nfterr ) then
c*******************************************************************
c
c        # print a warning message, stop execution.
c
         write(nlist,6010) 'bummer (nonfatal):', text, ierr
*mdc*if posix
*         write(stderr,6010) 'bummer (nonfatal):', text, ierr
*         call parerr( ierr )
*         call f77exit( 0 )
*mdc*elseif unicos
*         write(stderr,6010) 'bummer (nonfatal):', text, ierr
*         call tracebk( nlist )
*         call parerr( ierr )
*         stop 'program error'
*mdc*elseif rs6000
*c        # exit() requires call-by-value argument.
*         write(stderr,6010) 'bummer (nonfatal):', text, ierr
*         call parerr( ierr )
*         call exit( %val(0) )
*mdc*elseif unix
         write(stderr,6010) 'bummer (nonfatal):', text, ierr
         call parerr( ierr )
         call exit( 0 )
*mdc*elseif vax
*         call sys$exit(%val(42))
*mdc*elseif ibm
*         stop 901
*mdc*else
*         call parerr( ierr )
*         stop 'program error'
*mdc*endif
c
c**********************************************************************
      elseif ( errtyp .eq. faterr ) then
c**********************************************************************
c
c        # print an error message, stop execution, and abort job
c        # sequence.
c
         write(nlist,6010) 'bummer (fatal):', text, ierr
*mdc*if posix
*         write(stderr,6010) 'bummer (fatal):', text, ierr
*         call parerr( ierr )
*         call f77exit( 1 )
*mdc*elseif unicos
*         write(stderr,6010) 'bummer (fatal):', text, ierr
*         call parerr( ierr )
*c        # abort() generates tracebacks automatically.
*         call abort
*mdc*elseif rs6000
*c        # exit() requires call-by-value argument.
*         write(stderr,6010) 'bummer (fatal):', text, ierr
*         call parerr( ierr )
*         call exit( %val(1) )
*mdc*elseif unix
         write(stderr,6010) 'bummer (fatal):', text, ierr
         call parerr( ierr )
         call exit( 1 )
*mdc*elseif vax
*         call sys$exit( %val(44) )
*mdc*elseif ibm
*         stop 902
*mdc*else
*         stop 'program error'
*mdc*endif
c
c*******************************************************************
      else
c*******************************************************************
c
c        # unknown error level.  treat as a fatal error.
c
         write(nlist,6020) 'bummer (unknown): errtyp=',
     &    errtyp, text, ierr
*mdc*if posix
*         write(stderr,6020) 'bummer (unknown): errtyp=',
*     &    errtyp, text, ierr
*         call parerr( ierr )
*         call f77exit( 1 )
*mdc*elseif unicos
*         write(stderr,6020) 'bummer (unknown): errtyp=',
*     &    errtyp, text, ierr
*         call parerr( ierr )
*c        # abort() generates tracebacks automatically.
*         call abort
*mdc*elseif rs6000
*c        # exit() requires call-by-value argument.
*         write(stderr,6020) 'bummer (unknown): errtyp=',
*     &    errtyp, text, ierr
*         call parerr( ierr )
*         call exit( %val(1) )
*mdc*elseif unix
         write(stderr,6020) 'bummer (unknown): errtyp=',
     &    errtyp, text, ierr
         call parerr( ierr )
         call exit( 1 )
*mdc*elseif vax
*         call sys$exit( %val(44) )
*mdc*elseif ibm
*         stop 903
*mdc*else
*         stop 'program error'
*mdc*endif
c*******************************************************************
      endif
c*******************************************************************
c
c     # this statement is not executed, it is included
c     # just to avoid compiler warnings. -rls
      stop 'bummer error'
c
c     # initialization...
c
      entry ibummr( iunit )
c
c     # save the listing unit for use later.
c
      nlist = iunit
c
*mdc*if posix
*c     # set stderr to the correct value.
*      call f77const( 'stderr', stderr, f77err )
*      if ( f77err .ne. 0 ) then
*         write(*,6010) 'ibummr f77const() error=', f77err
*         call f77exit( 1 )
*      endif
*mdc*endif
c
      return
6010  format(1x,a,a,i10)
6020  format(1x,a,i10,a,i10)
      end
*deck pfname
c *** this routine is incremental ***
      subroutine pfname( nnames, fname )
c
c  construct a unique filename by appending the parallel process
c  number after the stub name,
c  i.e. <fname> = <name>_<nodeid>
c
c  input:  nnames = number of filenames to convert.
c          fname(1:nnames)(:) = character filename array.
c                           entries must be nonblank and must be
c                           large enough to hold the converted name.
c  output: fname(1:nnames)(:) = updated filenames.
c
c  *** this routine should only be called when running in parallel ***
c
c  11-sep-91 written by r. j. harrison.
c
      implicit none
      integer nnames
      character*(*) fname(nnames)
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
      integer lenf, k
c
      integer  nodeid, strlen
      external nodeid, strlen
      intrinsic len
c
      do 10 k = 1, nnames
c
         lenf = strlen( fname(k) )
c
         if ( lenf .eq. 0 ) then
            call bummer('pfname: name is all blanks', 0, faterr)
         elseif ( lenf+4 .gt. len(fname(k)) ) then
            call bummer('pfname: fname too short for name_id',
     &       len(fname(k)), faterr )
         endif
c
c        # append stubname with unique integer.
c
         write(fname(k)(lenf+1:lenf+4),'(a1,i3.3)') '_', nodeid()
10    continue
c
      return
      end
*deck pbginf
c *** this routine is incremental ***
      subroutine pbginf
c     # stub routine replacing the calls to tcgmsg
      return
      end
*deck pend
c *** this routine is incremental ***
      subroutine pend
c     # stub routine replacing the calls to tcgmsg
      return
      end
*deck parerr
c *** this routine is incremental ***
      subroutine parerr(ierr)
c     # stub routine replacing the calls to tcgmsg
      return
      end
*deck nodeid
c *** this routine is incremental ***
      integer function nodeid()
c
c  dummy nodeid() function.  This function should never be called.
c  the actual function, in the tcgmsg library, should be called instead.
c  this version is included in colib to ensure that libraries have been
c  loaded in the correct order.  -rls
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
      call bummer( 'colib nodeid() called.  Make sure libraries are'
     & // ' loaded in the correct order.', 0, faterr )
      nodeid = -1
      return
      end
*deck isalnm
      logical function isalnm( char )
c
c  returns true if char is alphabetic or numeric.
c
c  13-mar-91 posix code added. -rls
c  23-nov-90 cmdc block structure used. -rls
c
      character char
c
c  the ascii and ebcdic versions exists, not to make the routine faster,
c  but to make it less case sensitive
c
*mdc*if fortran
*c     # case-dependent standard fortran version.
*c     # *** warning: case dependent code ***
*      character*(*) alnmch
*      parameter ( alnmch = '0123456789'
*     $ //                  'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
*     $ //                  'abcdefghijklmnopqrstuvwxyz' )
*c
*      isalnm = ( index(alnmch,char) .ne. 0 )
*mdc*elseif posix
*      logical  lf77isalnum
*      external lf77isalnum
*      isalnm = lf77isalnum( char )
*mdc*elseif ebcdic
*c     # ebcdic version.
*c     # this code assumes that ichar() returns ebcdic values.
*      logical alnm
*      integer i
*      integer iset
*c
*      integer    sets
*      parameter( sets = 7 )
*      integer   low   ( sets )
*      integer   high  ( sets )
*      data low  / 129, 145, 162, 193, 209, 226, 240 /
*      data high / 137, 153, 169, 201, 217, 233, 249 /
*c
*      i = ichar(char)
*      alnm = .false.
*      do 100 iset = 1, sets
*         alnm = alnm .or. ( low(iset) .le. i .and. i .le. high(iset) )
* 100  continue
*      isalnm = alnm
*mdc*else
c     # ascii version.
c     # this code assumes that ichar() returns ascii values.
      integer i
      integer num0, num9, uppa, uppz, lowa, lowz
      parameter ( num0 =  48 )
      parameter ( num9 =  57 )
      parameter ( uppa =  65 )
      parameter ( uppz =  90 )
      parameter ( lowa =  97 )
      parameter ( lowz = 122 )
c
      i = ichar(char)
      isalnm = ( num0 .le. i  .and.  i .le. num9 )
     & .or.    ( uppa .le. i  .and.  i .le. uppz )
     & .or.    ( lowa .le. i  .and.  i .le. lowz )
*mdc*endif
      return
      end
*deck isalph
      logical function isalph( char )
c
c  returns true if char is alphabetic.
c
c  13-mar-91 posix code added. -rls
c  23-nov-90 cmdc block structure used. -rls
c
      character char
c
c  the ascii and ebcdic versions exists, not to make the routine faster,
c  but to make it less case sensitive
c
*mdc*if fortran
*c     # case-dependent standard fortran version.
*c     # *** warning: case dependent code ***
*      character*(*) alphch
*      parameter ( alphch = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
*     $ //                  'abcdefghijklmnopqrstuvwxyz' )
*c
*      isalph = ( index(alphch,char) .ne. 0 )
*mdc*elseif posix
*      logical  lf77isalpha
*      external lf77isalpha
*      isalph = lf77isalpha( char )
*mdc*elseif ebcdic
*c     # ebcdic version.
*c     # this code assumes that ichar() returns ebcdic values.
*      logical alph
*      integer i
*      integer iset
*c
*      integer   sets
*      parameter ( sets = 6 )
*      integer   low   ( sets )
*      integer   high  ( sets )
*      data low  / 129, 145, 162, 193, 209, 226 /
*      data high / 137, 153, 169, 201, 217, 233 /
*c
*      i = ichar(char)
*      alph = .false.
*      do 100 iset = 1, sets
*         alph = alph .or. ( low(iset) .le. i .and. i .le. high(iset) )
* 100  continue
*      isalph = alph
*mdc*else
c     # ascii version.
c     # this code assumes that ichar() returns ascii values.
      integer i
      integer uppa, uppz, lowa, lowz
      parameter ( uppa =  65 )
      parameter ( uppz =  90 )
      parameter ( lowa =  97 )
      parameter ( lowz = 122 )
c
      i = ichar(char)
      isalph = ( uppa .le. i  .and.  i .le. uppz )
     & .or.    ( lowa .le. i  .and.  i .le. lowz )
*mdc*endif
      return
      end
*deck streq
      logical function streq( str1, str2 )
c
c  returns true if two strings are equal using
c  a case-insensitive comparison.
c
      character*(*)     str1
      character*(*)     str2
c
c  --case insensitive string comparision
c  --need scratch arrays because we don't want to upper case constants
c  --hate having fixed length temp strings, but no way to avoid
c  --could ignore leading spaces, but i think better elsewhere
c    (if someone else adds, i won't delete)
c  --the ascii version exists solely for speed
c
      integer           len1
      integer           len2
*mdc*if fortran
*      integer           mstrln
*      parameter         ( mstrln = 512 )
*      character*(mstrln) tstr1
*      character*(mstrln) tstr2
*mdc*elseif ebcdic
*      integer           mstrln
*      parameter         ( mstrln = 500 )
*      character*(mstrln) tstr1
*      character*(mstrln) tstr2
*mdc*else
c     # ascii version.
      integer           i
      integer           nstr1
      integer           nstr2
      integer           uldiff,      uppa,      uppz
      parameter       ( uldiff = 32, uppa = 65, uppz = 90 )
*mdc*endif
c
c     # bummer error types.
      integer   wrnerr,  nfterr,  faterr
      parameter(wrnerr=0,nfterr=1,faterr=2)
c
c
      len1 = len(str1)
      len2 = len(str2)
c
*mdc*if ebcdic
*c     # convert to upper case and use fortran comparison.
*c-      if ( len1 .gt. mstrln  .or.  len2 .gt. mstrln ) stop 909
*      if ( len1 .gt. mstrln )
*     & call bummer('streq: string too long, len1=',len1,faterr)
*      if ( len2 .gt. mstrln )
*     & call bummer('streq: string too long, len2=',len2,faterr)
*c
*      tstr1(1:len1) = str1
*      tstr2(1:len2) = str2
*      call strupp( tstr1(1:len1) )
*      call strupp( tstr2(1:len2) )
*c
*      streq = tstr1(1:len1) .eq. tstr2(1:len2)
*mdc*else
c
c     # ascii version
c     # this code assumes that ichar() returns ascii values.
c
      streq = .false.
c
      do 100 i = 1, min( len1, len2 )
         nstr1 = ichar(str1(i:i))
         nstr2 = ichar(str2(i:i))
c return if the characters don't "match"
c they match if they're the same or upper/lower of same character
c if they are upper/lower of same char, they differ by uldiff
c   and the smallest one is an uppercase character
         if ( .not. ( nstr1 .eq. nstr2  .or.  (
     $        abs( nstr1 - nstr2 ) .eq. uldiff .and.
     $        min(nstr1,nstr2).ge.uppa .and. min(nstr1,nstr2).le.uppz )
     $        ) ) return
 100  continue
c
      if ( len1 .eq. len2 ) then
         streq = .true.
      elseif ( len1 .lt. len2 ) then
         streq = str2(len1+1:) .eq. ' '
      else
         streq = str1(len2+1:) .eq. ' '
      endif
*mdc*endif
c
      return
      end
*deck streqs
      logical function streqs( str1, str2 )
c
c  compare short string one to the initial characters of str two
c
      character*(*)     str1
      character*(*)     str2
c
c  --the strings are "equal" if the leading characters of the (possibly)
c    longer second string equal the shorter first string
c  --originally the two strings were treated equivalently.  while this
c    feels good, i needed the current behavior
c  --trailing blanks are ignored
c
      integer           strl
c
      external          streq
      logical           streq
      external          strlen
      integer           strlen
c
c
      strl = min( strlen(str1), len(str2) )
c
      streqs = streq( str1, str2(1:strl) )
c
      return
      end
*deck strlen
      integer function strlen( string )
c
c  returns "length" of string -- position of last non-space character
c  (at times it would be convienent to return a character variable of
c  the proper length.  unfortunately, that is not possible in fortran.)
c
c  13-mar-91 posix code added. -rls
c
      character*(*)     string
c
      integer           istr
c
*mdc*if posix
*      integer  if77lentrim
*      external if77lentrim
*      strlen = if77lentrim( string )
*mdc*else
c
c     # assume string has no characters.
      strlen = 0
c
      do 100 istr = len(string), 1, -1
         if ( string(istr:istr) .ne. ' ' ) then
            strlen = istr
            return
         endif
 100  continue
*mdc*endif
c
      return
      end
*deck strlow
      subroutine strlow( string )
c
c  change all upper case characters in string to lower
c
      character*(*)     string
c
c  the ascii and ebcdic versions exists, not to make the routine faster,
c  but to make it less case sensitive
c
      integer           istr
c
*mdc*if fortran
*c     # case-dependent standard fortran version.
*c     # warning: case dependent code
*      integer          indx
*      character*26     lower
*      character*26     upperc
*      data lower  / 'abcdefghijklmnopqrstuvwxyz' /
*      data upperc / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
*c
*      do 100 istr = 1, len(string)
*         indx = index(upperc, string(istr:istr) )
*         if ( indx .ne. 0 )
*     $        string(istr:istr) = lower(indx:indx)
* 100  continue
*mdc*elseif ebcdic
*c     # ebcdic version.
*c     # this code assumes that ichar() returns ebcdic values.
*      integer           ich
*      integer           uldiff
*      parameter ( uldiff = -64 )
*      integer   sets
*      parameter ( sets = 3 )
*      integer   low   ( sets )
*      integer   high  ( sets )
*      data low  / 193, 209, 226 /
*      data high / 201, 217, 233 /
*c
*c
*      do 100 istr = 1, len(string)
*         ich = ichar( string(istr:istr) )
*         if ( ( low(1) .le. ich  .and.  ich .le. high(1) ) .or.
*     $        ( low(2) .le. ich  .and.  ich .le. high(2) ) .or.
*     $        ( low(3) .le. ich  .and.  ich .le. high(3) ) )
*     $        string(istr:istr) = char( ich + uldiff )
* 100  continue
*mdc*else
c     # ascii version.
c     # this code assumes that ichar() returns ascii values.
      integer           ich
      integer uppa, uppz, uldiff
      parameter ( uppa = 65 )
      parameter ( uppz = 90 )
      parameter ( uldiff = 32 )
c
      do 100 istr = 1, len(string)
         ich = ichar( string(istr:istr) )
         if ( uppa .le. ich  .and.  ich .le. uppz )
     $        string(istr:istr) = char( ich + uldiff )
 100  continue
*mdc*endif
      return
      end
*deck strnds
      integer function strnds ( number, target, list, inc )
      implicit integer  ( a-z )
c
c     function strnds returns the element number (index) in
c     array 'list' containing the value 'target' using streqs (short
c     equals) for equality check
c
      integer           number, inc
      character*(*)     list( inc, number ), target
c
      integer           index
c
      external          streqs
      logical           streqs
c
c
c     assume target will not be found
c
      strnds = 0
c
      do 100 index = 1, number
         if ( streqs( list( 1, index ), target ) ) then
c
c           target found
c
            strnds = index
            return
         endif
 100  continue
c
c     target not found
c
      return
      end
*deck strndx
      integer function strndx ( number, target, list, inc )
      implicit integer  ( a-z )
c
c     function strndx returns the element number (index) in
c     array 'list' containing the value 'target' using streq (case
c     insenstive comparison) for equality check
c
      integer           number, inc
      character*(*)     list( inc, number ), target
c
      integer           index
c
      external          streq
      logical           streq
c
c
c     # assume target will not be found.
c
      strndx = 0
c
      do 100 index = 1, number
         if ( streq( list( 1, index ), target ) ) then
c
c           # target found.
c
            strndx = index
            return
         endif
 100  continue
c
c     # target not found
c
      return
      end
*deck strskp
      subroutine strskp( line, lptr, chars )
c
c  skips characters in line starting at lptr
c  until a character not in chars is found
c
c  lptr = len(line) + 1 if all characters in line are in chars
c
c
      implicit integer(a-z)
      character*(*)     line
      integer           lptr
      character*(*)     chars
c
c
  100 continue
c
      if ( lptr .le. len(line)  .and.
     $     index( chars, line(lptr:lptr) ) .gt. 0 ) then
c
         lptr = lptr + 1
c
         goto 100
      endif
c
c
      return
      end
*deck strtil
      subroutine strtil( line, lptr, chars )
c
c  skips characters in line starting at lptr
c  until a character in chars is found
c
c  lptr = len(line) + 1 if no more characters in line are in chars
c
c
      implicit integer(a-z)
      character*(*)     line
      integer           lptr
      character*(*)     chars
c
c
  100 continue
c
      if ( lptr .le. len(line)  .and.
     $     index( chars, line(lptr:lptr) ) .eq. 0 ) then
c
         lptr = lptr + 1
c
         goto 100
      endif
c
c
      return
      end
*deck strtok
      subroutine strtok( line, lptr, token, delim, error )
c
c  parse a token delimited by characters in delim
c
      implicit integer(a-z)
      character*(*)     line
      integer           lptr
      character*(*)     token
      character*(*)     delim
      integer           error
c
      integer           sptr
c
      integer     noerr,     longtk,      linend
      parameter ( noerr = 0, longtk = -2, linend =1 )
c
      external         strskp, strtil
c
c
      error = noerr
c
      call strskp( line, lptr, delim )
      if ( lptr .gt. len(line) ) then
         error = linend
         return
      endif
c
      sptr = lptr
      call strtil( line, lptr, delim )
c
      if ( lptr - sptr .gt. len( token ) ) error = longtk
c
      token = line( sptr : min( lptr, sptr + len(token) ) - 1 )
c
      return
      end
*deck strupp
*deck allcap
      subroutine allcap( string )
c
c  change all lower case characters in string to upper
c
      character*(*)     string
c
c  the ascii and ebcdic versions exists, not to make the routine faster,
c  but to make it less case sensitive
c
      integer           istr
c
c     # strupp() and allcap() do the same thing. -rls
c
      entry strupp( string )
c
*mdc*if fortran
*c     # case-dependent standard fortran version.
*c     # warning: case dependent code
*      integer           indx
*c     # warning: case dependent code
*c     # vax quirk; upperc(:) cannot be a parameter constant (rls).
*      character*(*)     lower
*      character*26     upperc
*      parameter ( lower  = 'abcdefghijklmnopqrstuvwxyz' )
*      data        upperc / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
*c
*      do 100 istr = 1, len(string)
*         indx = index(lower, string(istr:istr) )
*         if ( indx .ne. 0 )
*     $        string(istr:istr) = upperc(indx:indx)
* 100  continue
*mdc*elseif ebcdic
*c     # ebcdic version.
*c     # this code assumes that ichar() returns ebcdic values.
*      integer           ich
*      integer           uldiff
*      parameter ( uldiff = 64 )
*      integer   sets
*      parameter ( sets = 3 )
*      integer   low   ( sets )
*      integer   high  ( sets )
*      data low  / 129, 145, 162 /
*      data high / 137, 153, 169 /
*c
*c
*      do 100 istr = 1, len(string)
*         ich = ichar( string(istr:istr) )
*         if ( ( low(1) .le. ich  .and.  ich .le. high(1) ) .or.
*     $        ( low(2) .le. ich  .and.  ich .le. high(2) ) .or.
*     $        ( low(3) .le. ich  .and.  ich .le. high(3) ) )
*     $        string(istr:istr) = char( ich + uldiff )
* 100  continue
*mdc*else
c     # ascii version.
c     # this code assumes that ichar() returns ascii values.
      integer           ich
      integer lowa, lowz, uldiff
      parameter ( lowa =  97 )
      parameter ( lowz = 122 )
      parameter ( uldiff = -32 )
c
      do 100 istr = 1, len(string)
         ich = ichar( string(istr:istr) )
         if ( lowa .le. ich  .and.  ich .le. lowz )
     $        string(istr:istr) = char( ich + uldiff )
 100  continue
*mdc*endif
      return
      end
*deck strwr
      subroutine strwr( unit, string )
c
c  write string to file -- strips trailing blanks.
c  note: fortran carriage control is *not* used by this routine.
c
      integer           unit
      character*(*)     string
c
      integer           istr
c
      external          strlen
      integer           strlen
c
c
      istr = strlen( string )
      if ( istr .gt. 0 ) then
         write ( unit, '(a)' ) string(1:istr)
      else
         write ( unit , '(a)' )
      endif
c
      return
      end
*deck unicas
      character*(*) function unicas ( string )
c
      character*(*) string
c
c  return string converted to site-dependent consistent case
c
c  14-dec-90 temp*150 removed. -rls
c
      unicas = string
*mdc*if osu
*      call strupp( unicas )
*mdc*else
      call strlow( unicas )
*mdc*endif
c
      return
      end
*deck ifchl
      integer function ifchl( line, chlist, match )
c
c  find the position of the first character in line(1:) that is
c  contained in chlist(1:).  match is set to the index of the
c  matching character in chlist(1:).
c
c  *** beware of side-effects with the argument match ***
c
c  written by ron shepard 1-oct-87.
c
      implicit integer(a-z)
c
      character*(*) line, chlist
      integer match
c
      integer i
c
      do 10 i = 1, len(line)
         ifchl = i
         match = index(chlist,line(i:i))
         if ( match .ne. 0 ) return
10    continue
      ifchl = 0
c
      return
      end
*deck ifnch
      integer function ifnch( line, chlist )
c
c  find the position of the first character in line(1:) that is
c  not contained in chlist(1:).
c
c  written by ron shepard 1-oct-87.
c
      implicit integer(a-z)
c
      character*(*) line, chlist
c
      integer i
c
      do 10 i = 1, len(line)
         ifnch = i
         if ( index( chlist, line(i:i) ) .eq. 0 ) return
10    continue
      ifnch = 0
c
      return
      end
*deck who2c
c *** this routine is incremental ***
      subroutine who2c( prognm, nlist )
c
c  print out a short banner containing the name and address of the
c  local programmer responsible for maintaining the calling program.
c
c  input:
c  prognm = left-justified character string containing the
c           name of the calling program.
c  nlist  = output unit number for the listing file.
c
c  14-feb-91 written by rls/eas/rmp.
c
      implicit integer(a-z)
c
      integer nlist
      character*(*) prognm
c
      logical  streq
      external streq
c
*mdc*if osu
*      integer i
*      integer    rmpnum
*      parameter( rmpnum=5 )
*      character*6 rmpprg(rmpnum)
*c     # the following codes are maintained at OSU by R. M. Pitzer:
*      data rmpprg / 'argos', 'cnvrt', 'scfpq', 'tran', 'cidbg' /
*mdc*endif
c
      write (nlist,6000) prognm
6000  format(/' This Version of Program ', a, ' is Maintained by:')
c
*mdc*if argonne
c     # alliant, sun, stardent titan, vax, cray y-mp, cray 2,
c     # intel hypercube, intel delta (in progress), ibm rs/6000
      write (nlist,6010)
6010  format(
     & t5,'Ron Shepard'/
     & t5,'Theoretical Chemistry Group'/
     & t5,'Chemistry Division'/
     & t5,'Argonne National Laboratory'/
     & t5,'Argonne, IL  60439'/
     & t5,'Internet: shepard@tcg.anl.gov'/)
c
*mdc*elseif wien
*c     # sun, ibm, convex, ibm 3090, ibm rs/6000, sgi,
*c     # intel delta (in progress)
*      write (nlist,6010)
*6010  format(
*     & t5,'Hans Lischka'/
*     & t5,'Institute for Theoretical Chemistry'/
*     & t5,'University of Vienna'/
*     & t5,'Waeringerstr 17, A-1090 Wien, Austria'/
*     & t5,'Internet: hans@itc.univie.ac.at'/)
*c
*mdc*elseif osu
*c     # sun, vax, ibm, cray y-mp, stardent gs
*      do 10 i = 1, rmpnum
*         if ( streq( prognm, rmpprg(i) ) ) then
*            write (nlist,6010)
*            return
*         endif
*10    continue
*c
*      write (nlist,6020)
*c
*6010  format(
*     & t5,'Russell M. Pitzer'/
*     & t5,'Department of Chemistry'/
*     & t5,'The Ohio State University'/
*     & t5,'Columbus, OH  43210'/
*     & t5,'Internet: pitzer@neon.mps.ohio-state.edu'/)
*6020  format(
*     & t5,'Gary Kedziora'/
*     & t5,'Department of Chemistry'/
*     & t5,'The Ohio State University'/
*     & t5,'Columbus, OH  43210'/
*     & t5,'Internet: kedziora@mps.ohio-state.edu'/)
*c
*mdc*elseif radlab
*c     # convex c-1
*      write (nlist,6010)
*6010  format(
*     & t5,'John Bentley'/
*     & t5,'Radiation Laboratory',/
*     & t5,'University of Notre Dame'/
*     & t5,'Notre Dame, IN  46556-0768'/
*     & t5,'Bitnet: bentley@ndradlab'/)
*c
*mdc*elseif anu
*c     # fujitsu vp2200
*      write (nlist,6010)
*6010  format(
*     & t5,'Ross Nobes'/
*     & t5,'ANU Supercomputer Facility',/
*     & t5,'GPO Box 4'/
*     & t5,'Canberra  ACT  2601'/
*     & t5,'Australia'/
*     & t5,'Internet: R.Nobes@anu.edu.au'/)
*c
*mdc*else
*c
*c     # this is just to keep everyone honest. (:-)  -rls
*c
*      write (nlist,6010)
*6010  format(
*     & t5,52('?')/
*     & t5,'???  Tell your local COLUMBUS programmer to add  ???',/
*     & t5,'???  his name to routine who2c() in colib*.f     ???'/
*     & t5,52('?')/)
*c
*mdc*endif
c
      return
      end
