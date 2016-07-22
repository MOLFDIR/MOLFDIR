*colib6.f
C $Id: colib6.f,v 1.1.1.1 2001/04/09 08:15:53 visscher Exp $
C $Log: colib6.f,v $
C Revision 1.1.1.1  2001/04/09 08:15:53  visscher
C Source and other files for MOLFDIR as distributed in 1998-2000.
C
C Revision 5.2.1.1  1997/07/02 11:50:06  calderon
C niko_version1
C
C Revision 5.2  1997/06/16 11:13:12  calderon
C COLUMBUS52_vudhi
C
C Revision 5.0  1996/06/26 12:26:17  calderon
C  Columbus 5.0
C
C Revision 1.1  1996/04/23 15:24:33  calderon
C Initial revision
C
*colib part=6 of 9.  logical expression evaluation routines.
*version=4.1 last modified: 13-mar-91
*version 5.0
c
c  RCS $Revision: 1.1.1.1 $  $Date: 2001/04/09 08:15:53 $
c
c  see colib1.f for version history info.
c
c  this file is used by cmdc.
c
*deck chkstk
      subroutine chkstk( reason, valsiz, valstk, valtop, opsiz, opstk,
     &     optop, errnum, errval )
c
c  apply operators in stack to values in stack, as long as appropriate
c
      character*(*)     reason
      integer           valsiz
      logical           valstk ( valsiz )
      integer           valtop
      integer           opsiz
      integer           opstk  ( opsiz )
      integer           optop
      integer           errnum
      integer           errval
c
      logical           error
c
      integer           op
      logical           val
      logical           val1
      logical           val2
c
      external          ipop
      external          ipush
      external          lpop
      external          lpush
c
      integer     offt,     ont
      parameter ( offt = 0, ont = 1 )
      integer     minopt,      ort,      andt,      nott
      parameter ( minopt = 10, ort = 11, andt = 12, nott = 13 )
      integer     opent,      closet,      endt
      parameter ( opent = 14, closet = 15, endt = 20 )
c
c  note:  it may appear that not all the necessary error checking is
c  done for the stack routines.  It turns out that the following is
c  actually sufficient to catch all errors
c
c  the push/pop routines are for clarity, not efficiency.
c  Unfortunately, I'm not certain they are worth the trouble
c
c
      errnum = 0
      errval = 0
c
c     # loop over operators until finished
c     # (all gone or lower precedence).
c
100   continue
c
      call ipop( opsiz, opstk, optop, op, error )
      if ( error ) then
c        # missing open parantheses.
         if ( reason .eq. 'clparn' ) errnum = 201
         return
      endif
c
      if ( op .eq. nott ) then
         call lpop( valsiz, valstk, valtop, val, error )
         if ( error ) then
c           missing value - not
            errnum = 202
            return
         endif
         val = .not. val
         call lpush( valsiz, valstk, valtop, val, error )
c
      elseif ( op .eq. andt ) then
         call lpop( valsiz, valstk, valtop, val1, error )
         call lpop( valsiz, valstk, valtop, val2, error )
         if ( error ) then
c           missing value - and
            errnum = 203
            return
         endif
         val = val1 .and. val2
         call lpush( valsiz, valstk, valtop, val, error )
c
      elseif ( op .eq. ort ) then
         if ( reason .eq. 'token' ) then
c           # will process at close paren or end of line.
            call ipush( opsiz, opstk, optop, op, error )
            return
         endif
         call lpop( valsiz, valstk, valtop, val1, error )
         call lpop( valsiz, valstk, valtop, val2, error )
         if ( error ) then
c           # missing value - "or".
            errnum = 204
            return
         endif
         val = val1 .or. val2
         call lpush( valsiz, valstk, valtop, val, error )
c
      elseif ( op .eq. opent ) then
         if ( reason .eq. 'token' ) then
c           # will process with matching close.
            call ipush( opsiz, opstk, optop, op, error )
            return
         elseif ( reason .eq. 'endln' ) then
c           # missing close paran.
            errnum = 205
            return
         elseif ( reason .eq. 'clparn' ) then
            return
         else
c           # unkown reason code (char num ).
            errnum = 206
            errval = ichar(reason)
            return
c
         endif
c
      else
c        # unkown operator.
         errnum = 207
         errval = op
         return
c
      endif
c
      goto 100
c
      end
*deck lexln.f
      subroutine lexln( line, nflags, flags, ntkary, tkary, errnum,
     &     errval )
c
c  returns an array with all the tokens from line properly coded
c
c
      character*(*)     line
      integer           nflags
      character*(*)     flags  ( nflags )
      integer           ntkary
      integer           tkary  ( ntkary )
      integer           errnum
      integer           errval
c
      integer           itkary
      integer           itoken
      integer           lptr
      integer           olptr
c                       previous lptr -- for error messages
      character*20      token
      integer           tkclas
c                       token class -- not necessary
c                       could be useful with future expansion
      integer           error
c
c     # errors from lextok.
c
      integer     noerr,     longtk
      parameter ( noerr = 0, longtk = -2 )
c
c     # token class from lextok.
c
      integer     endln,     char,     string
      parameter ( endln = 0, char = 1, string = 2 )
c
      integer     offt,     ont
      parameter ( offt = 0, ont = 1 )
      integer     minopt,      ort,      andt,      nott
      parameter ( minopt = 10, ort = 11, andt = 12, nott = 13 )
      integer     opent,      closet,      endt
      parameter ( opent = 14, closet = 15, endt = 20 )
c
c     # external functions.
c
      integer           strlen
      integer           strnds
      integer           strndx
c
c     # context free tokens.
c
      integer           nspcnm
      parameter         ( nspcnm = 11 )
      character*5       nspctk ( nspcnm )
      integer           nspcop ( nspcnm )
c
c     # space delimited tokesn.
c
      integer           spcnm
      parameter         ( spcnm = 3 )
      character*5       spctk ( spcnm )
      integer           spcop ( spcnm )
c
      data nspctk / '+', '.or.', '||', '*', '.and.', '&&', '-', '.not.',
     &     '!', '(', ')' /
      data nspcop /  ort,  ort,   ort, andt,  andt,  andt, nott,  nott,
     &     nott, opent, closet /
      data spctk / 'or', 'and', 'not' /
      data spcop /  ort,  andt,  nott /
c
      lptr   = 1
      itkary = 1
      errnum = 0
      errval = 0
c
100   continue
c
c     # skip white space.
      call strskp( line, lptr, ' ' )
c
      if ( (itkary .le. ntkary)  .and.  (lptr .le. len(line)) ) then
c
c        # check for context-free tokens first.
c
         itoken = strnds( nspcnm, line(lptr:), nspctk, 1 )
         if ( itoken .ne. 0 ) then
            if ( strlen(line(lptr:)) .lt. strlen(nspctk(itoken)) )
     &           goto 904
            tkary(itkary) = nspcop( itoken )
            lptr = lptr + strlen( nspctk(itoken) )
c
         else
c
            olptr = lptr
            call lextok( line, lptr, token, tkclas, error )
            if ( (tkclas .eq. string)  .and. (error .eq. noerr) ) then
c
c               # valid token found.
c
c              # first look for space delimited tokens.
c
               itoken = strndx( spcnm, token, spctk, 1 )
               if ( itoken .ne. 0 ) then
                  tkary(itkary) = spcop( itoken )
c
               else
c
c                 # is an option flag -- true or false depending on
c                 # whether token appears in flag array or not.
c
                  tkary(itkary) = offt
                  if ( strndx( nflags, token, flags, 1 ) .ne. 0 )
     &                 tkary(itkary) = ont
               endif
c
            else
c
c              # unexpected special character.
               if ( tkclas .eq. char ) goto 903
c
c              # token too long.
               if ( error .eq. longtk ) goto 902
c
c              # unexpected error from lextok.
               goto 905
c
            endif
         endif
c
         itkary = itkary + 1
c
         goto 100
      endif
c
c     # check for too many tokens.
      if ( itkary .gt. ntkary ) goto 901
c
c     # check for empty input.
      if ( itkary .le. 1 ) goto 906
c
      tkary(itkary) = endt
c
c
      return
c
 901  continue
c     # too many tokens.
      errnum = 301
      errval = ntkary
      return
c
 902  continue
c     # token too long.
      errnum = 302
      errval = olptr
      return
c
 903  continue
c     # unexpected special character.
      errnum = 303
      errval = olptr
      return
c
 904  continue
c     # only portion of string found -- bad error -- strnds messed up.
      errnum = 304
      errval = lptr
      return
c
 905  continue
c     # unexpected error from lextok.
      errnum = 305
      errval = olptr
      return
c
 906  continue
c     # empty logical expression.
      errnum = 306
      errval = 0
      return
c
      end
*deck lexp
      subroutine lexp( line, nflags, flags, value, errnum, errval )
c
c  evaluate logical expression in line -- flags are true
c
c
      character*(*)     line
      integer           nflags
      character*(*)     flags( nflags )
      logical           value
      integer           errnum
      integer           errval
c
      integer           ntoken
      parameter         ( ntoken = 40 )
      integer           token( ntoken)
c
c
      errnum = 0
      errval = 0
c
      call lexln( line, nflags, flags, ntoken, token, errnum, errval )
      if ( errnum .ne. 0 ) return
c
      call parstk( ntoken, token, value, errnum, errval )
c
      return
      end
*deck lexpe
      subroutine lexpe( option, errnum, errval )
c
c  provide convenient default error handling for lexp
c
      integer           option
c                   0 - never stop, 1 - stop for errors, 2 - always stop
      integer           errnum
      integer           errval
c
      integer           bumerr
      integer           bumwrn
c
c
c     # set up the bummer codes so the desired
c     # behavior will occur later.
c
      if ( option .eq. 0 ) then
         bumerr = 0
         bumwrn = 0
      elseif ( option .eq. 1 ) then
         bumerr = 2
         bumwrn = 0
      elseif ( option .eq. 2 ) then
         bumerr = 2
         bumwrn = 2
      else
         call bummer( 'lexp: 991 unkown option value', option, 2 )
      endif
c
c     # actually process the error numbers.
c
      if ( errnum .eq. 0 ) then
c        no error
         return
      elseif ( errnum .eq. 101 ) then
         call bummer('lexp: 101 parstk: stack overflow: token = ',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 102 ) then
         call bummer('lexp: 102 parstk: token after not must be ' //
     &        'flag or open (', errval, bumerr )
c
      elseif ( errnum .eq. 103 ) then
         call bummer('lexp: 103 parstk: undefined token -- ',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 104 ) then
         call bummer('lexp: 104 parstk: too many operators -- ',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 105 ) then
         call bummer('lexp: 105 parstk: too many values -- ',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 106 ) then
         call bummer('lexp: 106 parstk: too few values',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 107 ) then
         call bummer('lexp: 107 parstk: missing end token',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 201 ) then
         call bummer('lexp: 201 chkstk: missing open parantheses',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 202 ) then
         call bummer('lexp: 202 chkstk: missing value - not',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 203 ) then
         call bummer('lexp: 203 chkstk: missing value - and',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 204 ) then
         call bummer('lexp: 204 chkstk: missing value - or',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 205 ) then
         call bummer('lexp: 205 chkstk: missing close paran',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 206 ) then
         call bummer('lexp: 206 chkstk: unkown reason code (char num)',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 207 ) then
         call bummer('lexp: 207 chkstk: unkown operator --',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 301 ) then
         call bummer('lexp: 301 lexln: too many tokens. max - ',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 302 ) then
         call bummer('lexp: 302 lexln: token too long; column - ',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 303 ) then
         call bummer('lexp: 303 lexln: unexpected special char; col - ',
     &        errval, bumerr )
c
      elseif ( errnum .eq. 304 ) then
         call bummer('lexp: 304 lexln: only portion of string found; '//
     &        'col - ', errval, bumerr )
c
      elseif ( errnum .eq. 305 ) then
         call bummer('lexp: 305 lexln: unexpected error from lextok; '//
     &        'col - ', errval, bumerr )
c
      elseif ( errnum .eq. 306 ) then
         call bummer('lexp: 306 lexln: empty logical expression',
     &        errval, bumerr )
c
      else
         call bummer('lexp: 992 unkown error number', errnum, 2 )
      endif
c
      return
      end
*deck lextok.f
      subroutine lextok( line, lptr, token, tkclas, error )
c
c  returns next token in line.
c  -- skips white space
c  -- legal strings are consistent with leglic() and leglc().
c  -- all other characters -- token
c
c  14-dec-90  leglic() and leglc() used. -rls
c
      character*(*)     line
      integer           lptr
      character*(*)     token
      integer           tkclas
      integer           error
c
      integer     noerr,     longtk
      parameter ( noerr = 0, longtk = -2 )
c
      integer     endln,     char,     string
      parameter ( endln = 0, char = 1, string = 2 )
c
      integer           sptr
c
      logical  leglc, leglic
      character*1 chchar
c
      logical  isalph, isalnm
      external isalph, isalnm
c
c     # leglic() checks legal initial characters for cmdc variables.
      leglic( chchar ) = (chchar .eq. '$')
     & .or.              (chchar .eq. '_')
     & .or.              isalph( chchar )
c
c     # leglc() checks legal characters within cmdc variables.
      leglc( chchar ) = (chchar .eq. '$')
     & .or.             (chchar .eq. '_')
     & .or.             isalnm( chchar )
c
c     # assume no error.
      error = noerr
c
c     # skip white space.
      call strskp( line, lptr, ' ' )
c
c     # check for end of line.
      if ( lptr .gt. len(line) ) then
         token  = ' '
         tkclas = endln
         return
      endif
c
c     # check for an illegal initial char for a string;
c     # if found, assume special character and return for processing.
      if ( .not. leglic( line(lptr:lptr) ) ) then
         token  = line(lptr:lptr)
         lptr   = lptr + 1
         tkclas = char
         return
      endif
c
c     # check for legal string characters.
c
      sptr = lptr
100   continue
      if ( lptr .le. len(line) ) then
         if ( leglc( line(lptr:lptr) ) ) then
            lptr = lptr + 1
            goto 100
         endif
      endif
c
      if ( (lptr - sptr + 1) .gt. len(token) ) error = longtk
c
      token = line(sptr:lptr-1)
      tkclas = string
c
      return
      end
*deck parstk
      subroutine parstk( ntoken, token, value, errnum, errval )
c
c  parse the list of tokens, returning the value of
c  the logical expression.
c
c  13-mar-91 implied ".or." bugs fixed involving ()-delimited
c            expressions. -rls
c
      implicit integer(a-z)
c
      integer           ntoken
      integer           token  ( ntoken )
      logical           value
      integer           errnum
      integer           errval
c
      integer           stksiz
      parameter         ( stksiz = 20 )
      logical           valstk ( stksiz )
      integer           valtop
      integer           opstk  ( stksiz )
      integer           optop
c
      integer     offt,     ont
      parameter ( offt = 0, ont = 1 )
      integer     minopt,      ort,      andt,      nott
      parameter ( minopt = 10, ort = 11, andt = 12, nott = 13 )
      integer     opent,      closet,      endt
      parameter ( opent = 14, closet = 15, endt = 20 )
c
      integer           ctoken
c                       current token value
      logical           error
      integer           idummy
      integer           itoken
      logical           ldummy
c
      external          ipop
      external          ipush
      external          lpop
      external          lpush
c
c     # initialize stacks to empty.
c
      optop  = 0
      valtop = 0
      errnum = 0
      errval = 0
c
      itoken = 1
100   continue
      if ( (itoken .le. ntoken) .and. (token(itoken) .ne. endt) ) then
c
         ctoken = token( itoken )
c
         if (  (ctoken .eq. opent) ) then
            if ( itoken .gt. 1) then
               if (  (token(itoken-1) .le. minopt)
     &          .or. (token(itoken-1) .eq. closet) ) then
c                 # push implied ".or." operator:
c                 # "flag ("           ==>   "flag .or. ("
c                 # "(expression) ("   ==>   "(expression) .or. ("
                  call ipush( stksiz, opstk, optop, ort, error )
                  if ( error ) goto 910
               endif
            endif
            call ipush( stksiz, opstk, optop, ctoken, error )
            if ( error ) goto 910
c
         elseif (  (ctoken .eq. andt) .or. (ctoken .eq. ort) ) then
c
            call ipush( stksiz, opstk, optop, ctoken, error )
            if ( error ) goto 910
c
         elseif ( ctoken .eq. nott ) then
c
c           # next token must be open or value.
c
            if ( (itoken .ge. ntoken)  .or.
     &       .not.( (token(itoken+1) .eq. opent )
     &       .or.   (token(itoken+1) .lt. minopt) ) ) then
c              # token after .not. must be flag or open (
               errnum = 102
               return
            endif
            call ipush( stksiz, opstk, optop, ctoken, error )
            if ( error ) goto 910
c
         elseif ( ctoken .eq. closet ) then
c
            call chkstk( 'clparn', stksiz, valstk, valtop, stksiz,
     &           opstk, optop, errnum, errval )
            if ( errnum .ne. 0 ) return
            call chkstk( 'token', stksiz, valstk, valtop, stksiz,
     &           opstk, optop, errnum, errval )
            if ( errnum .ne. 0 ) return
c
         elseif ( ctoken .eq. offt  .or.  ctoken .eq. ont ) then
c
            if ( itoken .gt. 1) then
               if (  (token(itoken-1) .le. minopt)
     &          .or. (token(itoken-1) .eq. closet) ) then
c                 # push implied ".or." operator:
c                 # "flag flag"           ==>   "flag .or. flag"
c                 # "(expression) flag"   ==>   "(expression) .or. flag"
                  call ipush( stksiz, opstk, optop, ort, error )
                  if ( error ) goto 910
               endif
            endif
c
            call lpush( stksiz, valstk, valtop, (ctoken.eq.ont), error )
            if ( error ) goto 910
            call chkstk( 'token', stksiz, valstk, valtop, stksiz, opstk,
     &           optop, errnum, errval )
            if ( errnum .ne. 0 ) return
c
         else
c
c           # undefined token.
            errnum = 103
            errval = ctoken
            return
         endif
c
         itoken = itoken + 1
         goto 100
      endif
c
      if ( itoken .gt. ntoken ) then
c        # undefined token.
         errnum = 107
         return
      endif
c
      call chkstk( 'endln', stksiz, valstk, valtop, stksiz, opstk,
     &     optop, errnum, errval )
      if ( errnum .ne. 0 ) return
c
      call lpop( stksiz, valstk, valtop, value, error )
      if ( error ) then
c        # too few values.
         errnum = 106
         return
      endif
c
c     # make sure both stacks are empty.
      call ipop( stksiz, opstk, optop, idummy, error )
      if ( .not. error ) then
c        # too many operators.
         errnum = 104
         errval = idummy
         return
      endif
      call lpop( stksiz, valstk, valtop, ldummy, error )
      if ( .not. error ) then
c        # too many values.
         errnum = 105
         if ( ldummy ) errval = 1
         return
      endif
c
      return
c
c     # error conditions.
910   continue
c     # stack overflow.
      errnum = 101
      errval = itoken
      return
c
      end
*deck ipop
      subroutine ipop( size, stack, top, value, error )
c
c      pop integer values from a stack
c
      integer           size
      integer           stack  ( size)
      integer           top
      integer           value
      logical           error
c
c
      error = .true.
c
      if ( top .le. 0 ) return
c
      value = stack(top)
      top = top - 1
      error = .false.
c
      return
      end
*deck ipush
      subroutine ipush( size, stack, top, value, error )
c
c      push integer values onto a stack
c
      integer           size
      integer           stack  ( size)
      integer           top
      integer           value
      logical           error
c
c
      error = .true.
c
      if ( top .ge. size ) return
c
      top = top + 1
      stack(top) = value
      error = .false.
c
      return
      end
*deck lpop
      subroutine lpop( size, stack, top, value, error )
c
c      pop logical values from a stack
c
      integer           size
      logical           stack  ( size)
      integer           top
      logical           value
      logical           error
c
c
      error = .true.
c
      if ( top .le. 0 ) return
c
      value = stack(top)
      top = top - 1
      error = .false.
c
      return
      end
*deck lpush
      subroutine lpush( size, stack, top, value, error )
c
c      push logical values onto a stack
c
      integer           size
      logical           stack  ( size)
      integer           top
      logical           value
      logical           error
c
c
      error = .true.
c
      if ( top .ge. size ) return
c
      top = top + 1
      stack(top) = value
      error = .false.
c
      return
      end
