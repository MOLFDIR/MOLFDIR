*colib2.f
*colib part=2 of 9.  io interface routines.
*version=4.1 last modified: 21-dec-90
c
c  RCS $Revision: 1.3 $  $Date: 2001/09/07 02:25:11 $
c
c  see colib1.f for version history info.
c
*deck airead
      subroutine airead( unit, buffer, buflen )
c
c----------------------------------------------------------
c this is a primitive i/o routine to be used by all higher
c level routines requiring i/o of this type. machine i/o
c peculiarites are meant to reside in these routines only.
c----------------------------------------------------------
c
c routine name: airead
c version: 1.0                        date: 10/13/88
c author: eric stahlberg - ohio state university
c purpose: this routine will read a record from an asynchronous file
c          buffer length is in real*8 words.
c          if an error occurs, bummer is called with an
c          encoded error number: ioerr*100+unit
c parameters:
c     unit:    unit number for file to be opened
c     buffer:  buffer to read record into
c     buflen:  length of buffer to be read
c
      implicit integer(a-z)
c
      integer unit,buflen
      real*8 buffer(buflen)
c
c     # bummer error types
      integer wrnerr, nfterr, faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
c
      integer ioerr
c
*mdc*if debug
*      print *, 'airead: unit=, buflen=',unit,buflen
*mdc*endif
*mdc*if reference
*c      asynchronous i/o for this case is not implmented because
*c      calling program calls in wrong order. should be fixed
*c      when calling programs are rewritten.
*      read (unit=unit,iostat=ioerr,err=900,wait=.false.) buffer
*mdc*else
c
c read regular file
c
      read (unit=unit,iostat=ioerr,err=900) buffer
*mdc*endif
c
*mdc*if (debug .and. osu)
*      print *, (buffer(i), i=1, min(100,buflen) )
*mdc*endif
      return
c
 900  continue
      call bummer('i/o error in airead, (ioerr*100+unit)=',
     & (ioerr*100+unit), faterr )
      end
*deck closda
      subroutine closda( unit, delflg, parm )
c
c----------------------------------------------------------
c this is a primitive i/o routine to be used by all higher
c level routines requiring i/o of this type. machine i/o
c peculiarites are meant to reside in these routines only.
c----------------------------------------------------------
c
c routine name: closda
c version: 1.2                        date: 12/17/90
c author: eric stahlberg - ohio state university
c purpose: this routine will close a direct access file. the
c          file may be deleted if delflg is set to 'delete'.
c          vector
c paramters:
c     unit: unit number of file to close
c     delflg: flag to indicate deletion of file upon close
c     parm: value employed by certain machines in closing
c
      implicit integer(a-z)
c
      integer unit, parm
      character*(*) delflg
c
c common block for unit information
      integer         mxunit
      parameter       (mxunit=99)
c recnum retains next record to be written
c reclen retains record length in open
c seqwrt retains true if sequential write is to be enforced
      integer         recnum(mxunit)
      integer         reclen(mxunit)
      logical         seqwrt(mxunit)
      common  /esio88/   recnum, reclen, seqwrt
      save    /esio88/
c
c     # bummer error types
      integer wrnerr, nfterr, faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
c
      logical  streq
      external streq
c
c
c perform error checking
c
*mdc*if debug
*      print *, 'closda: unit=, delflg=, parm=',unit,delflg,' ',parm
*mdc*endif
      if (unit.gt.mxunit.or.unit.le.0) goto 910
c
c no error detection is done on the close. an error in this routine
c will give more information from a system abend.
c
*mdc*if ibmcms
*      call uclose(unit,parm,ierr)
*c     parm = 2  close and retain
*c            4  close and delete
*      if (ierr.ne.0) then
*        call bummer('closda: error in uclose, (ierr*100+unit)=',
*     +  (ierr*100+unit), faterr )
*      endif
*mdc*else
      if ( streq( delflg, 'delete' ) ) then
        close (unit=unit,status='delete')
      else
        close (unit=unit)
      endif
*mdc*endif
      return
 910  continue
      call bummer('closda: invalid unit', unit, nfterr)
      return
      end
*deck writda
      subroutine writda( unit, record, buffer, buflen )
c
c----------------------------------------------------------
c this is a primitive i/o routine to be used by all higher
c level routines requiring i/o of this type. machine i/o
c peculiarites are meant to reside in these routines only.
c----------------------------------------------------------
c
c routine name: writda
c version: 1.2                        date: 12/17/90
c author: eric stahlberg - ohio state university
c purpose: this routine will write a given vector to a
c          direct access file. the length of the vector is given
c          in minimum necessary real*8 words to access the entire
c          vector. io errors reported in encoded form: 100*ioerr+unit
c parameters:
c     unit:  unit number to write to
c     record: record number to be written
c     buffer: buffer to be written on record
c     buflen: length of buffer in real*8 words
c
      implicit integer(a-z)
c
      integer unit,record,buflen
      real*8 buffer ( buflen )
*mdc*if ibmmvs
*c these variables help in the logical record to physical
*c record reduction
*c     index*:  loop counter
*c     nphyrc:  number of complete physical records
*c     ibmlen:  real*8 words in 1/2 track of 3380 device
*c     phyrec:  physical record buffer
*c     phyrcn:  physical record number for this logical record
*      integer index,index2,nphyrc,ibmlen,phyrcn
*      parameter (ibmlen=2932)
*      real*8 phyrec (ibmlen)
*mdc*endif
c
c common block for unit information
      integer         mxunit
      parameter       (mxunit=99)
c recnum retains next record to be accessed
c reclen retains record length in open
c seqwrt retains true if sequential write is to be enforced
      integer         recnum(mxunit)
      integer         reclen(mxunit)
      logical         seqwrt(mxunit)
      common  /esio88/   recnum, reclen, seqwrt
      save    /esio88/
c
c     # bummer error types
      integer wrnerr, nfterr, faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
c
      integer ioerr
c
*mdc*if debug
*      print*,'writda: unit=,record=,buflen=',unit,record,buflen
*mdc*endif
*mdc*if (debug .and. osu)
*      print *, (buffer(i), i=1, min(100,buflen) )
*mdc*endif
c
c perform error checking
c
      if (unit.gt.mxunit) goto 910
      if (buflen.ne.reclen(unit)) goto 920
      if ((record.ne.recnum(unit)).and.(seqwrt(unit))) goto 930
c
*mdc*if ibmmvs
*      if (reclen(unit).gt.ibmlen) then
*c break da logical record into smaller pieces
*c number of complete physical records in the logical record
*         nphyrc=buflen/ibmlen
*c point to first physical record for this logical record
*         if (nphyrc*ibmlen.eq.buflen) then
*            phyrcn=(record-1)*nphyrc+1
*         else
*            phyrcn=(record-1)*(nphyrc+1)+1
*         endif
*c transfer complete records
*         do 10 index=1,nphyrc
*c
*c copy data to physical record
*c
*            do 20 index2=1,ibmlen
* 20           phyrec(index2)=buffer((index-1)*ibmlen+index2)
*c
*c write the physical record
*c
*         if (seqwrt(unit)) then
*           write (unit=unit,iostat=ioerr,err=900) phyrec
*         else
*           write (unit, rec = phyrcn+index-1,
*     +     iostat=ioerr, err=900 )phyrec
*         endif
*  10  continue
*c
*c check for remainder of record
*c
*         if (nphyrc*ibmlen.ne.buflen) then
*            do 30 index2=1,ibmlen
* 30           phyrec(index2)=buffer(buflen-ibmlen+index2)
*            if (seqwrt(unit)) then
*              write (unit=unit,iostat=ioerr,err=900) phyrec
*            else
*              write (unit, rec = phyrcn+nphyrc,
*     +        iostat=ioerr, err=900 )phyrec
*            endif
*         endif
*       else
*         if (seqwrt(unit)) then
*           write(unit,iostat=ioerr, err=900) buffer
*         else
*           write (unit,rec=record,
*     +       iostat=ioerr,err=900) buffer
*         endif
*       endif
*mdc*elseif ibmcms
*c ibmcms quirk: length of data xferred counted in 4 byte words
*c
*      len2=2*buflen
*      ipos=(record-1)*len2+1
*      call rwfil (unit, 2, ipos, buffer, len2, ierr)
*      if (ierr.ne.0) then
*        call bummer('writda: from rwfil, (ierr*100+unit)=',
*     +  (ierr*100+unit), faterr )
*      endif
*mdc*else
      write (unit, rec = record, iostat=ioerr, err=900 ) buffer
*mdc*endif
c
c point to next record
c for sequential writes, this will increment the count in recnum
c for random writes, this will point 1 record beyond that written
c
      recnum(unit)=record+1
c
      return
 899  format(' current reclen=',i10/
     & ' opened reclen=',i10/' recnum=',i10)
c
c error conditions
 900  continue
      write (*,899) reclen(unit),buflen,record
      call bummer('writda: i/o error, (ioerr*100+unit)=',
     & (ioerr*100+unit),faterr)
 910  continue
      call bummer('writda: invalid unit number ',unit,faterr)
 920  continue
      write (*,899) reclen(unit),buflen,record
      call bummer('writda: buffer length ne record length ',unit,faterr)
 930  continue
      write (*,899) reclen(unit),buflen,record
      call bummer('writda: records out of order ',unit,faterr)
      end
*deck aiclos
      subroutine aiclos( unit )
c
c----------------------------------------------------------
c this is a primitive i/o routine to be used by all higher
c level routines requiring i/o of this type. machine i/o
c peculiarites are meant to reside in these routines only.
c----------------------------------------------------------
c
c routine name: aiclos
c version: 1.0                        date: 10/13/88
c author: eric stahlberg - ohio state university
c purpose: this routine will close a file used for asynchronous i/o.
c
c parameters:
c     unit:    unit number for file to be opened
c
      implicit integer(a-z)
c
      integer unit
c
c     # bummer error types
      integer wrnerr, nfterr, faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
*mdc*if debug
*      print *, 'aiclos: unit=',unit
*mdc*endif
c
c close a regular file
c
      close(unit=unit)
c
      return
      end
*deck aiwait
      subroutine aiwait( unit )
c
c----------------------------------------------------------
c this is a primitive i/o routine to be used by all higher
c level routines requiring i/o of this type. machine i/o
c peculiarites are meant to reside in these routines only.
c----------------------------------------------------------
c
c routine name: aiwait
c version: 1.0                        date: 10/13/88
c author: eric stahlberg - ohio state university
c purpose: this routine will wait for an asynchronous file
c
c parameters:
c     unit:    unit number for file to be opened
c
      implicit integer(a-z)
c
      integer unit,ioerr
c
c     # bummer error types
      integer wrnerr, nfterr, faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
*mdc*if debug
*      print *, 'aiwait: unit=',unit
*mdc*endif
c
c
*mdc*if reference
*      wait(unit,iostat=ioerr)
*      if (ierr.ne.0) goto 900
*mdc*endif
      return
c
 900  call bummer('aiwait: i/o error, (ioerr*100+unit)=',
     & (ioerr*100+unit), faterr )
      end
*deck openda
      subroutine openda( unit, filnam, length, scrtch, seqflg )
c
c----------------------------------------------------------
c this is a primitive i/o routine to be used by all higher
c level routines requiring i/o of this type. machine i/o
c peculiarites are meant to reside in these routines only.
c----------------------------------------------------------
c
c routine name: openda
c version: 1.2                        date: 12/17/90
c author: eric stahlberg - ohio state university
c purpose: this routine will open a direct access file. if seqflg
c          is set to 'seqwrt' , then sequential writing of da
c          records will be enforced. record length is in real*8
c          words. if an error occurs, bummer is called with an
c          encoded error number: ioerr*100+unit
c parameters:
c     filnam: external file name to associate with unit
c     unit:    unit number for file to be opened
c     length:  length of direct access records in real*8 words
c     seqflg:  flag to indicate sequential write of records to
c              be enforced. enforcement if seqflg='seqwrt'
c     scrtch: flag for scratch files. unit to be opened is scratch
c              if scrtch='scratch'
c
      implicit integer(a-z)
c
      integer unit,length
      character*(*) filnam,seqflg,scrtch
*mdc*if ibmcms
*      character*80  text
*mdc*endif
c
c common block for unit information
      integer         mxunit
      parameter       (mxunit=99)
c recnum retains next record to be accessed
c reclen retains record length in open
c seqwrt retains true if sequential write is to be enforced
      integer         recnum(mxunit)
      integer         reclen(mxunit)
      logical         seqwrt(mxunit)
      common  /esio88/   recnum, reclen, seqwrt
      save    /esio88/
c
*mdc*if ibmmvs
*      integer         ibmlen
*      parameter (ibmlen=2932)
*mdc*endif
c     # bummer error types
      integer wrnerr, nfterr, faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
c
      integer ioerr
      integer lrecl
      external streq
      logical  streq
c
*mdc*if debug
*      print *, 'openda: unit=,filnam=,length=,seqflg=,scrtch=',
*     + unit,filnam,' ',length,seqflg,' ',scrtch
*mdc*endif
c
c perform error checking and info assignments
       if (unit.gt.mxunit.or.unit.lt.0) goto 910
       if ( streq( seqflg, 'seqwrt' ) ) then
          seqwrt(unit)=.true.
       else
          seqwrt(unit)=.false.
       endif
       reclen(unit)=length
       recnum(unit)=1
c
       if ( .not. streq( scrtch, 'scratch' ) ) then
c
c open regular file
c
*mdc*if vax titan
*      lrecl=2*length
*      open(unit=unit,access='direct',form='unformatted',status=
*     +  'unknown',file=filnam,iostat=ioerr,recl=lrecl,err=900)
*mdc*elseif ibmmvs
*      if (seqwrt(unit)) then
*        open(unit=unit,access='sequential', form='unformatted',
*     +  file=filnam,iostat=ioerr,err=900,status='unknown')
*      else
*        if (ibmlen.lt.reclen(unit)) then
*          lrecl=8*ibmlen
*        else
*          lrecl=8*reclen(unit)
*        endif
*        open(unit=unit,file=filnam,status='unknown',access='direct',
*     +  form='unformatted',recl=lrecl,err=900,iostat=ioerr)
*      endif
*mdc*elseif ibmcms
*c  warning *** case dependent code ***
*      TEXT='FILE '//FILNAM
*      CALL UOPEN(UNIT,TEXT,5,3,4096,4096,'FB',1,IOERR)
*      if(ioerr.ne.0) goto 900
*mdc*else
      lrecl=8*length
      open(unit=unit,access='direct',form='unformatted',status=
     +  'unknown',file=filnam,iostat=ioerr,recl=lrecl,err=900)
*mdc*endif
c
      else
c
c open scratch files here
c
*mdc*if ibmmvs
*      call bummer('openda: variable length da records not supported'
*     + ,0,wrnerr)
*      print *, 'openda: consult osu for remedies'
*      if (seqwrt(unit)) then
*        open(unit=unit,access='sequential', form='unformatted',
*     +  iostat=ioerr,err=900,status='scratch')
*      else
*        if (ibmlen.lt.reclen(unit)) then
*          lrecl=8*ibmlen
*        else
*          lrecl=8*reclen(unit)
*        endif
*        open(unit=unit,status='scratch',access='direct',
*     +  form='unformatted',recl=lrecl,err=900,iostat=ioerr)
*      endif
*mdc*elseif vax titan
*c     # must use a filename to keep files in the correct directory
*      lrecl=2*length
*      open(unit=unit,access='direct',form='unformatted',status=
*     +  'unknown',file=filnam,iostat=ioerr,recl=lrecl,err=900)
*mdc*elseif ibmcms
*c  warning *** case dependent code ***
*      TEXT='FILE '//FILNAM//' G'
*      CALL UOPEN(UNIT,TEXT,5,3,4096,4096,'FB',1,IOERR)
*      if(ioerr.ne.0) goto 900
*mdc*elseif unix sun cray alliant fps iris convex
c     # must use a filename to keep files in the correct directory.
      lrecl=8*length
      open( unit=unit, access='direct', form='unformatted', status=
     & 'unknown', file=filnam, iostat=ioerr, recl=lrecl, err=900 )
*mdc*else
*c     # if there are no machines that actually use this block, then
*c     # this block and the previous one should switch places. -rls
*      lrecl=8*length
*      open(unit=unit,access='direct',form='unformatted',status=
*     +  'scratch',iostat=ioerr,recl=lrecl,err=900)
*mdc*endif
      endif
      return
c
 900  continue
      call bummer('openda: (ioerr*100+unit)=',(ioerr*100+unit),faterr)
 910  continue
      call bummer('openda: bad unit number',unit,faterr)
      end
*deck seqrbf
      subroutine seqrbf( unit, buffer, buflen )
c
c----------------------------------------------------------
c this is a primitive i/o routine to be used by all higher
c level routines requiring i/o of this type. machine i/o
c peculiarites are meant to reside in these routines only.
c----------------------------------------------------------
c
c routine name: seqrbf
c version: 1.0                        date: 8/24/88
c author: eric stahlberg - ohio state university
c purpose: this routine will read a given vector from a
c          sequential file. the length of the vector is given
c          in minimum necessary real*8 words to access the entire
c          vector
c parameters:
c     unit:   unit number to read from
c     buffer: buffer to transfer information to
c     buflen: number of real*8 words to read from file
c
      implicit integer(a-z)
c
      integer unit, buflen
      real*8 buffer ( buflen )
c
*mdc*if debug
*      print *,'seqrbf: unit=, buflen=',unit,buflen
*mdc*endif
c
      read (unit) buffer
c
*mdc*if ( debug .and. osu)
*      print *, (buffer(i), i=1, min(100,buflen) )
*mdc*endif
      return
      end
*deck aiopen
      subroutine aiopen( unit, filnam, length )
c
c----------------------------------------------------------
c this is a primitive i/o routine to be used by all higher
c level routines requiring i/o of this type. machine i/o
c peculiarites are meant to reside in these routines only.
c----------------------------------------------------------
c
c routine name: aiopen
c version: 1.0                        date: 10/13/88
c author: eric stahlberg - ohio state university
c purpose: this routine will open a file for asynchronous i/o.
c          record length is in real*8 words for machines
c          which require this value.
c          if an error occurs, bummer is called with an
c          encoded error number: ioerr*100+unit
c parameters:
c     filnam: external file name to associate with unit
c     unit:    unit number for file to be opened
c     length:  length of direct access records in real*8 words
c
      implicit integer(a-z)
c
      integer unit,length
      character*(*) filnam
c
c     # bummer error types
      integer wrnerr, nfterr, faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
c
      integer ioerr
c
*mdc*if debug
*      print *, 'aiopen: unit=,filnam=,length=',
*     + unit,filnam,' ',length
*mdc*endif
*mdc*if reference
*c      asynchronous i/o for this case is not implmented because
*c      calling program calls in wrong order. should be fixed
*c      when calling programs are rewritten.
*      open(unit=unit,form='unformatted',status='unknown',
*     + sync='asynchronous',file=filnam,iostat=ioerr,err=900)
*mdc*else
c
c open regular file
c
      open(unit=unit,access='sequential',form='unformatted',status=
     +  'unknown',file=filnam,iostat=ioerr,err=900)
*mdc*endif
      return
c
 900  continue
      call bummer('i/o error in openda, (ioerr*100+unit)=',
     & (ioerr*100+unit), faterr )
      end
*deck aiwrit
      subroutine aiwrit( unit, buffer, buflen )
c
c----------------------------------------------------------
c this is a primitive i/o routine to be used by all higher
c level routines requiring i/o of this type. machine i/o
c peculiarites are meant to reside in these routines only.
c----------------------------------------------------------
c
c routine name: aiwrit
c version: 1.0                        date: 10/13/88
c author: eric stahlberg - ohio state university
c purpose: this routine will write a record to an asynchronous file
c          buffer length is in real*8 words.
c          if an error occurs, bummer is called with an
c          encoded error number: ioerr*100+unit
c parameters:
c     unit:    unit number for file to be opened
c     buffer:  buffer to read record into
c     buflen:  length of buffer to be read
c
      implicit integer(a-z)
c
      integer unit,buflen
      real*8 buffer(buflen)
c
c     # bummer error types
      integer wrnerr, nfterr, faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
c
      integer ioerr
c
*mdc*if debug
*      print *, 'aiwrit: unit=, buflen=',unit,buflen
*mdc*endif
*mdc*if ( debug .and. osu)
*      print *, (buffer(i), i=1, min(100,buflen) )
*mdc*endif
*mdc*if  reference
*c      asynchronous i/o for this case is not implmented because
*c      calling program calls in wrong order. should be fixed
*c      when calling programs are rewritten.
*      write (unit=unit,iostat=ioerr,err=900,wait=.false.) buffer
*mdc*else
c
c write regular file
c
      write (unit=unit,iostat=ioerr,err=900) buffer
*mdc*endif
      return
c
 900  continue
      call bummer('i/o error in airead, (ioerr*100+unit)=',
     & (ioerr*100+unit), faterr )
      end
*deck readda
      subroutine readda( unit, record, buffer, buflen )
c
c----------------------------------------------------------
c this is a primitive i/o routine to be used by all higher
c level routines requiring i/o of this type. machine i/o
c peculiarites are meant to reside in these routines only.
c----------------------------------------------------------
c
c routine name: readda
c version: 1.2                        date: 12/17/90
c author: eric stahlberg - ohio state university
c purpose: this routine will read a given vector from a
c          direct access file. the length of the vector is given
c          in minimum necessary real*8 words to access the entire
c          vector. io errors reported in encoded form: ioerr*100+unit
c parameters:
c     unit:   unit number of file to be read
c     record: record number to be read
c     buffer: buffer to read record into
c     buflen: number of real*8 words to read from record
c
      implicit integer(a-z)
c
      integer unit,record,buflen
      real*8 buffer ( buflen )
*mdc*if ibmmvs
*c these variables help in the logical record to physical
*c record reduction
*c     index*:  loop counter
*c     nphyrc:  number of complete physical records
*c     ibmlen:  real*8 words in 1/2 track of 3380 device
*c     phyrec:  physical record buffer
*c     phyrcn:  physical record number for this logical record
*      integer index,index2,nphyrc,ibmlen,phyrcn
*      parameter (ibmlen=2932)
*      real*8 phyrec (ibmlen)
*mdc*endif
c
c common block for unit information
      integer         mxunit
      parameter       (mxunit=99)
c recnum retains next record to be accessed
c reclen retains record length in open
c seqwrt retains true if sequential write is to be enforced
      integer         recnum(mxunit)
      integer         reclen(mxunit)
      logical         seqwrt(mxunit)
      common  /esio88/   recnum, reclen, seqwrt
      save    /esio88/
c
c     # bummer error types
      integer wrnerr, nfterr, faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
c
      integer ioerr
c
*mdc*if debug
*      print*,'readda: unit=,record=,buflen=',unit,record,buflen
*mdc*endif
c
c
c perform error checking
c
      if (unit.gt.mxunit.or.unit.le.0) goto 910
      if (buflen.ne.reclen(unit)) goto 920
      if (seqwrt(unit)) goto 930
*mdc*if ibmmvs
*       if (reclen(unit).gt.ibmlen) then
*c recreate da logical record from smaller physical records
*c number of complete physical records in the logical record
*         nphyrc=buflen/ibmlen
*c point to first physical record for this logical record
*         if (nphyrc*ibmlen.eq.buflen) then
*            phyrcn=(record-1)*nphyrc+1
*         else
*            phyrcn=(record-1)*(nphyrc+1)+1
*         endif
*c read complete records
*         do 10 index=1,nphyrc
*c
*c read the physical record
*c
*         read (unit, rec = phyrcn+index-1, iostat=ioerr, err=900 )
*     +            phyrec
*c
*c copy physical record to buffer
*c
*            do 20 index2=1,ibmlen
* 20           buffer((index-1)*ibmlen+index2)=phyrec(index2)
* 10     continue
*c
*c check for remainder of record
*c
*         if (nphyrc*ibmlen.ne.buflen) then
*            read (unit,rec=phyrcn+nphyrc,
*     +         iostat=ioerr, err=900 ) phyrec
*            do 30 index2=1,ibmlen
* 30           buffer(buflen-ibmlen+index2)=phyrec(index2)
*         endif
*       else
*         read (unit,rec=record) buffer
*       endif
*mdc*elseif ibmcms
*c ibmcms quirk: length of data xferred counted in 4 byte words
*c
*      len2=2*buflen
*      ipos=(record-1)*len2+1
*      call rwfil (unit, 1, ipos, buffer, len2, ierr)
*      if (ierr.ne.0) then
*        call bummer('readda: from rwfil, (ierr*100+unit)=',
*     +  (ierr*100+unit),faterr)
*      endif
*mdc*else
      read (unit, rec = record ,iostat=ioerr, err=900) buffer
*mdc*endif
c point to next record
      recnum(unit)=record+1
c
*mdc*if ( debug .and. osu)
*      print *, (buffer(i), i=1, min(100,buflen) )
*mdc*endif
      return
 899  format(' current reclen=',i10/
     & ' opened reclen=',i10/' recnum=',i10)
c
c error conditions
 900  continue
      write(*,899) reclen(unit),buflen,record
      call bummer('readda: i/o error,  (ioerr*100+unit)=',
     & (ioerr*100+unit), faterr )
 910  continue
      call bummer('readda: invalid unit number ',unit,faterr)
 920  continue
      write(*,899) reclen(unit),buflen,record
      call bummer('readda: buffer length gt record length ',unit,faterr)
 930  continue
      write(*,899) reclen(unit),buflen,record
      call bummer('readda: unit opened for seq. write',unit,faterr)
      end
*deck seqwbf
      subroutine seqwbf( unit, buffer, buflen )
c
c----------------------------------------------------------
c this is a primitive i/o routine to be used by all higher
c level routines requiring i/o of this type. machine i/o
c peculiarites are meant to reside in these routines only.
c----------------------------------------------------------
c
c routine name: seqwbf
c version: 1.0                        date: 8/24/88
c author: eric stahlberg - ohio state university
c purpose: this routine will write a given vector to a
c          sequential file. the length of the vector is given
c          in minimum necessary real*8 words to access the entire
c          vector
c parameters:
c     unit:   unit number to be written
c     buffer: buffer to be written
c     buflen: number of real*8 words to write to file
c
      implicit integer(a-z)
c
      integer unit, buflen
      real*8 buffer ( buflen )
c
*mdc*if debug
*      print *,'seqwbf: unit=, buflen=',unit,buflen
*mdc*endif
*mdc*if (debug .and. osu)
*      print *, (buffer(i),i=1, min(100,buflen) )
*mdc*endif
c
      write (unit) buffer
      return
      end
*deck readvc
      subroutine readvc(
     & unit,   reclen, vector, start,
     & length, logrec, loglen, rcode )
c
c     readvc
c     version 1.2               date: 12:17:90
c     eric stahlberg - ohio state university
c
c
c     subroutine to read in a segment from a da file
c
c     unit : unit number to read from
c     vector: vector to return read information in
c     start  : starting word of data
c     length : length of data to read
c     loglen : length of a logical record
c     logrec : logical record number
c
c     rcode  : list of return codes from routine
c
      implicit integer(a-z)
c
      integer unit,length,logrec,loglen,reclen,start
      integer rcode(4)
      real*8 vector(*)
c
      integer vecpos, first1, last1, first, last, dafrst, dalast
c
c     common block for direct access buffer
c     should be tuned to optimal values for particular machines
c
      real*8 iobuf
      integer buflen
      parameter (buflen=4096)
      common / dabuf / iobuf(buflen)
c
      integer numset, lencpy, irec
c
c     # bummer error types
      integer    wrnerr,   nfterr,   faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
c
c     underlying design
c
c     calculate record numbers which contain the range of
c     desired information
c
c     perform consistency checks
c
*mdc*if debug
*       write(*,*)'readvc:unit,logrec,start,length ',unit,logrec,start,
*     +               length
*mdc*endif
      if (buflen.lt.reclen) then
         call bummer('readvc: buflen too short',buflen,faterr)
      elseif (logrec.le.0) then
         call bummer('readvc: invalid logical record',logrec,faterr)
      elseif ((logrec.ne.1).and.(loglen.le.0)) then
         call bummer('readvc: invalid logical length',loglen,faterr)
      elseif (((start+length-1).gt.loglen).and.(loglen.gt.0)) then
         call bummer('readvc: read beyond end of logical record',
     +    loglen, faterr )
      endif
c
c     set up addressing information
c
      numset = (loglen - 1) / reclen + 1
c
c     calculate records offset into a vector set
c
      first = (start - 1) /reclen + 1
      first1 = start - (first - 1) * reclen
c
      last = (start + length - 2) / reclen + 1
      last1 = start + length - 1 - (last - 1) * reclen
c
c     calculate true direct access record of start
c
      dafrst = numset * (logrec - 1) + first
      dalast = numset * (logrec - 1) + last
c
c     # special cases: 1 ) irec = dafrst
c     #                2 ) irec = dalast
c     #                3 ) irec = in between
c     #                4 ) dafrst = dalast
c
      vecpos = 1
      if (dafrst.eq.dalast) then
         call readda( unit, dafrst, iobuf, reclen )
         lencpy = last1 - first1 + 1
         call dcopy( lencpy,  iobuf(first1), 1,  vector(vecpos), 1 )
      else
         do 10 irec = dafrst,dalast
            if (irec.eq.dalast) then
c
c              read in record at end and copy portion to return vector
c
               call readda( unit, irec, iobuf, reclen )
               lencpy = last1
               call dcopy( lencpy,  iobuf(1), 1,  vector(vecpos), 1 )
               vecpos = vecpos + lencpy
            elseif (irec.eq.dafrst) then
c
c              read in record at beginning and copy portion
c              to return vector
c
               call readda( unit, irec, iobuf, reclen )
               lencpy = reclen - first1 + 1
               call dcopy( lencpy, iobuf(first1), 1, vector(vecpos), 1 )
               vecpos = vecpos + lencpy
            else
c
c              read in record at correct location in return vector
c
               call readda( unit, irec, vector(vecpos), reclen )
               vecpos = vecpos + reclen
            endif
10       continue
      endif
c
c     set up return codes describing information location
c
      rcode(1) = dafrst
      rcode(2) = dalast
      rcode(3) = first1
      rcode(4) = last1
c
*mdc*if debug
*      write(*,*) ' readvc: unit,logrec,rcode ',unit,logrec,rcode
*      call writex(vector,length)
*mdc*endif
      return
      end
*deck writvc
      subroutine writvc(
     & unit,   reclen, vector, start,
     & ilen,   logrec, loglen, icode,
     & rcode )
c
c     writvc
c     version 2.0               date: 12:17:90
c     eric stahlberg - ohio state university
c
c subroutine to update a segment from a da file
c
c     unit   : unit number to read from
c     vector : vector to return read information in
c     start  : starting word of data in logical record
c     ilen   : length of data to write
c            : if length = 0, then do no write
c     loglen : length of a logical record
c     logrec : logical record number
c     icode  : function code for reading fragments
c     rcode  : integer array of return codes
c
c     special note: some o/s machine combinations do not allow
c     reading a record which has not already been initialized.
c     a method has been adopted to address this issue.  in
c     these cases, there must be a test to check if a read can
c     be done prior to updating. this will be apparent in the
c     following code.
c
      implicit integer(a-z)
c
      integer unit,ilen,logrec,loglen,start,reclen
      integer rcode(4),icode
      real*8 vector(*)
c
      integer vecpos, first1, last1, first, last, dafrst, dalast
      integer length
      logical rlead,rtrail
c
c     buffer should be tuned to appropriate values for machines
c     (see also readvc)
c
      integer buflen
      parameter (buflen=4096)
      real*8 iobuf(buflen)
c
      integer numset, lencpy, irec
c
c     # bummer error types
      integer    wrnerr,   nfterr,   faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
c
c     underlying design
c
c     calculate record numbers which contain the range of
c     desired information
c
c     perform error checking
c
*mdc*if debug
*      write(*,*)'writvc unit,logrec,start,ilen',unit,logrec,start,ilen
*mdc*endif
      length = abs(ilen)
*mdc*if debug
*      call writex(vector,length)
*mdc*endif
c
      if (buflen.lt.reclen) then
         call bummer('writvc: buflen too short ',buflen,faterr)
      elseif (logrec.le.0) then
         call bummer('writvc: invalid logical record',logrec,faterr)
      elseif ((logrec.ne.1).and.(loglen.le.0)) then
         call bummer('writvc: invalid logical record length',
     &    loglen, faterr )
      elseif (((start+length-1).gt.loglen).and.(loglen.gt.0)) then
         call bummer ('writvc: write beyond end of logical record',
     +                loglen, faterr )
      endif
c
      if (ilen.le.0) then
         return
      endif
c
c     set up fragment pre-reading control
c     binary representation...
c     00 = (0) no read leading or trailing fragment
c     01 = (1) no read leading, do read trailing
c     10 = (2) read leading, no read trailing
c     11 = (3) read leading and trailing fragments
c
       rlead  = ((icode / 2) .eq. 1)
       rtrail = (mod(icode,2) .eq. 1)
c
c     set up addressing information
c
      numset = (loglen - 1) / reclen + 1
c
c     calculate records offset into a vector set
c
      first = (start - 1) / reclen + 1
      first1 = start - (first - 1) * reclen
c
      last = (start + length - 2 ) / reclen + 1
      last1 = start + length - 1 - (last - 1) * reclen
c
c     calculate true direct access record of start
c
      dafrst = numset * (logrec - 1) + first
      dalast = numset * (logrec - 1) + last
c
c     # special cases: 1 ) irec = dafrst
c     #                2 ) irec = dalast
c     #                3 ) irec = in between
c
      vecpos = 1
      if (dafrst.eq.dalast) then
c
c        required information resides on one physical record only
c        three cases:
c        #1) no leading fragments
c        #2) no trailing fragments
c        #3) both leading and trailing fragments
c
         if (first1.eq.1) then
            if ( ((last1.ne.reclen) .and.
     &       ((start+length-1).ne.loglen)) .and. rtrail ) then
               call readda( unit, dafrst, iobuf, reclen )
            endif
         elseif ((last1.eq.reclen) .or.
     &       ((start+length-1).eq.loglen)) then
            if ((first1.ne.1).and.rlead) then
               call readda( unit, dafrst, iobuf, reclen )
            endif
         elseif (rlead.or.rtrail) then
            call readda( unit, dafrst, iobuf, reclen )
         endif
         lencpy = last1 - first1 + 1
         call dcopy( lencpy,  vector(vecpos), 1,  iobuf(first1), 1 )
         call writda( unit, dafrst, iobuf, reclen )
      else
c
c        information spans multiple physical records
c
         do 10 irec = dafrst,dalast
            if (irec.eq.dalast) then
c
c              read in record at end and copy in new portion
c
               if ((((start+length-1).ne.loglen).and.
     +          (last1.ne.reclen)).and.rtrail) then
                  call readda( unit, irec, iobuf, reclen )
               endif
               lencpy = last1
               call dcopy( lencpy, vector(vecpos), 1, iobuf(1), 1 )
c
c              write out updated record
c
               call writda( unit, irec, iobuf, reclen )
               vecpos = vecpos + lencpy
            elseif (irec.eq.dafrst) then
c
c              read in record at beginning and copy in new portion
c
               if ((first1.ne.1).and.rlead) then
                  call readda( unit, irec, iobuf, reclen )
               endif
               lencpy = reclen - first1 + 1
               call dcopy( lencpy, vector(vecpos), 1, iobuf(first1), 1 )
c
c              write out updated record
c
               call writda( unit, irec, iobuf, reclen )
               vecpos = vecpos + lencpy
            else
c
c              write out record at correct location in vector
c
               call writda( unit, irec, vector(vecpos), reclen )
               vecpos = vecpos + reclen
            endif
10       continue
      endif
c
c     set return codes to status of file access
c
      rcode(1) = dafrst
      rcode(2) = dalast
      rcode(3) = first1
      rcode(4) = last1
*mdc*if debug
*      write(*,*) 'writvc: unit,logrec,rcode ',unit,logrec,rcode
*mdc*endif
c
      return
      end
*deck inivec
      subroutine inivec( offset, number )
c
c     inivec: initialize vector i/o interface routines for
c             segmented writing
c
c     written by: eric stahlberg
c     date:       september 1990
c     version:    1.1
c
      implicit integer(a-z)
c
      integer offset(*),number
c
c     offset  : integer offset of breaks in logical record
c     number  : number of breaks
c
      integer maxbrk,nbreak,breaks
      parameter (maxbrk=100)
      common /esio90/ nbreak, breaks(maxbrk)
      save   /esio90/
c
      integer i
c
c     # bummer error types
      integer wrnerr, nfterr, faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
c
      if (number.gt.maxbrk) then
         call bummer('inivec: insuff. word-addressable breaks',
     &    number, faterr )
      endif
c
      nbreak = number
      do 10 i = 1, number
         breaks(i) = offset(i) + 1
10    continue
      return
      end
*deck getvec
      subroutine getvec(
     & unit,   lenrec, logrec, loglen,
     & vector, start,  length )
c
c     getvec : subroutine to access word addressable information
c              residing on direct access file
c
c     written by: eric stahlberg (in consulation with ron shepard)
c     date:       september 1990
c     version:    1.1
c
      implicit integer(a-z)
c
      integer unit,logrec,loglen,start,length,lenrec
      real*8 vector(*)
c
c     unit  : unit number
c     logrec: logical record number
c     loglen: logical record length
c     vector: data
c     start : start of data in logical record
c     length: length of record in logical data
c
c     local variables
c
      integer rcode(4)
c
      call readvc(
     & unit,   lenrec, vector, start,
     & length, logrec, loglen, rcode )
c
      return
      end
*deck putvec
      subroutine putvec(
     & unit,   lenrec, logrec, loglen,
     & vector, start,  length, icode  )
c
c     putvec : subroutine to place word addressable information
c              residing on direct access file
c
c     written by: eric stahlberg (in consultation with ron shepard)
c     date:       december 1990
c     version:    1.2
c
c
      implicit integer(a-z)
c
      integer unit,logrec,loglen,start,length,icode,lenrec
      real*8 vector(*)
c
c     unit  : unit number
c     logrec: logical record number
c     loglen: logical record length
c     vector: data
c     start : start of data in logical record
c     length: length of record in logical data
c     icode : initialization code (defined below)
c             icode=0 : no initialization
c             icode=1 : initialize record with breakpoints
c                       listed in common esio90
c             icode=2 : fully initialize the entire record
c
c     local variables
c
      integer ncodev,irecd,ibrk,ntime,rcode(4),precd
      real*8 data(1)
c
c     # common information
c
c     # positions of breaks when writing data in offset form
c     #  - used for icode = 2 only
c
      integer breaks,nbreak,maxbrk
      parameter (maxbrk=100)
      common /esio90/ nbreak, breaks(maxbrk)
      save   /esio90/
c
c     # bummer error types
      integer    wrnerr,   nfterr,   faterr
      parameter (wrnerr=0, nfterr=1, faterr=2)
c
c     accounting information
c
      integer    maxvec
      parameter (maxvec=500)
      integer   vtotal,    codev(maxvec),   times(maxvec)
      save      vtotal,    codev,           times
      data      vtotal/0/, codev/maxvec*0/, times/maxvec*0/
c
c
      if ((unit.le.0).or.(unit.ge.100)) then
         call bummer('putvec: bad unit number', unit, faterr )
      endif
c
c     # search initialized vector list for current vector
c
      ncodev = logrec*100 + unit
      precd = 0
      do 10 irecd=1,vtotal
         if (codev(irecd).eq.ncodev) then
            precd = irecd
            goto 15
         endif
10    continue
c
      if (precd.eq.0) then
         vtotal = vtotal + 1
         precd  = vtotal
         codev(precd) = ncodev
         times(precd) = 0
      endif
      if (vtotal.gt.maxvec) then
         call bummer('putvec: max number vectors exceeded',
     &    maxvec, faterr )
      endif
15    continue
      times(precd) = times(precd) + 1
      ntime = times(precd)
c
      if ( ntime .eq. 1 ) then
c        # first time for this vector.  initialize if necessary.
         if ( icode .eq. 0 ) then
c           # best case: no initialiation requested
         elseif ( icode .eq. 1 ) then
c           # initialize break records only
            do 100 ibrk = 1, nbreak
               call writvc(
     &          unit,   lenrec, data,   breaks(ibrk),
     &          1,      logrec, loglen, 0,
     &          rcode  )
100         continue
         elseif ( icode .eq. 2 ) then
c           # initialize every record
            do 200 ibrk = 1, loglen, lenrec
               call writvc(
     &          unit,   lenrec, data,   ibrk,
     &          1,      logrec, loglen, 0,
     &          rcode  )
200         continue
         else
            call bummer('putvec: invalid icode ', icode, faterr )
         endif
      endif
c
      call writvc(
     & unit,   lenrec, vector, start,
     & length, logrec, loglen, 3,
     & rcode  )
c
      return
      end
