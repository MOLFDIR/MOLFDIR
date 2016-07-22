      Subroutine skelop (irrep,jrrep,krrep,lrrep,mtau)
C
      Implicit Real*8 (a-h, o-z)
C
      INCLUDE "molf.inc"
      INCLUDE "sym.inc"
      INCLUDE "redme.inc"
      complex*16 c0,sum,proper,propnr,value
#if defined (BIT64)
      complex*16 cdotc
#else
      complex*16 zdotc
#endif
C
      dimension value(256),ind(256),proper(256,12),propnr(12)
C
#if defined (BIT64)
      c0 = (0.0, 0.0)
      c1 = (1.0, 0.0)
#else
      c0 = (0.d0, 0.d0)
      c1 = (1.d0, 0.d0)
#endif
C
C     Calculate projection operator to symmetrize integrals :
C     Project on (complex) canonical and Kramers' unique set
C     Kramers sets : 1) (ibar,jbar|kbar,lbar)
C                    2) (ibar,j   |kbar,l   )
C                    3) (ibar,j   |k   ,lbar)
C                    4) (ibar,j   |k   ,l   )
C     4) will only be non-zero for the quaternionic groups but this
C     will be found by the projection as well
C
      tres = 1.d-10
      ltau = 0
      do 40000 ia = ioffrp(irrep), ioffrp(irrep+1)-1
        isa = iprtnr(ia)
        iatr = krptnr(ia)
        if (iatr.GT.0.AND.iatr.NE.ia) goto 40000 ! Skip (i,x|x,x)
        do 30000 ib = ioffrp(jrrep), ioffrp(jrrep+1)-1
          isb = iprtnr(ib)
          ibtr = krptnr(ib)
          do 20000 ic = ioffrp(krrep), ioffrp(krrep+1)-1
            isc = iprtnr(ic)
            ictr = krptnr(ic)
            if (ictr.GT.0.AND.ictr.NE.ic.AND.ibtr.LT.0)
     &         goto 20000 ! Skip (ibar,jbar|k,x)
            do 10000 id = ioffrp(lrrep), ioffrp(lrrep+1)-1
               isd = iprtnr(id)
               idtr = krptnr(id)
C              Skip (ibar,j|kbar,lbar) and (ibar,jbar|kbar,l)
               if (idtr.LT.0.AND.ictr.LT.0.AND.ibtr.GT.0.AND.
     &             ibtr.NE.ib) goto 10000 
               if (idtr.GT.0.AND.ictr.LT.0.AND.ibtr.LT.0.AND.
     &             idtr.NE.id) goto 10000
               ijkldim = idim(ia) * idim(ib) * idim(ic) * idim(id)
               num = 0
               ijkl = 0
               do 4000 isi = 1, idim(ia)
                 ii = ia + (isi - isa)
                 iitr = abs(krptnr(ii))
                 do 3000 isj = 1, idim(ib)
                   ij = ib + (isj - isb)
                   ijtr = abs(krptnr(ij))
                   do 2000 isk = 1, idim(ic)
                     ik = ic + (isk - isc)
                     iktr = abs(krptnr(ik))
                     do 1000 isl = 1, idim(id)
                       il = id + (isl - isd)
                       iltr = abs(krptnr(il))
                       ijkl = ijkl + 1
                       sum = c0
                       do 100 iop = 1, nsymop
#if defined (BIT64)
                         sum = sum + conjg(repmat(isi,isa,iop,irrep))
#else
                         sum = sum +dconjg(repmat(isi,isa,iop,irrep))
#endif
     1                             *       repmat(isj,isb,iop,jrrep)
#if defined (BIT64)
     2                             * conjg(repmat(isk,isc,iop,krrep))
#else
     2                             *dconjg(repmat(isk,isc,iop,krrep))
#endif
     3                             *       repmat(isl,isd,iop,lrrep)
  100                    continue
#if defined (BIT64)
                       if (cabs(sum).gt.tres) then
#else
                       if (cdabs(sum).gt.tres) then
#endif
                         num = num + 1
                         value(num) = sum
                         ind(num) = ijkl
                         zeroint(ii,ij,ik,il) = .FALSE.
                         zeroint(ij,ii,il,ik) = .FALSE.
                         zeroint(ik,il,ii,ij) = .FALSE.
                         zeroint(il,ik,ij,ii) = .FALSE.
                         zeroint(ijtr,iitr,ik,il) = .FALSE.
                         zeroint(iitr,ijtr,il,ik) = .FALSE.
                         zeroint(iltr,iktr,ii,ij) = .FALSE.
                         zeroint(iktr,iltr,ij,ii) = .FALSE.
                         zeroint(ii,ij,iltr,iktr) = .FALSE.
                         zeroint(ij,ii,iktr,iltr) = .FALSE.
                         zeroint(ik,il,ijtr,iitr) = .FALSE.
                         zeroint(il,ik,iitr,ijtr) = .FALSE.
                         zeroint(ijtr,iitr,iltr,iktr) = .FALSE.
                         zeroint(iitr,ijtr,iktr,iltr) = .FALSE.
                         zeroint(iltr,iktr,ijtr,iitr) = .FALSE.
                         zeroint(iktr,iltr,iitr,ijtr) = .FALSE.
                       endif
 1000                  continue
 2000                continue
 3000              continue
 4000            continue
C
C              Now all the non-zero contributions to these integrals are found
C              Diagonalize the projection operator :
C
               if (num.gt.0) then
                  call czero (ijkldim,proper(1,ltau+1))
                  do 5000 ium = 1, num
                    proper(ind(ium),ltau+1) = value(ium)
 5000               continue
                  do 6000 itau = 1, ltau
                    sum = propnr(itau) *
#if defined (BIT64)
     &              cdotc (ijkldim,proper(1,itau),1,proper(1,ltau+1),1)
                    if (cabs(sum).gt.tres) then
#else
     &              zdotc (ijkldim,proper(1,itau),1,proper(1,ltau+1),1)
                    if (cdabs(sum).gt.tres) then
#endif
                     nmatb = nmatb + 1
                     if (nmatb.gt.n51) call q2job(4,'N51.skelop',' ',0)
                     nmat = mmatb(ia,ib) + 1
                     if (nmat.gt.N52) call q2job (4,'N52.skelop',' ',0)
                     mmatb(ia,ib) = nmat
                     mmatpb(nmat,ia,ib) = nmatb
                     bmat(nmatb) = sum
                     kb(nmatb) = ic
                     lb(nmatb) = id
                     itaub(nmatb) = ntau + itau
                    endif
#if defined (BIT64)
                    call caxpy
#else
                    call zaxpy
#endif
     &              (ijkldim,-sum,proper(1,itau),1,proper(1,ltau+1),1)
 6000               continue
                  sum =
#if defined (BIT64)
     &            cdotc (ijkldim,proper(1,ltau+1),1,proper(1,ltau+1),1)
                  if (cabs(sum).gt.tres) then
#else
     &            zdotc (ijkldim,proper(1,ltau+1),1,proper(1,ltau+1),1)
                  if (cdabs(sum).gt.tres) then
#endif
                    ltau = ltau + 1
                    if (ltau.gt.mtau) call q2job(6,'ltaugtmtau',' ',0)
                    propnr(ltau) = c1 / sum
                    nmatb = nmatb + 1
                    if (nmatb.gt.n51) call q2job(4,'N51.skelop',' ',0)
                    nmat = mmatb(ia,ib) + 1
                    if (nmat.gt.N53) call q2job (4,'N53.skelop',' ',0)
                    mmatb(ia,ib) = nmat
                    mmatpb(nmat,ia,ib) = nmatb
                    bmat(nmatb) = c1
                    kb(nmatb) = ic
                    lb(nmatb) = id
                    itaub(nmatb) = ntau + itau
                    ijkl = 0
                    do 6100 isi = 1, idim(ia)
                      ii = ia + (isi - isa)
                    do 6100 isj = 1, idim(ib)
                      ij = ib + (isj - isb)
                    do 6100 isk = 1, idim(ic)
                      ik = ic + (isk - isc)
                    do 6100 isl = 1, idim(id)
                      il = id + (isl - isd)
                      ijkl = ijkl + 1
#if defined (BIT64)
                      if (cabs(proper(ijkl,ltau)).gt.tres) then
#else
                      if (cdabs(proper(ijkl,ltau)).gt.tres) then
#endif
                        nmata = nmata + 1
                        if (nmata.gt.N50) call q2job(4,'N50',' ',0)
                        nmat = mmata(ii,ij) + 1
                        if (nmat.gt.N52) call q2job(4,'N52',' ',0)
                        mmata(ii,ij) = nmat
                        mmatpa(nmat,ii,ij) = nmata
                        amat(nmata) = proper(ijkl,ltau)
                        ka(nmata) = ik
                        la(nmata) = il
                        itaua(nmata) = ntau + ltau
                       endif
 6100                  continue
                  endif
                endif
10000         continue
20000       continue
30000     continue
40000   continue
      mtau = ltau
      return
      end
