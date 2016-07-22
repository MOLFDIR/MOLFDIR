C
      Subroutine Comsym
C
      Implicit Real*8 (a-h, o-z)
C
C     Calculates sparse transformation matrices to complete skeleton
C     symmetry integrals
C
      INCLUDE "molf.inc"
      INCLUDE "sym.inc"
      INCLUDE "inpt.inc"
      INCLUDE "redme.inc"
      INCLUDE "mpidef.inc"
      complex*16 sum,c0
C     Find out if we're dealing with a quaternionic group
      quater = .FALSE.
      do irp = 1, nsymrp
         if (krptnr(irp).eq.irp) quater = .TRUE.
      enddo
      if (mpirnk.eq.0) then
      print*
      if (quater) 
     &   print*, 'Point group is quaternionic :',
     &           ' integrals (ibar,j|k,l) are written'
      print*, 'Symmetry evaluation :'
      endif
#if defined (BIT64)
      c0 = (0.0, 0.0)
#else
      c0 = (0.0d0, 0.0d0)
#endif
      tres = 1.d-10
      ntau = 0
      nrmes = 0
      do 4000 irrep = 1, nrep
        ifie = nfie(irrep)
        if (ifie.eq.0) goto 4000
        do 3000 jrrep = 1, irrep
          jfie = nfie(jrrep)
          if (jfie.eq.0) goto 3000
C-------------------------------------------------------
C Completion of skeleton integrals is done in batches
C with irrep,moi and jrrep,moj fixed and all other labels
C free : amount of space required of order NSP^2
C-------------------------------------------------------
          incore = 0
          krrepmx = irrep
          if (quater) krrepmx = nrep
          do 2000 krrep = 1, krrepmx
            kfie = nfie(krrep)
            if (kfie.eq.0) goto 2000
            lrrepmx = irrep
            if (quater) lrrepmx = nrep
            do 1000 lrrep = 1, lrrepmx
              lfie = nfie(lrrep)
              if (lfie.eq.0) goto 1000
              klfie = kfie * lfie
              sum = c0
              do 100 iop = 1, nsymop
#if defined (BIT64)
                sum = sum + conjg(repchr(iop,irrep))*
#else
                sum = sum + dconjg(repchr(iop,irrep))*
#endif
     1                            repchr(iop,jrrep) *
#if defined (BIT64)
     2                      conjg(repchr(iop,krrep))*
#else
     2                     dconjg(repchr(iop,krrep))*
#endif
     3                            repchr(iop,lrrep)
  100           continue
#if defined (BIT64)
              if (cabs(sum).gt.tres) then
               mtau = (real(sum) + 0.1) / nsymop
#else
              if (cdabs(sum).gt.tres) then
               mtau = (dble(sum) + 0.1d0) / nsymop
#endif
              if ((ntau+mtau).gt.N54) call q2job(4,'Comsym.N54',' ',0)
              call skelop (irrep,jrrep,krrep,lrrep,mtau)
              do 200 itau = ntau + 1, ntau + mtau
                indtau(itau) = incore + 1
                incore = incore + klfie
 200            continue
              ntau = ntau + mtau
              if (mtau.ne.0.and.iprnt.ge.1.and.mpirnk.eq.0)
     &           print999,irrep,jrrep,krrep,lrrep,mtau
  999         format (' Irrep quadruple (',4i3,') :',i4,' Red. MEs')
              endif
 1000         continue
 2000       continue
          nrme(irrep,jrrep) = incore 
          nrmes = max0(incore,nrmes)
 3000     continue
 4000   continue
      if (mpirnk.eq.0) then
      write (*,*)
      write (*,*) 'Number of transformation elements :',nmata
      write (*,*) 'Number of backtransformation elements :',nmatb
      if (iprnt.gt.0) then
      do irrep = 1, nrep
         do jrrep = 1, irrep
            write (*,998) irrep,jrrep,nrme(irrep,jrrep)
 998        format (" Irrep pair ",'(',i2,',',i2,')',i8,' reduced ',
     &              'integrals')
         enddo
      enddo
      endif
      write (*,*) 'Maximum number of elements in one batch :',nrmes
      write (*,*)
      endif
      return
      end
