*deck iodfac
      function iodfac(l1,l2,m1,m2,n1,n2)
c
c  compute the product of odd number factorials which gives the
c  one-center overlaps of cartesian gaussian aos.
c
      if(mod(l1+l2,2).eq.1 .or.
     &   mod(m1+m2,2).eq.1 .or.
     &   mod(n1+n2,2).eq.1) then
        iodfac = 0
      else
        iprd = 1
        do 20 i=3,l1+l2-1,2
          iprd = i*iprd
   20   continue
        do 30 i=3,m1+m2-1,2
          iprd = i*iprd
   30   continue
        do 40 i=3,n1+n2-1,2
          iprd = i*iprd
   40   continue
        iodfac = iprd
      endif
      return
      end
