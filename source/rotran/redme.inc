      parameter (N50=21092, N51=21092, N52=249, N53=192, N54=2000)
      complex*16 amat,bmat
      integer ntau,nrmes,nrme,indtau,nmata,nmatb,itaua,itaub,ka
      integer kb,la,lb,mmata,mmatb,mmatpa,mmatpb
      logical zeroint,quater,realgr
      common/comint/ ntau,nrmes,nrme(8,8),indtau(N54),nmata,nmatb,
     & itaua(N50),itaub(N51),ka(N50),kb(N51),la(N50),lb(N51),
     & mmata(16,16),mmatb(16,16),mmatpa(N52,16,16),mmatpb(N53,16,16)
      common/comcom/ amat(N50),bmat(N51)
      common/comlog/ zeroint(16,16,16,16),quater,realgr
