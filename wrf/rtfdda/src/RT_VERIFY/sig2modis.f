  subroutine sig2modis(sig,prs,psig,pmodis,itr,kx,kpt,spval)

! sig and psig are from top to bottom.
! sig is the array that holds the values of a specific field on sigma levels.
! psig is the pressure array on sigma levels.
! prs is the array that holds the interpolated (onto every 25mb) values and
! retruned to the main program for subsequent use.

  real, dimension(kx) :: sig,psig
  real, dimension(kpt) :: prs,pmodis
  real, dimension(100) :: alp,als,d1,d2

  kst=kx

  if(itr == 1) then
    do k=1,kpt
       alp(k)=pmodis(k)
    enddo
  elseif(itr == 2) then
    do k=1,kpt
       alp(k)=alog(pmodis(k))
    enddo
  endif

  if(itr == 1) then
    do k=1,kst
       a=psig(k)
       als(k)=a
       d1(k)=sig(k)
    enddo
  elseif(itr == 2) then
    do k=1,kst
       a=psig(k)
       als(k)=alog(a)
       d1(k)=sig(k)
    enddo
  endif

  Loop1: do k=1,kpt
     if(pmodis(k) == spval) then
       prs(k)=spval
       cycle Loop1
     endif

     m=1
     do l=1,kst
        if(als(l) <= alp(k)) m=m+1
     enddo

     if(m > kst) then
       d2(k)=spval
     elseif(m == 1) then
       d2(k)=spval
     else
       ad=als(m)-alp(k)
       au=(alp(k)-als(m-1))
       d2(k)=(d1(m)*au+d1(m-1)*ad)/(als(m)-als(m-1))
     endif

     prs(k)=d2(k)
  enddo Loop1

  return

  end subroutine sig2modis
