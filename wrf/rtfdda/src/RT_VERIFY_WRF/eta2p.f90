  subroutine eta2p(eta,prs,peta,itr,kx,kpt,spval)

! eta and peta are from bottom to top.
! eta is the array that holds the values of a specific field on eta levels.
! peta is the pressure array on eta levels.
! prs is the array that holds the interpolated (onto every 25mb) values and
! retruned to the main program for subsequent use.

  integer :: kx,kpt,itr,k,kst,m,l
  real, dimension(kx) :: eta,peta
  real, dimension(kpt) :: prs,p
  real, dimension(100) :: alp,als,d1,d2
  real :: a,spval,au,ad

  p(1)=101000.
  do k=2,kpt
     p(k)=100000.-(k-2)*2500.
  enddo

  kst=kx

  if(itr == 1) then
    do k=1,kpt
       alp(k)=p(k)
    enddo
  elseif(itr == 2) then
    do k=1,kpt
       alp(k)=alog(p(k))
    enddo
  endif

  if(itr == 1) then
    do k=1,kst
       a=peta(k)
       als(k)=a
       d1(k)=eta(k)
    enddo
  elseif(itr == 2) then
    do k=1,kst
       a=peta(k)
       if (a == spval) then
          als(k) = spval
       else
          als(k)=alog(a)
       end if
       d1(k)=eta(k)
    enddo
  endif

  Loop1: do k=1,kpt
     if(p(k) == spval) then
       prs(k)=spval
       cycle Loop1
     endif

     m=1
     do l=1,kst
        if(als(l) >= alp(k)) m=m+1
     enddo

     if(m > kst) then
       d2(k)=spval
     elseif(m == 1) then
       d2(k)=spval
     else
       if (als(m) == spval .or. als(m-1) == spval .or. d1(m) == spval &
           .or. d1(m-1) == spval) then
          d2(k) = spval
       else
          au=alp(k)-als(m)
          ad=als(m-1)-alp(k)
          d2(k)=(d1(m)*ad+d1(m-1)*au)/(als(m-1)-als(m))
       end if
     endif

     prs(k)=d2(k)
  enddo Loop1

  return

  end subroutine eta2p
