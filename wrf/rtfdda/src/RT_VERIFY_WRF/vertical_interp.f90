  subroutine vertical_interp(raobs,crs,qc,qc_new,pr,mkz,nlvls,badvalue,itr)

  integer :: nlvls,mkz,k,l,lvl,iflag
  real :: badvalue,pres,pabv,pblw,vabv,vblw
  real, dimension(nlvls) :: raobs,pr
  real, dimension(mkz) :: crs,pc
  real, dimension(5000) :: obs,pl
  integer, dimension(nlvls) :: qc
  integer, dimension(mkz) :: qc_new
  integer, dimension(5000) :: gqc
  integer :: qabv,qblw
  integer :: itr

  pc(1)=1010.
  do k=2,mkz
     pc(k)=1000.-(k-2)*25.
  enddo

  l=0
  do k=1,nlvls
     if(raobs(k) > badvalue .and. pr(k) > badvalue) then  ! Keep only those
       l=l+1                                              ! levels with good
       obs(l)=raobs(k)                                    ! data.
       pl(l)=pr(k)
       gqc(l)=qc(k)
     endif
  enddo

  lvl=l

  if(lvl < 2) then
    print *, 'There are only ',lvl,'lvls avialable.'
    print *, 'Too bad, stop here.'
    crs=badvalue
    qc_new=0
    return
    stop
  endif

  Loop1: do k=1,mkz
     pres=pc(k)
     if(pres > pl(1) .or. pres < pl(lvl)) then
       crs(k)=badvalue
       qc_new(k)=0
     else
       iflag=0
       Loop2: do l=2,lvl
          if(pres <= pl(l-1) .and. pres >= pl(l)) then
            pabv=pl(l)
            pblw=pl(l-1)
            vabv=obs(l)
            vblw=obs(l-1)
            qabv=gqc(l)
            qblw=gqc(l-1)
            iflag=1
            exit Loop2
          endif
       enddo Loop2

       if(iflag == 0) then
         crs(k)=badvalue
         qc_new(k)=max(qabv,qblw)
         cycle Loop1
       endif

       if(pblw == pabv) then
         crs(k)=vabv
         qc_new(k)=qabv
       else
        if(itr == 1) then
         crs(k)=vabv+(pres-pabv)*(vblw-vabv)/(pblw-pabv)
        else 
! yliu: use log p fro T interp, to be consistent with sig2p of model part
         crs(k)=vabv+(alog(pres)-log(pabv))*(vblw-vabv)/(log(pblw)-log(pabv))
        endif
         qc_new(k)=max(qabv,qblw)
       endif

       if(pres == pabv) qc_new(k)=qabv

! reset by YLIU for new QC and compliment also to old QC
       if(qabv > 30000. .or. qblw > 30000.) then
        qc_new(k) = max (qabv, qblw)
       else if (qabv < 3.1 .or. qblw < 3.1) then
        qc_new(k) = min (qabv, qblw)
       endif
! yliu end
       
       if(pblw < 1. .or. pabv < 1.) crs(k)=badvalue

       if(abs(pres-pabv) > 60. .and. abs(pres-pblw) > 10.) crs(k)=badvalue
       if(abs(pres-pabv) > 30. .and. abs(pres-pblw) > 30.) crs(k)=badvalue
       if(abs(pres-pabv) > 10. .and. abs(pres-pblw) > 60.) crs(k)=badvalue

     endif

  enddo Loop1

  return
  end subroutine vertical_interp
