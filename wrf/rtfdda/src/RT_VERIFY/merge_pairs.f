  program merge_pairs
!
  character(len=80) :: exename,fn1,fn2,fn3,fn4
!
  j=iargc()
  if(j < 2) then
    call getarg(0,exename)
    print*,'Usage: ',TRIM(exename),' filename1 filename2 [filename3 ...]'
    stop
  endif
!
  call getarg(1,fn1)
  call getarg(2,fn2)
  if(j > 2) call getarg(3,fn3)
  if(j > 3) call getarg(4,fn4)

  open(11,file=fn1,access='direct',recl=56)
  open(12,file=fn2,access='direct',recl=56)
  if(j > 2) open(13,file=fn3,access='direct',recl=56)
  if(j > 3) open(14,file=fn4,access='direct',recl=56)

  call replace(11,12)
  if(j > 2) call replace(11,13)
  if(j > 3) call replace(11,14)

  end program merge_pairs
!
!
!
  subroutine replace(iunit1,iunit2)
!
  integer*2 :: psfc_obs_qc,slp_obs_qc,t_obs_qc,td_obs_qc,ws_obs_qc, &
               wd_obs_qc,q_obs_qc
  integer*2 :: iyear,imonthday,ihourmin,ilat,ilon,domain_id, &
               ipmstn,ipsfc_obs,islpstn,islp_obs,iterstn,ih_obs,it2stn,it_obs, &
               iqmstn,iq_obs,iwsstn,iws_obs,iwdstn,iwd_obs
  integer*2 :: psfc_obs_qc2,slp_obs_qc2,t_obs_qc2,td_obs_qc2, &
               ws_obs_qc2,wd_obs_qc2,q_obs_qc2
  integer*2 :: iyear2,imonthday2,ihourmin2,ilat2,ilon2,domain_id2, &
               ipmstn2,ipsfc_obs2,islpstn2,islp_obs2,iterstn2,ih_obs2, &
               it2stn2,it_obs2,iqmstn2,iq_obs2,iwsstn2,iws_obs2,iwdstn2,iwd_obs2
  character(len=4) :: platform
  character(len=4) :: platform2

  irec2=0
  ierr2=0
! irec1=0   ! yliu   Did not work well, comment it out!


loop2: do
       irec2=irec2+1
       read(iunit2,rec=irec2,iostat=ierr2) iyear2,imonthday2,ihourmin2, &
               ilat2,ilon2, &
               domain_id2,platform2, &
               ipmstn2,ipsfc_obs2,psfc_obs_qc2, &
               islpstn2,islp_obs2,slp_obs_qc2, &
               iterstn2,ih_obs2, &
               it2stn2,it_obs2,t_obs_qc2, &
               iqmstn2,iq_obs2,q_obs_qc2, &
               iwsstn2,iws_obs2,ws_obs_qc2, &
               iwdstn2,iwd_obs2,wd_obs_qc2

       if(ierr2 /= 0) exit loop2

       irec1=0   ! yliu   Has to turn it back on!
       ierr1=0

       loop1: do
              irec1=irec1+1
              read(iunit1,rec=irec1,iostat=ierr1) iyear,imonthday,ihourmin, &
               ilat,ilon,domain_id,platform, &
               ipmstn,ipsfc_obs,psfc_obs_qc, &
               islpstn,islp_obs,slp_obs_qc, &
               iterstn,ih_obs, &
               it2stn,it_obs,t_obs_qc, &
               iqmstn,iq_obs,q_obs_qc, &
               iwsstn,iws_obs,ws_obs_qc, &
               iwdstn,iwd_obs,wd_obs_qc

              if(ierr1 /= 0) exit loop1

              if((iyear2 == iyear) .and. (imonthday2 == imonthday) .and. &
                 (ihourmin2 == ihourmin) .and. (ilat2 == ilat) .and. &
                 (ilon2 == ilon) .and. (platform2 == platform)) then

!               print*, 'Replacing record number ',irec1
                write(11,rec=irec1) iyear2,imonthday2,ihourmin2,ilat2,ilon2, &
                  domain_id2,platform2, &
                  ipmstn2,ipsfc_obs2,psfc_obs_qc2, &
                  islpstn2,islp_obs2,slp_obs_qc2, &
                  iterstn2,ih_obs2, &
                  it2stn2,it_obs2,t_obs_qc2, &
                  iqmstn2,iq_obs2,q_obs_qc2, &
                  iwsstn2,iws_obs2,ws_obs_qc2, &
                  iwdstn2,iwd_obs2,wd_obs_qc2

                exit loop1

              endif
              enddo loop1
       enddo loop2

  return
  end subroutine replace
