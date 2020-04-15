  program read_pairs
!
  integer*2 :: psfc_obs_qc,slp_obs_qc,t_obs_qc,td_obs_qc,ws_obs_qc, &
               wd_obs_qc,q_obs_qc
  integer*2 :: iyear,imonthday,ihourmin,ilat,ilon,domain_id, &
               ipmstn,ipsfc_obs,islpstn,islp_obs,iterstn,ih_obs,it2stn,it_obs, &
               iqmstn,iq_obs,iwsstn,iws_obs,iwdstn,iwd_obs
  character(len=4) :: platform
  character(len=80) :: exename,fn1
  character(len=10) :: opt
  logical :: laddstid = .FALSE.

!
  j=iargc()
  if(j < 1) then
    call getarg(0,exename)
    print*,'Usage: ',TRIM(exename),' filename [-add_stid]'
    stop
  endif

  call getarg(1,fn1)

  if (j == 2) then
     call getarg(2,opt)
     if (index(opt,'-add_stid') > 0) laddstid = .TRUE.
  end if

  if (laddstid) then
     open(11,file=fn1,access='direct',recl=64)
  else
     open(11,file=fn1,access='direct',recl=56)
  end if

  irec=0
loop_read: do

           irec=irec+1
           if (laddstid) then
              read(11,rec=irec,iostat=ierr) iyear,imonthday,ihourmin, &
                  ilat,ilon,domain_id,platform, &
                  ipmstn,ipsfc_obs,psfc_obs_qc, &
                  islpstn,islp_obs,slp_obs_qc, &
                  iterstn,ih_obs, &
                  it2stn,it_obs,t_obs_qc, &
                  iqmstn,iq_obs,q_obs_qc, &
                  iwsstn,iws_obs,ws_obs_qc, &
                  iwdstn,iwd_obs,wd_obs_qc, &
                  st_id
           else
              read(11,rec=irec,iostat=ierr) iyear,imonthday,ihourmin, &
                  ilat,ilon,domain_id,platform, &
                  ipmstn,ipsfc_obs,psfc_obs_qc, &
                  islpstn,islp_obs,slp_obs_qc, &
                  iterstn,ih_obs, &
                  it2stn,it_obs,t_obs_qc, &
                  iqmstn,iq_obs,q_obs_qc, &
                  iwsstn,iws_obs,ws_obs_qc, &
                  iwdstn,iwd_obs,wd_obs_qc
           end if

           if(ierr /= 0) exit loop_read

           if (laddstid) then
              write(6,*) iyear,imonthday,ihourmin, &
                  ilat,ilon,domain_id,platform, &
                  ipmstn,ipsfc_obs,psfc_obs_qc, &
                  islpstn,islp_obs,slp_obs_qc, &
                  iterstn,ih_obs, &
                  it2stn,it_obs,t_obs_qc, &
                  iqmstn,iq_obs,q_obs_qc, &
                  iwsstn,iws_obs,ws_obs_qc, &
                  iwdstn,iwd_obs,wd_obs_qc, &
                  st_id
           else
              write(6,*) iyear,imonthday,ihourmin, &
                  ilat,ilon,domain_id,platform, &
                  ipmstn,ipsfc_obs,psfc_obs_qc, &
                  islpstn,islp_obs,slp_obs_qc, &
                  iterstn,ih_obs, &
                  it2stn,it_obs,t_obs_qc, &
                  iqmstn,iq_obs,q_obs_qc, &
                  iwsstn,iws_obs,ws_obs_qc, &
                  iwdstn,iwd_obs,wd_obs_qc
           end if

           enddo loop_read

  end program read_pairs
