  program read_pairs
!
  integer*2 :: psfc_obs_qc,slp_obs_qc,t_obs_qc,td_obs_qc,ws_obs_qc, &
               wd_obs_qc,q_obs_qc
  integer*2 :: iyear,imonthday,ihourmin,ilat,ilon,domain_id, &
               ipmstn,ipsfc_obs,islpstn,islp_obs,iterstn,ih_obs,it2stn,it_obs, &
               iqmstn,iq_obs,iwsstn,iws_obs,iwdstn,iwd_obs
  character(len=4) :: platform
  character(len=80) :: exename,fn1
  character(len=14) :: time_str
  character (len=120), parameter :: outfmt = &
            '(a14,f8.3,f9.3,1x,a4,2(f9.2,i7),f9.2,4(f9.2,i7),7f9.2)'
  character(len=4) :: ayr
  character(len=5) :: amd,ahm
  integer :: missing=-8888
  real :: rmissing=-8888.

!
  j=iargc()
  if(j < 1) then
    call getarg(0,exename)
    print*,'Usage: ',TRIM(exename),' filename'
    stop
  endif

  call getarg(1,fn1)

  open(11,file=fn1,access='direct',recl=56)
  open(12,file='obs.dat',recl=1000)

  irec=0
loop_read: do

           irec=irec+1
           read(11,rec=irec,iostat=ierr) iyear,imonthday,ihourmin, &
               ilat,ilon,domain_id,platform, &
               ipmstn,ipsfc_obs,psfc_obs_qc, &
               islpstn,islp_obs,slp_obs_qc, &
               iterstn,ih_obs, &
               it2stn,it_obs,t_obs_qc, &
               iqmstn,iq_obs,q_obs_qc, &
               iwsstn,iws_obs,ws_obs_qc, &
               iwdstn,iwd_obs,wd_obs_qc

!          if(iyear == 0) exit loop_read
           if(iyear < 1980 .or. iyear > 2020 ) exit loop_read
           if(ierr /= 0) exit loop_read

!          write(12,*) iyear,imonthday,ihourmin, &
!              ilat,ilon,domain_id,platform, &
!              ipmstn,ipsfc_obs,psfc_obs_qc, &
!              islpstn,islp_obs,slp_obs_qc, &
!              iterstn,ih_obs, &
!              it2stn,it_obs,t_obs_qc, &
!              iqmstn,iq_obs,q_obs_qc, &
!              iwsstn,iws_obs,ws_obs_qc, &
!              iwdstn,iwd_obs,wd_obs_qc

           write(ayr,'(i4)') iyear
           write(amd,'(i5)') 10000+imonthday
           write(ahm,'(i5)') 10000+ihourmin
           time_str=ayr//amd(2:5)//ahm(2:5)//'00'

           rlat=ilat*0.01
           rlon=ilon*0.01
             pmstn=ipmstn*0.1
             slpstn=islpstn*0.1
             t2stn=it2stn*0.01
             qmstn=iqmstn*0.01
             wsstn=iwsstn*0.01
             wdstn=iwdstn*1.0
             terstn=iterstn*1.0
           if(ipsfc_obs /= missing) then
             psfc_obs=ipsfc_obs*0.1
           else
             psfc_obs=rmissing
           endif
           if(islp_obs /= missing) then
             slp_obs=islp_obs*0.1
           else
             slp_obs=rmissing
           endif
           h_obs=float(ih_obs) 
           if(it_obs /= missing) then
             t_obs=it_obs*0.01
           else
             t_obs=rmissing
           endif
           if(iq_obs /= missing) then
             q_obs=iq_obs*0.01
           else
             q_obs=rmissing
           endif
           if(iws_obs /= missing) then
             ws_obs=iws_obs*0.01
           else
             ws_obs=rmissing
           endif
           wd_obs=float(iwd_obs)

           write(12,outfmt)time_str,rlat,rlon,platform, &
                 psfc_obs,psfc_obs_qc,slp_obs,slp_obs_qc,h_obs,t_obs, &
                 t_obs_qc,q_obs,q_obs_qc,ws_obs,ws_obs_qc,wd_obs,wd_obs_qc, &
                 pmstn,slpstn,terstn,t2stn,qmstn,wsstn,wdstn

           enddo loop_read

  end program read_pairs

