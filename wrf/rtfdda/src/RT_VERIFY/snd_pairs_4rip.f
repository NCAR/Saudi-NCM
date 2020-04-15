  program snd_pairs

  INTERFACE
    subroutine pseudo_snd(alat,alon,ix,jx,kx,xlat,xlon,x,x_ps,iflag)
 
      real, dimension(:,:), intent(in) :: xlat,xlon
      real, dimension(:,:,:), intent(in) :: x
      real, dimension(kx) :: x_ps
    end subroutine pseudo_snd

  END INTERFACE

  integer, parameter :: ks=40,badvalue=-8888.
  real, dimension(ks) :: um,vm,tm,qm,pm,rhm,wsm,wdm
  real, dimension(ks) :: t_oint,u_oint,v_oint,q_oint,rh_oint,ws_oint,wd_oint
  integer, dimension(ks) :: t_oint_qc,u_oint_qc,v_oint_qc,q_oint_qc,rh_oint_qc, &
                            ws_oint_qc,wd_oint_qc
  real,  dimension(100) :: prs_ps,t_ps,td_ps,q_ps,u_ps,v_ps,ws_ps,wd_ps
  real,  dimension(150) :: prs_obs,t_obs,td_obs,q_obs,u_obs,v_obs,ws_obs,wd_obs
  integer, dimension(150) :: prs_qc,t_qc,td_qc,ws_qc,wd_qc,q_qc,u_qc,v_qc

  integer, parameter :: snd_unit=61,model_unit=71,out_unit=81
  integer :: domain_id
  character :: adomain_id

  integer :: AllocateStatus,flag

  character (len=10) :: atime_new,model_time_new='0000000000'
  integer :: t_count
  logical :: OUTPUT,found,early

  real :: alon,alat,hs,elev
  real, dimension(50) :: wk,wk1,pwk

  character (len=14) :: time_strm,time_stro 
  character(len=28) :: otll,mtll,cndom
  integer, parameter :: imissing=-8888
  real, parameter :: missing=-8888.
  real :: rhcalc

   !  cndom = domain #
  CALL getarg(1, cndom)
  adomain_id=TRIM(cndom)
  read(adomain_id,FMT='(i1)') domain_id
  !domain_id=1
  !write(adomain_id,'(i1)') domain_id

  pi=acos(-1.)

  ierro=0
  ierrm=0
  icnt=0         !! counter to determine when MM5 arrays need to be deallocated
  irec=0         !! counter for direct access record number
  t_count=0      !! counter to keep track of the time period of MM5 output

SND:  do while(ierro == 0)

     read(snd_unit,*,IOSTAT=ierro) time_stro,alat,alon,n_levels
     if(ierro /= 0) exit SND
     call nearest_hour(time_stro,atime_new)
     print*,'obs sndg. time, lat, lon: ',time_stro, alat, alon
     write(otll,"(a14,2f7.2)") time_stro, alat, alon

     do k=1,n_levels
        read(snd_unit,*) iprs_obs,prs_qc(k),it_obs,t_qc(k),itd_obs,td_qc(k), &
                   iws_obs,ws_qc(k),iwd_obs,wd_qc(k)
        if(prs_qc(k) >= 32768) prs_qc(k)=32767
        if(t_qc(k) >= 32768) t_qc(k)=32767
        if(td_qc(k) >= 32768) td_qc(k)=32767
        if(ws_qc(k) >= 32768) ws_qc(k)=32767
        if(wd_qc(k) >= 32768) wd_qc(k)=32767

        if(iprs_obs > imissing) then
          prs_obs(k)=iprs_obs*0.01      ! now in mb
        else
          prs_obs(k)=missing
        endif

        if(it_obs > imissing) then
          t_obs(k)=it_obs*0.01          ! in K
        else
          t_obs(k)=missing
        endif

        if(itd_obs > imissing) then
          td_obs(k)=itd_obs*0.01        ! in K
        else
          td_obs(k)=missing
        endif

        if(iws_obs > imissing) then
          ws_obs(k)=iws_obs*0.01           ! in m/s
        else
          ws_obs(k)=missing
        endif

        wd_obs=iwd_obs*1.             ! in deg
        call qfromttd(t_obs(k),t_qc(k),td_obs(k),td_qc(k), &
                      prs_obs(k),prs_qc(k),qv,iqc,missing)
        q_obs(k)=qv
        q_qc(k)=iqc

        if(ws_obs(k) > missing .and. wd_obs(k) > missing) then
          u_obs(k)=-ws_obs(k)*sin(wd_obs(k)*pi/180.)
          v_obs(k)=-ws_obs(k)*cos(wd_obs(k)*pi/180.)
        else
          u_obs(k)=missing
          v_obs(k)=missing
        endif

        u_qc(k)=max(ws_qc(k),wd_qc(k))
        v_qc(k)=u_qc(k)

     enddo
!
!    Find the matching MM5 Pseudo-soundings
!
  MODEL:  do while(ierrm== 0)

     if(.not. found) then
     read(model_unit,*,IOSTAT=ierrm) time_strm,blat,blon,m_levels
     if(ierrm /= 0) exit SND
     call nearest_hour(time_strm,model_time_new)
     print*,'mod sndg. time, lat, lon: ',time_strm , blat, blon
     write(mtll,"(a14,2f7.2)") time_strm, blat, blon

     do k=1,m_levels
        read(model_unit,*) iprs_ps,junk,it_ps,junk,itd_ps,junk, &
                   iws_ps,junk,iwd_ps,junk

        if(iprs_ps > imissing) then
          prs_ps(k)=iprs_ps*0.01      ! now in mb
        else
          prs_ps(k)=missing
        endif

        if(it_ps > imissing) then
          t_ps(k)=it_ps*0.01          ! in K
        else
          t_ps(k)=missing
        endif

        if(itd_ps > imissing) then
          td_ps(k)=itd_ps*0.01        ! in K
        else
          td_ps(k)=missing
        endif

        if(iws_ps > imissing) then
          ws_ps(k)=iws_ps*0.01           ! in m/s
        else
          ws_ps(k)=missing
        endif

        wd_ps=iwd_ps*1.             ! in deg
        call qfromttd(t_ps(k),t_qc(k),td_ps(k),td_qc(k), &
                      prs_ps(k),prs_qc(k),qv,iqc,missing)
        q_ps(k)=qv

        u_ps(k)=-ws_ps(k)*sin(wd_ps(k)*pi/180.)
        v_ps(k)=-ws_ps(k)*cos(wd_ps(k)*pi/180.)
     enddo
     endif

     if(mtll > otll) then
      print*, "   Pass O -- Should not happen ... "
      print*, "       O ",atime_new,alat,alon
      print*, "       M ",model_time_new,blat,blon
      found = .TRUE.
      cycle SND
     endif

     found =.FALSE.
     ifound = 0

     if(mtll < otll) then
      print*,'   Pass M time, lat, lon: ',time_strm, blat, blon
      cycle MODEL
     endif

     print*,'--- Get pairs time, lat, lon: ',time_stro, alat, alon
     ifound = 1
     exit MODEL

     enddo MODEL

     if(ifound == 1) then

       call sig2p(t_ps,tm,prs_ps,2,m_levels,ks,missing)
       call sig2p(u_ps,um,prs_ps,1,m_levels,ks,missing)
       call sig2p(v_ps,vm,prs_ps,1,m_levels,ks,missing)
       call sig2p(q_ps,qm,prs_ps,1,m_levels,ks,missing)

       call vertical_interp(t_obs,t_oint,t_qc,t_oint_qc,prs_obs,ks, &
                            n_levels,missing,2)
       call vertical_interp(u_obs,u_oint,u_qc,u_oint_qc,prs_obs,ks, &
                            n_levels,missing,1)
       call vertical_interp(v_obs,v_oint,v_qc,v_oint_qc,prs_obs,ks, &
                            n_levels,missing,1)
       call vertical_interp(q_obs,q_oint,q_qc,q_oint_qc,prs_obs,ks, &
                            n_levels,missing,1)

       arg=(alon-xlonc)*xn*pi/180.
       do k=1,ks
          if(um(k) > missing .and. vm(k) > missing) then
            u=um(k)*cos(arg)+vm(k)*sin(arg)
            v=-um(k)*sin(arg)+vm(k)*cos(arg)
            wsm(k)=sqrt(um(k)**2+vm(k)**2)
            wdm(k)=(1.5*pi-atan2(v,u))*180./pi
            if(wdm(k) > 360.) wdm(k)=wdm(k)-360.
          else
            wsm(k)=missing
            wdm(k)=missing
          endif

          if(u_oint(k) > missing .and. v_oint(k) > missing) then
            ws_oint(k)=sqrt(u_oint(k)**2+v_oint(k)**2)
            wd_oint(k)=(1.5*pi-atan2(v_oint(k),u_oint(k)))*180./pi
            if(wd_oint(k) >  360.) wd_oint(k)=wd_oint(k)-360.
          else
            ws_oint(k)=missing
            wd_oint(k)=missing
          endif
          ws_oint_qc(k)=max(u_oint_qc(k),v_oint_qc(k))
          wd_oint_qc(k)=ws_oint_qc(k)

!         Calculate RH for both MM5 and Obs

          if(k == 1) then
            p=1010.
          else
            p=1000.-(k-2)*25.
          endif

          if(tm(k) > missing .and. qm(k) > missing) then
            rhm(k)=rhcalc(tm(k),qm(k),p)
          else
            rhm(k)=missing
          endif

          if(t_oint(k) > missing .and. q_oint(k) > missing) then
            rh_oint(k)=rhcalc(t_oint(k),q_oint(k),p)
          else
            rh_oint(k)=missing
          endif

          rh_oint_qc(k)=max(t_oint_qc(k),q_oint_qc(k))

       enddo

       icnt2=icnt2+1

       if(icnt2 == 1) open(out_unit,file='snd_pairs_domain'//adomain_id, &
                           status="REPLACE", &
                           access='direct', &
                           recl=64)

       irec=irec+1
       write(out_unit,rec=irec) atime_new, alat, alon, domain_id

       call write_snd_pairs1(out_unit,ks,tm,t_oint,t_oint_qc,qm,q_oint,q_oint_qc, &
                            rhm,rh_oint,rh_oint_qc,wsm,ws_oint,ws_oint_qc, &
                            wdm,wd_oint,wd_oint_qc,missing,irec)

     endif

  enddo SND

  end program snd_pairs
!
!
!
  function rhcalc(t,q,pm)
!
!   To ensure this function works properly, temperature "t" has to be in K,
!   whereas mixing ration "q" has to be in kg/kg (or g/g), and pressure "pm"
!   in mb.
! 
    es=10**(-2937.4/t-4.9283*log10(t)+23.5518)  !! in mb
    re=q/(1.-q)
    ee=pm*re/(0.622+re)
    rhcalc=ee/es*100.
    if(rhcalc < 0.) rhcalc=0.
    if(rhcalc > 100.) rhcalc=100.

  return
  end function rhcalc
