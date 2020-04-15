    program rewrite_obs
!
    USE HEADER_TYPES
    USE MEAS_TYPES

    CHARACTER ( LEN = 120 ) , PARAMETER :: rpt_format =  &
                 ' ( 2f20.5 , 2a40 , ' &             ! format for location_type
              // ' 2a40 , 1f20.5 , 5i10 , 3L10 , ' & ! format for source_info
              // ' 2i10 , a20 , ' &                  ! fmt for valid_time
              // ' 13( f13.5 , i7 ) ) '              ! fmt for 'terrestrial' 

    CHARACTER ( LEN = 120 ) , PARAMETER :: meas_format = & 
                 ' ( 10( f13.5 , i7 ) ) '            ! fmt for measurement rcd

    CHARACTER ( LEN = 120 ) , PARAMETER :: end_format = &
                 ' ( 3 ( i7 ) ) '                    ! fmt for end record

    character (len = 120), parameter :: outfmt1 = &
              '(a14,f8.3,f9.3,x,a4,2(f9.2,i7),f9.2,5(f9.2,i7))'

    character (len = 120), parameter :: outfmt2 = &
              '(a14,f8.3,f9.3,x,a4,2(f9.2,i7),f9.2,5(f9.2,i7),x,a8)'

    real, parameter :: missing=-8888.
    integer :: slp_qc,psfc_qc,t_qc,td_qc,ws_qc,wd_qc,u_qc,v_qc,rh_qc,qc
    character(len=3) :: sams_id
    character(len=1) :: sams_type ! either 'S' for SAMS or 'M' for miniSAMS
    character(len=4) :: pltf
    character(len=80) :: arg
    character(len=8) :: stid

 
    TYPE(Header) :: head
    TYPE(meas) :: measure

    character (len=80) :: exename,fn
    logical :: end
    logical :: laddstid = .false.

    j=iargc()
    call getarg(0,exename)
    l=index(exename," ")

    if(j == 0) then
      print*,'Usage: ',exename(1:l-1),' filename [-add_stid]'
      stop
    endif
!
    call getarg(1,fn)
    open(21,file=fn,status='old') 
!   open(31,file='obs.dat')

    if (j == 2) then
       call getarg(2,arg)
       if (index(arg,'-add_stid') > 0) then
          laddstid = .true.
       else
          print*,'Unrecognized argument: ',trim(arg)
          stop
       end if
    end if
!
    read(21,rpt_format,iostat=ierr) head
!   print*,'ierr= ',ierr
!   print*,head%loc%lat,head%loc%lon
!   print*,head%ground%obs(5)%val
    stid = head%loc%desc1(1:8)

    do while (ierr == 0)

!      print*,head%time_tag%time_str(7:20)
       end=.FALSE.
       do while (.not. end)
          read(21,meas_format) measure
          if(measure%obs(1)%val == -777777.0 .and. &
             measure%obs(1)%val == -777777.) then
            end=.TRUE.
            read(21,*)
            read(21,rpt_format,iostat=ierr) head
            stid = head%loc%desc1(1:8)
          else
! yliu add qc missing value conversion -- 05/15/03
            p=measure%obs(1)%val

            if(measure%obs(1)%qc_flag > 0) then
               p_qc=mod(measure%obs(1)%qc_flag,100)
            else
               p_qc=missing
            end if

            if(p <= -888888.) p=missing
             if(p_qc <= -888888.) p_qc=missing

            h=measure%obs(2)%val

            if ( h > -888888.) then
               h_qc = 10
            else
               h = missing
               h_qc = missing
            endif

            t=measure%obs(3)%val
            t_qc=measure%obs(3)%qc_flag
            if(t <= -888888.) t=missing
             if(t_qc <= -888888.) t_qc=missing
            td=measure%obs(4)%val
            td_qc=measure%obs(4)%qc_flag
            if(td <= -888888.) td=missing
             if(td_qc <= -888888.) td_qc=missing
            ws=measure%obs(5)%val
            ws_qc=measure%obs(5)%qc_flag
            if(ws <= -888888.) ws=missing
             if(ws_qc <= -888888.) ws_qc=missing
            wd=measure%obs(6)%val
            wd_qc=measure%obs(6)%qc_flag
            if(wd <= -888888.) wd=missing
             if(wd_qc <= -888888.) wd_qc=missing
!
! The following added to correct the problem of wind QC flag. 05/15/2002
!
            u_qc=measure%obs(7)%qc_flag
            v_qc=measure%obs(8)%qc_flag
            ws_qc=max(ws_qc,wd_qc,u_qc,v_qc)
            wd_qc=ws_qc
!
            rh=measure%obs(9)%val
            rh_qc=measure%obs(9)%qc_flag
            if(rh <= -888888.) rh=missing
             if(rh_qc <= -888888.) rh_qc=missing
!

            z=head%name_src%elev

            if (( h <= missing ) .and. (z > -888888. )) then
                  h = z
                  h_qc = 10
            endif

            slp=head%ground%obs(1)%val
            slp_qc=head%ground%obs(1)%qc_flag
            psfc=measure%obs(1)%val
!           psfc_qc=measure%obs(1)%qc_flag
            psfc_qc=min(p_qc,h_qc)
! RSS removes the following pracitce: 5/14/09
!! yliu add 05/15/03
!!    head%ground%obs(5)%val appear problematic, not used it  
!!    allow to be calculated from "slp" or take from "p"
!            psfc= -888888.
!            psfc_qc = -888888.

! RSS adds the following slp check: 5/14/09

!           if(slp < 15000.) then  !! bad slp's need to be tossed out
!           forcing slp to be re-calculated from psfc: RSS 5/15/09
            slp = -888888.
            slp_qc = -888888.
!           endif

!   sometimes, the elevation is recorded in the obs part, not the header
!    So the sfc pressure; pslp and psfcslp appears messed-up with small difference

!           if (z <= -888888.) z = h
            if (slp <= -888888.) then
!            slp = psfcslp                    !?????
!            slp_qc = psfcslp_qc              !?????
            endif
! yliu end
             if(u_qc <= -888888.) u_qc=missing
             if(v_qc <= -888888.) v_qc=missing
             if(slp_qc <= -888888.) slp_qc=missing
             if(psfc_qc <= -888888.) psfc_qc=missing
!
!           The following if block added 10/5/00.
!           If it's SAMS station, psfc should be treated differently
!
            if(index(head%loc%desc2,'SAMS') > 0) then
              psfc=measure%obs(1)%val
              psfc_qc=measure%obs(1)%qc_flag
              if(psfc_qc <= -888888.) psfc_qc=missing
            endif
!            print *, "ps p_qc slp psfcslp",psfc,p_qc,slp,psfcslp
!
            call pressure(slp,psfc,h,t,slp_qc,psfc_qc,t_qc, &
                          head%name_src%platform,missing)
! yliu 
            if ( psfc <= missing) then
             psfc = p            ! from the model based on height -- not the obs
             psfc_qc = min(p_qc,h_qc)      ! 
             ! one more attempt to get slp: RSS 5/14/2009
             if (slp <= missing) then
                call pressure(slp,psfc,h,t,slp_qc,psfc_qc,t_qc, &
                              head%name_src%platform,missing)
             endif
            endif
! yliu end
            if(slp > 0.) slp=slp*0.01
            if(psfc > 0.) psfc=psfc*0.01
!            print *, "ps p_qc slp psfcslp",psfc,p_qc,slp,psfcslp

!           if(index(head%loc%desc2,'SAMS') > 0) then       commented out
!             pltf='SAMS'                                   commented out
!           elseif(index(head%name_src%platform,'METAR') > 0) then commented out
            if(index(head%name_src%platform,'METAR') > 0) then
              pltf='METR'
            elseif(index(head%name_src%platform,'SYNOP') > 0) then
              if(index(head%loc%desc2,'SAMS ATEC') > 0 ) then
                call check_sams(head%loc%desc2,sams_id,sams_type)
                pltf=sams_type//sams_id
              else
                pltf='SYNP'
              endif
            elseif(index(head%name_src%platform,'SPECI') > 0) then
              pltf='SPEC'
            elseif(index(head%name_src%platform,'SHIP') > 0) then
              pltf='SHIP'
            else
              pltf='OTHR'
            endif
 
!yliu special flagged data
            if(abs(psfc_qc-666666) < 1 .or. abs(slp_qc -666666) < 1 .or. &
               abs(t_qc -666666) < 1   .or. abs(td_qc -666666) < 1  .or. &
               abs(ws_qc -666666) < 1  .or. abs(wd_qc -666666) < 1 ) then
              psfc_qc=26666
              slp_qc=26666
              t_qc=26666
              td_qc=26666
              ws_qc=26666
              wd_qc=26666
            endif
!yliu
            if(psfc_qc >= 32768 ) then
              if(psfc_qc == 400000) then
                psfc_qc=32767
              else
                qc=psfc_qc-int(psfc_qc/100)*100
                if(qc >= 0 .and. qc <= 10) then
                  psfc_qc=qc
                else
                  psfc_qc=32767
                end if
              end if
            end if

            if(slp_qc >= 32768 ) then
              if(slp_qc == 400000) then
                slp_qc=32767
              else
                qc=slp_qc-int(slp_qc/100)*100
                if(qc >= 0 .and. qc <= 10) then
                  slp_qc=qc
                else
                  slp_qc=32767
                end if
              end if
            end if

            if(t_qc >= 32768 ) then
              if(t_qc == 400000) then
                t_qc=32767
              else
                qc=t_qc-int(t_qc/10)*10
                if(qc >= 0 .and. qc <= 10) then
                  t_qc=qc
                else
                  t_qc=32767
                end if
              end if
            end if

            if(td_qc >= 32768 ) then
              if(td_qc == 400000) then
                td_qc=32767
              else
                qc=td_qc-int(td_qc/10)*10
                if(qc >= 0 .and. qc <= 10) then
                  td_qc=qc
                else
                  td_qc=32767
                end if
              end if
            end if

            if(td_qc <= -800000) td_qc=32767       ! Rong

            if(ws_qc >= 32768 ) then
              if(ws_qc == 400000) then
                ws_qc=32767
              else
                qc=ws_qc-int(ws_qc/10)*10
                if(qc >= 0 .and. qc <= 10) then
                  ws_qc=qc
                else
                  ws_qc=32767
                end if
              end if
            end if

            if(wd_qc >= 32768 ) then
              if(wd_qc == 400000) then
                wd_qc=32767
              else
                qc=wd_qc-int(wd_qc/10)*10
                if(qc >= 0 .and. qc <= 10) then
                  wd_qc=qc
                else
                  wd_qc=32767
                end if
              end if
            end if

!
!           RSS Add 12/17/2009
!
            if(t_qc > 10 .or. t_qc < 0) t_qc=int(missing)
            if(td_qc > 10 .or. td_qc < 0) td_qc=int(missing)
            if(ws_qc > 10 .or. ws_qc < 0) ws_qc=int(missing)
            if(wd_qc > 10 .or. wd_qc < 0) wd_qc=int(missing)
            if(psfc_qc > 10 .or. psfc_qc < 0) psfc_qc=int(missing)
            if(slp_qc > 10 .or. slp_qc < 0) slp_qc=int(missing)

            !print *, head%time_tag%time_str(7:20),head%loc%lat
            if (laddstid) then

              if (trim(stid) == 'UTAA') stid = '1101691 '
              if (trim(stid) == 'LTFJ') stid = '1101650 '

              write(31,outfmt2) head%time_tag%time_str(7:20),head%loc%lat, &
                              head%loc%lon,pltf, &
                              psfc,psfc_qc, &
                              slp,slp_qc, &
                              h, &
                              t,t_qc, &
                              td,td_qc, &
                              ws,ws_qc, &
                              wd,wd_qc, &
                              rh,rh_qc, &
                              stid
            else
              write(31,outfmt1) head%time_tag%time_str(7:20),head%loc%lat, &
                              head%loc%lon,pltf, &
                              psfc,psfc_qc, &
                              slp,slp_qc, &
                              h, &
                              t,t_qc, &
                              td,td_qc, &
                              ws,ws_qc, &
                              wd,wd_qc, &
                              rh,rh_qc
            end if
          end if
       enddo

    enddo

    end program rewrite_obs
!
!
!
  subroutine pressure(slp,psfc,elev,t,slp_qc,psfc_qc,t_qc,platform,missing)
!
  real :: missing
  character(len=40) :: platform
  real, parameter :: utlpr=0.0065,g=9.8,r=287.05
  integer :: slp_qc,psfc_qc,t_qc

 !if((index(platform,'METAR') == 0) .and. (index(platform,'SPECI') == 0)) then
  if(index(platform,'SPECI') == 0) then
    if(psfc > 0.) then
      if(slp > 0.) then
        return
      else
        if((t > missing) .and. (elev > missing)) then
          tslv=t+elev*utlpr
          slp=psfc*exp(elev*g/r/(0.5*(t+tslv)))
          if (slp > 120000. .or. slp < 85000.) then
             slp=missing
          else
             slp_qc=max(t_qc,psfc_qc)
          endif
        else
          slp=missing
        endif
      endif
    else
      if(slp < 0.) then
        slp=missing
        psfc=missing
      else
        if((t > missing) .and. (elev > missing)) then
          tslv=t+elev*utlpr
          psfc=slp*exp(-elev*g/r/(0.5*(t+tslv)))
          psfc_qc=max(t_qc,slp_qc)
        else
          psfc=missing
        endif
      endif
    endif

  else

    if(psfc > 0.) then      !! altimeter setting

      if(slp > 0.) then
        if((t > missing) .and. (elev > missing)) then
          dz=(r*288.16/g)*(alog(101360.)-alog(psfc))
          z0=elev+dz
          psfc=slp*exp(-g*z0/r/t)+(101360.*0.01*2.54/76.)
          psfc_qc=max(t_qc,slp_qc)
        else
          psfc=missing
        endif
      else
        psfc=missing
        slp=missing
      endif

    else

      if(slp < 0.) then
        slp=missing
        psfc=missing
      else
        if((t > missing) .and. (elev > missing)) then
          tslv=t+elev*utlpr
          psfc=slp*exp(-elev*g/r/(0.5*(t+tslv)))
          psfc_qc=max(t_qc,slp_qc)
        else
          psfc=missing
        endif
      endif
    endif
  endif
      
  return

  end subroutine pressure
!
!
!
  subroutine check_sams(sams_string,sams_id,sams_type)
  implicit none
  character(len=3) :: sams_id
  character(len=40) :: sams_string
  character(len=4) :: sams_station
  character(len=1) :: sams_type
  integer :: isams
  integer :: iunderscore_s,iunderscore_s_uc,iunderscore_m,iunderscore_m_uc
  integer :: iunderscore

  iunderscore_s=index(sams_string,'_s')
  iunderscore_s_uc=index(sams_string,'_S')
  iunderscore_m=index(sams_string,'_m')
  iunderscore_m_uc=index(sams_string,'_M')
  iunderscore=max(iunderscore_s,iunderscore_s_uc,iunderscore_m,iunderscore_m_uc)
  if (iunderscore > 0) then
     read(sams_string(iunderscore:40),'(2x,i3)') isams
  else
     isams=999
  end if

  write(sams_station,'(i4)') 1000+isams

  sams_id=sams_station(2:4)

  if (iunderscore_s > 0 .or. iunderscore_s_uc > 0) then
     sams_type='S'
  else
     sams_type='M'
  end if

  return

  end subroutine check_sams
