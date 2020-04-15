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

    character (len = 120), parameter :: outfmt = &
              '(a14,f8.3,f9.3,x,a4,2(f9.2,i7),f9.2,4(f9.2,i7))'

    real, parameter :: missing=-8888.
    integer :: slp_qc,psfc_qc,t_qc,td_qc,ws_qc,wd_qc
    character(len=4) :: pltf

 
    TYPE(Header) :: head
    TYPE(meas) :: measure

    character (len=80) :: exename,fn
    logical :: end

    j=iargc()
    call getarg(0,exename)
    l=index(exename," ")

    if(j == 0) then
      print*,'Usage: ',exename(1:l-1),' filename'
      stop
    endif
!
    call getarg(1,fn)
    open(21,file=fn,status='old') 
!   open(31,file='obs.dat')
!
    read(21,rpt_format,iostat=ierr) head
!   print*,'ierr= ',ierr
!   print*,head%loc%lat,head%loc%lon
!   print*,head%ground%obs(5)%val
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
          else
            if(p <= -888888.) p=missing
            h=measure%obs(2)%val
            h_pc=measure%obs(2)%qc_flag
            if(h <= -888888.) h=missing
            t=measure%obs(3)%val
            t_qc=measure%obs(3)%qc_flag
            if(t <= -888888.) t=missing
            td=measure%obs(4)%val
            td_qc=measure%obs(4)%qc_flag
            if(td <= -888888.) td=missing
            ws=measure%obs(5)%val
            ws_qc=measure%obs(5)%qc_flag
            if(ws <= -888888.) ws=missing
            wd=measure%obs(6)%val
            wd_qc=measure%obs(6)%qc_flag
            if(wd <= -888888.) wd=missing

            z=head%name_src%elev
            slp=head%ground%obs(1)%val
            slp_qc=head%ground%obs(1)%qc_flag
            psfc=head%ground%obs(5)%val
            psfc_qc=head%ground%obs(5)%qc_flag

            call pressure(slp,psfc,z,t,slp_qc,psfc_qc,t_qc, &
                          head%name_src%platform,missing)
            if(slp > 0.) slp=slp*0.01
            if(psfc > 0.) psfc=psfc*0.01

            if(index(head%name_src%platform,'SAMS') > 0) then
              pltf='SAMS'
            elseif(index(head%name_src%platform,'METAR') > 0) then
              pltf='METR'
            elseif(index(head%name_src%platform,'SYNOP') > 0) then
              pltf='SYNP'
            elseif(index(head%name_src%platform,'SPECI') > 0) then
              pltf='SPEC'
            elseif(index(head%name_src%platform,'SHIP') > 0) then
              pltf='SHIP'
            else
              pltf='OTHR'
            endif
 
            if(psfc_qc >= 32768 ) psfc_qc=32767
            if(slp_qc >= 32768) slp_qc=32767
            if(t_qc >= 32768) t_qc=32767
            if(td_qc >= 32768) td_qc=32767
            if(ws_qc >= 32768) ws_qc=32767
            if(wd_qc >= 32768) wd_qc=32767

            write(31,outfmt) head%time_tag%time_str(7:20),head%loc%lat, &
                            head%loc%lon,pltf, &
                            psfc,psfc_qc, &
                            slp,slp_qc, &
                            z, &
                            t,t_qc, &
                            td,td_qc, &
                            ws,ws_qc, &
                            wd,wd_qc
          endif
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

  if((index(platform,'METAR') == 0) .and. (index(platform,'SPECI') == 0)) then
    if(psfc > 0.) then
      if(slp > 0.) then
        return
      else
        if((t > -888888.) .and. (elev > -888888.)) then
          tslv=t+h*utlpr
          slp=psfc*exp(elev*g/r/(0.5*(t+tslv)))
          slp_qc=max(t_qc,psfc_qc)
        else
          slp=missing
        endif
      endif
    else
      if(slp < 0.) then
        slp=missing
        psfc=missing
      else
        if((t > -888888.) .and. (elev > -888888.)) then
          tslv=t+h*utlpr
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
        if((t > -888888.) .and. (elev > -888888.)) then
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
        if((t > -888888.) .and. (elev > -888888.)) then
          tslv=t+h*utlpr
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
