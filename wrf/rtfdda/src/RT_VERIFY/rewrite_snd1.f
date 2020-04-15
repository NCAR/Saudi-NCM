!
!   THIS PROGRAM READS IN COMPLEX LITTLE_R FORMAT SOUNDINGS DATA AND WRITES
!   OUT DATA IN SIMPLE FORMAT. THE OUTPUT FOR EACH SOUNDING HAS A HEADER,
!   INCLUDING TIME TAG, LAT, LON, TOTAL NUMBER OF LEVELS, FOLLOWED BY DATA
!   AT EACH LEVEL. DATA FOR EACH LEVEL CONSIST OF THE FOLLOWING, ON THE SAME
!   RECORD:
!
!   PRESSURE (PA),
!   PRESSURE QC FLAG,
!   TEMPERATURE (X100 K),
!   TEMPERATURE QC FLAG,
!   DEW-POINT TEMPERATURE (X100 K),
!   DEW-POINT TEMPERATURE QC FLAG,
!   WIND SPEED (X100 M/S),
!   WIND SPEED QC FLAG,
!   WIND DIRECTION (DEG),
!   WIND DIRECTION QC FLAG,
!   RELATIVE HUMIDITY (%),
!   RELATIVE HUMIDITY QC FLAG
!   HEIGHT ABOVE MEAN SEA LEVEL (m)
!   HEIGTH ABOVE MEAN SEA LEVEL QC FLAG
!
    program rewrite_snd1
!
    USE HEADER_TYPES
    USE MEAS_TYPES
    USE LEVEL_RECORD1
 
    CHARACTER ( LEN = 120 ) , PARAMETER :: rpt_format =  &
                 ' ( 2f20.5 , 2a40 , ' &             ! format for location_type
              // ' 2a40 , 1f20.5 , 5i10 , 3L10 , ' & ! format for source_info
              // ' 2i10 , a20 , ' &                  ! fmt for valid_time
              // ' 13( f13.5 , i7 ) ) '              ! fmt for 'terrestrial'
 
    CHARACTER ( LEN = 120 ) , PARAMETER :: meas_format = &
                 ' ( 10( f13.5 , i7 ) ) '            ! fmt for measurement rcd
 
    CHARACTER ( LEN = 120 ) , PARAMETER :: end_format = &
                 ' ( 3 ( i7 ) ) '                    ! fmt for end record       

    CHARACTER(LEN=8) :: st_id

    INTEGER :: AllocateStatus

    TYPE(Header) :: head
    TYPE(meas) :: measure

    TYPE Level_node
       TYPE(Upr_record) :: data
       TYPE(Level_node), POINTER :: next
    END TYPE Level_node

    TYPE(Level_node), POINTER :: LevelList, TmpPtr

    character (len=80) :: exename,fn
    logical :: end

    character (len=14) :: time_str
    real :: lat, lon
    integer :: elev
    character (len=40) :: description

    integer :: qc

    real, parameter :: missing_long=-888888.,missing1_long=999999.
    integer, parameter :: missing=-8888,missing1=-9999
 
!   CHECK FOR NUMBER OF ARGUMENT; HAS TO BE ONE FOR THE INPUT FILENAME

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
 
!   Initialize the pointer to null
 
    nullify(LevelList)
!
    read(21,rpt_format,iostat=ierr) head

    do while (ierr == 0)
 
       time_str=head%time_tag%time_str(7:20)
       lat=head%loc%lat
       lon=head%loc%lon
       st_id=head%loc%desc1(1:8)
       elev=nint(head%name_src%elev)
       description=head%loc%desc2

!      print*,head%time_tag%time_str(7:20)
       end=.FALSE.                                                              

       n_levels=0
       do while (.not. end)
          read(21,meas_format) measure
          if(measure%obs(1)%val == -777777.0 .or. &
             measure%obs(1)%qc_flag == -777777) then
            end=.TRUE.
            read(21,*)
            read(21,rpt_format,iostat=ierr) head
          else
            allocate(TmpPtr, STAT=AllocateStatus)
            if(AllocateStatus /= 0) stop "*** Not enough memory ***"

            n_levels = n_levels+1

            do i=1,9
               if(measure%obs(i)%qc_flag >= 32768) then
                 if(measure%obs(i)%qc_flag == 400000) then
                    measure%obs(i)%qc_flag=32767
                 else
                   qc=mod(measure%obs(i)%qc_flag,100)
                   if(qc >= 0 .and. qc <= 10) then
                     measure%obs(i)%qc_flag=qc
                   else
                     measure%obs(i)%qc_flag=0
                   end if
                 end if
               end if
            end do

            if(measure%obs(1)%val == missing_long) then
              TmpPtr%data%pressure=missing
            else if(measure%obs(1)%val == missing1_long) then
              TmpPtr%data%pressure=missing
            else
              TmpPtr%data%pressure=nint(measure%obs(1)%val)
            endif
            TmpPtr%data%pressure_qc=abs(measure%obs(1)%qc_flag)

            if(measure%obs(2)%val == missing_long) then
              TmpPtr%data%zamsl=missing
            else if(measure%obs(2)%val == missing1_long) then
              TmpPtr%data%zamsl=missing
            else
              TmpPtr%data%zamsl=nint(measure%obs(2)%val)
            endif
            TmpPtr%data%zamsl_qc=abs(measure%obs(2)%qc_flag)

            if(measure%obs(3)%val == missing_long) then
              TmpPtr%data%temp=missing
            else if(measure%obs(3)%val == missing1_long) then
              TmpPtr%data%temp=missing
            else
              TmpPtr%data%temp=nint(measure%obs(3)%val*100.)
            endif
            TmpPtr%data%temp_qc=abs(measure%obs(3)%qc_flag)

            if(measure%obs(4)%val == missing_long) then
              TmpPtr%data%td=missing
            else if(measure%obs(4)%val == missing1_long) then
              TmpPtr%data%td=missing
            else
              TmpPtr%data%td=nint(measure%obs(4)%val*100.)
            endif
            TmpPtr%data%td_qc=abs(measure%obs(4)%qc_flag)

            if(measure%obs(5)%val == missing_long) then
              TmpPtr%data%ws=missing
            else if(measure%obs(5)%val == missing1_long) then
              TmpPtr%data%ws=missing
            else
              TmpPtr%data%ws=nint(measure%obs(5)%val*100.)
            endif
            TmpPtr%data%ws_qc=abs(measure%obs(5)%qc_flag)

            if(measure%obs(6)%val == missing_long) then
              TmpPtr%data%wd=missing
            else if(measure%obs(6)%val == missing1_long) then
              TmpPtr%data%wd=missing
            else
              TmpPtr%data%wd=measure%obs(6)%val
            endif
            TmpPtr%data%wd_qc=abs(measure%obs(6)%qc_flag)

            if(measure%obs(9)%val == missing_long) then
              TmpPtr%data%rh=missing
            else if(measure%obs(9)%val == missing1_long) then
              TmpPtr%data%rh=missing
            else
              TmpPtr%data%rh=nint(measure%obs(9)%val)
            endif
            TmpPtr%data%rh_qc=abs(measure%obs(9)%qc_flag)
!
! The following added to correct the problem with wind QC flag! 05/15/2002
!
            TmpPtr%data%ws_qc=min(TmpPtr%data%ws_qc,TmpPtr%data%wd_qc, &
                   abs(measure%obs(7)%qc_flag),abs(measure%obs(8)%qc_flag))
            TmpPtr%data%wd_qc=TmpPtr%data%ws_qc
!
! When zamsl is not QC'd (vertical interpolation coordinate is height), then
! zamsl takes the QC value of pressure.
!
            TmpPtr%data%zamsl_qc=min(TmpPtr%data%zamsl_qc,TmpPtr%data%pressure_qc)

            TmpPtr%next => LevelList
            LevelList => TmpPtr
          endif
       enddo

!
!      write out only the standard time at 00 and 12 UTC
!
!      if(time_str(9:10) == '00' .or. time_str(9:10) == '12') then
!      if(index(description,'PROFILER') == 0 .and. &
       if(index(description,'SATWNDS') == 0 .and. &
          index(description,'AMV') == 0 .and. &
          index(description,'ACARS') == 0 ) then
!        write(61,'(a14,f8.3,f9.3,i6,i4)') time_str,lat,lon,elev,n_levels
         write(61,'(a14,x,a8,f8.3,f9.3,i10,i5)') time_str,st_id,lat,lon,elev,n_levels

         do while (associated(LevelList))
            write(61,'(i7,i7,6(i6,i7))') LevelList%data
!           print*, LevelList%data
            LevelList => LevelList%next
         enddo
       endif
!      endif

       deallocate(TmpPtr)
       nullify(LevelList)
    enddo

    end program rewrite_snd1
