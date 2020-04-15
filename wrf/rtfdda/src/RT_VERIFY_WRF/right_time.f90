  subroutine right_time(time_str,icsize,jcsize,dtstring,iwin,iflag,iexceed,itime)
!
  integer :: icsize,jcsize,iwin,iflag,itime,iexceed,j
  character(len=14) :: time_str
  character(len=icsize), dimension(jcsize) :: dtstring
!
  integer :: year_o,month_o,day_o,hour_o,min_o
  integer :: year_m,month_m,day_m,hour_m,min_m
  integer, dimension(12) :: days
  integer*8 :: time_o
  integer*8, dimension(jcsize) :: time_m
!
  days=(/31,28,31,30,31,30,31,31,30,31,30,31/)

  read(time_str,'(i4,4i2)') year_o,month_o,day_o,hour_o,min_o

  if(mod(year_o,4) == 0) then
    if(mod(year_o,100) == 0) then
      if(mod(year_o,400) == 0) then
        days(2)=29
      endif
    else
      days(2)=29
    endif
  endif

  if(min_o >= iwin) then
    min_o=0
    hour_o = hour_o + 1
    if(hour_o >= 24) then
      hour_o = hour_o - 24
      day_o = day_o + 1
      if(day_o > days(month_o)) then
        day_o = day_o - days(month_o)
        month_o = month_o + 1
        if(month_o >= 12) then
          month_o = month_o -12
          year_o = year_o + 1
        endif
      endif
    endif
  elseif(min_o <= 10) then
    min_o=0
  endif
!
!
  if(min_o /= 0) then
    iflag=0
  endif
!
  time_o=year_o*1000000+month_o*10000+day_o*100+hour_o

  iflag=0
  itime=-99
  iexceed=0

  do j=1,jcsize
     read(dtstring,'(i4,4(x,i2))') year_m,month_m,day_m,hour_m,min_m
     time_m(j)=year_m*1000000+month_m*10000+day_m*100+hour_m
     if(time_o == time_m(j)) then
       itime=j
       iflag=1
       exit
     end if
  end do

! print*,'time_str = ',time_str,'  time_o = ',time_o
  if(time_o > time_m(jcsize)) iexceed=1

  return
  end subroutine right_time
