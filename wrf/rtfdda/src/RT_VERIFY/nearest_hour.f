subroutine nearest_hour(atime,atime_new)

  character (len=14) :: atime
  character (len=10) :: atime_new
  character (len=4) :: ayear
  character (len=3) :: amonth,aday,ahour

  integer, dimension(12) :: days=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  integer :: year,month,day,hour,minute,sec

  read(atime,'(i4,5i2)') year,month,day,hour,minute,sec

  if(mod(year,400) == 0) then
    days(2)=29
  else
    if((mod(year,4) == 0) .and. (mod(year,100) /= 0)) days(2)=29
  endif

  if(sec >= 30) then
    minute=minute+1
    if(minute >= 60) then
      minute=minute-60
      hour=hour+1
      if(hour >= 24) then
        hour=hour-24
        day=day+1;
        if(day > days(month)) then
          day=day-days(month)
          month=month+1
          if(month > 12) then
            month=month-12
            year=year+1
          endif
        endif
      endif
    endif
  endif

  if(minute >= 30) then
    minute = 0
    hour=hour+1
    if(hour >= 24) then
      hour=hour-24
      day=day+1
      if(day > days(month)) then
        day=day-days(month)
        month=month+1
        if(month > 12) then
          month=month-12
          year=year+1
        endif
      endif
    endif
  endif

  write(ayear,'(i4)') year

  month=100+month
  write(amonth,'(i3)') month

  day=100+day
  write(aday,'(i3)') day

  hour=100+hour
  write(ahour,'(i3)') hour

  atime_new=ayear//amonth(2:3)//aday(2:3)//ahour(2:3)

  return

end subroutine nearest_hour
