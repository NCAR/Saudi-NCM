MODULE time_routines

CONTAINS

  subroutine adjust_hour(atime,iadd_hr,atime_new)

  implicit none

  character (len=10) :: atime
  character (len=10) :: atime_new
  character (len=4) :: ayear
  character (len=3) :: amonth,aday,ahour
  integer :: iadd_hr

  integer, dimension(12) :: days=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  integer :: year,month,day,hour,minute,sec

  read(atime,'(i4,3i2)') year,month,day,hour

  if(mod(year,400) == 0) then
    days(2)=29
  else
    if((mod(year,4) == 0) .and. (mod(year,100) /= 0)) days(2)=29
  endif

  hour=hour+iadd_hr
  do while (hour >= 24)
    hour=hour-24
    day=day+1;
    do while (day > days(month))
      day=day-days(month)
      month=month+1
      do while (month > 12)
        month=month-12
        year=year+1
      end do
    end do
  end do

  do while (hour < 0)
    hour=hour+24
    day=day-1
    do while (day < 1)
       if(month == 1) then
         day=day+days(12)
         month=12
       else
         day=day+days(month-1)
         month=month-1
       end if

       do while (month < 1)
          month=month+12
          year=year-1
       end do
    end do
  end do

  write(ayear,'(i4)') year

  month=100+month
  write(amonth,'(i3)') month

  day=100+day
  write(aday,'(i3)') day

  hour=100+hour
  write(ahour,'(i3)') hour

  atime_new=ayear//amonth(2:3)//aday(2:3)//ahour(2:3)

  return

  end subroutine adjust_hour


END MODULE time_routines
