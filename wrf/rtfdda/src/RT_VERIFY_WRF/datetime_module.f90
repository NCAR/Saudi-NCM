MODULE DATETIME_MODULE

CONTAINS

  subroutine secs2datetime(i_secs,datetime)
  implicit none
!
  integer, dimension(12) :: mdays
  integer(kind=8),intent(in) :: i_secs
  integer(kind=8),intent(out) :: datetime
  integer(kind=8) :: n_secs
  integer :: year,month,date,hour,minute,second
  integer :: idays
!
  mdays=(/31,28,31,30,31,30,31,31,30,31,30,31/)
!
  year=1970 

  n_secs = i_secs

  do while (n_secs > 0)
!
    idays=365

    if(mod(year,100) == 0) then
      if(mod(year,400) == 0) then
        idays=366
      endif
    else
      if(mod(year,4) == 0) idays=366
    endif
!
    n_secs=n_secs-idays*86400
    if(n_secs < 0) exit
    year=year+1
  enddo

  if(n_secs < 0) n_secs=n_secs+idays*86400

!
  if(mod(year,100) == 0) then
    if(mod(year,400) == 0) then
      mdays(2)=29
    endif
  else
    if(mod(year,4) == 0) mdays(2)=29
  endif
!
  month=1
  do while (n_secs > 0)
     n_secs=n_secs-mdays(month)*86400

     if(n_secs < 0) exit
     month=month+1
  enddo

  if(n_secs < 0) n_secs=n_secs+mdays(month)*86400
!
  date=1
  do while (n_secs > 0)
     n_secs=n_secs-86400
     if(n_secs < 0) exit
     date=date+1
  enddo

  if(n_secs < 0) n_secs=n_secs+86400
!
  hour=0
  do while (n_secs > 0)
     n_secs=n_secs-3600
     if(n_secs < 0) exit
     hour=hour+1
  enddo

  if(n_secs < 0) n_secs=n_secs+3600
!
  minute=0
  do while (n_secs > 0)
     n_secs=n_secs-60
     if(n_secs < 0) exit
     minute=minute+1
  enddo

  if(n_secs < 0) then
    second=n_secs+60
  else
    second=n_secs
  end if

  datetime=year*100000000_8*100_8+month*100000000_8+date*1000000_8+hour*10000_8+minute*100_8+second
!
  return
  end subroutine secs2datetime
!
!
!
  integer function date2secs(datetime)

  implicit none

  integer :: datetime
  integer, dimension(12) :: mdays=(/31,28,31,30,31,30,31,31,30,31,30,31/)

  integer :: n_secs
  integer :: iy, im, iyLoopMax
  integer :: idays
  integer :: year, month, day, hour, minute=0, sec=0

  year = datetime/1000000
  month = mod(datetime,1000000)/10000
  day = mod(datetime,10000)/100
  hour = mod(datetime,100)

  n_secs = 0

  iyLoopMax = year - 1970

  do iy = 1,iyLoopMax

     idays = 365

     if (mod(iy+1969,100) == 0) then
        if (mod(iy+1969,400) == 0) idays = 366
     else
        if (mod(iy+1969,4) == 0) idays = 366
     end if

     n_secs = n_secs + idays*86400

  end do

  if (mod(year,100) == 0) then
     if (mod(year,400) == 0) mdays(2) = 29
  else
     if (mod(year,4) == 0) mdays(2) = 29
  end if

  do im = 1,month-1
     n_secs = n_secs + mdays(im)*86400
  end do

  n_secs = n_secs+(day-1)*86400+hour*3600+minute*60+sec

  date2secs = n_secs

  return

  end function date2secs

END MODULE DATETIME_MODULE
