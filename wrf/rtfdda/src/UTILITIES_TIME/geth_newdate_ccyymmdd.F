program geth_newdate_main

! geth_newdate.csh ccyy-mm-dd(_hh:mn:ss) +/-tincr
!
! Find the new date given the old date and the time increment.
! The time increment is expressed in terms of the date unit (from day to second)
! For instance:
!
!  geth_newdate.exe 20010619       +2 = 20010621           (days)
!  geth_newdate.exe 2001061900     +2 = 2001061902        (hours)
!  geth_newdate.exe 200106190000   +2 = 200106190002    (minutes)
!  geth_newdate.exe 20010619000000 +2 = 20010619000002  (seconds)
! 
! Routine translates date in yyyymmddhhmmss to yyyy-mm-dd_hh:mm:ss
!  before calling geth_newdate. Output is in same yyyymmddhhmmss 
!  format

  implicit none
  
  character (len=14) :: ndate
  character (len=10) :: cidt
  character (len=19) :: start_date,end_date
  integer :: idt,date_len
  integer, external :: iargc

  if (iargc() < 2) then
     print *,'Usage:'
     print *,'  geth_newdate requires two arguments'
     print *,'  geth_newdate date idt'
     stop
  end if
  
  call getarg(1,ndate)
  call getarg(2,cidt)
  read(cidt,'(i20)') idt
!  print *,idt

  date_len = len_trim(ndate)
!  print *,"date is ",date_len," charecters long"

  if (date_len.eq.14) then
     start_date = ndate(1:4)//"-"//ndate(5:6)//"-"//ndate(7:8)//"_"//ndate(9:10) &
          //":"//ndate(11:12)//":"//ndate(13:14)
  else if (date_len.eq.12) then
     start_date = ndate(1:4)//"-"//ndate(5:6)//"-"//ndate(7:8)//"_"//ndate(9:10) &
          //":"//ndate(11:12)
  else if (date_len.eq.10) then
     start_date = ndate(1:4)//"-"//ndate(5:6)//"-"//ndate(7:8)//"_"//ndate(9:10)
  else if (date_len.eq.8) then
     start_date = ndate(1:4)//"-"//ndate(5:6)//"-"//ndate(7:8)
  else
     write(6,*) "Error, weird date"
     stop
  end if
  call geth_newdate(end_date,trim(start_date),idt)
!  print *,start_date,"  ",end_date
  
  if (date_len.eq.14) then
     ndate = end_date(1:4)//end_date(6:7)//end_date(9:10)//end_date(12:13) &
          //end_date(15:16)//end_date(18:19)
  else if (date_len.eq.12) then
     ndate = end_date(1:4)//end_date(6:7)//end_date(9:10)//end_date(12:13) &
          //end_date(15:16)
  else if (date_len.eq.10) then
     ndate = end_date(1:4)//end_date(6:7)//end_date(9:10)//end_date(12:13)
  else if (date_len.eq.8) then
     ndate = end_date(1:4)//end_date(6:7)//end_date(9:10)
  end if
  write(6,'(a)') ndate

end program geth_newdate_main
   

