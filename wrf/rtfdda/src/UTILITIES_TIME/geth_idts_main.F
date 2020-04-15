program geth_idts_main

  implicit none
  character (len=10) :: date1,date2,aux
  character (len=19) :: start_date,end_date
  integer :: idt,date_len
  integer, external :: iargc

  if (iargc() < 2) then
     print *,'Usage:'
     print *,'  geth_idts.exe requires two arguments'
     print *,'  geth_idts.exe begin_date end_date'
     stop
  end if

  call getarg(1,date1)
  call getarg(2,date2)
  if (date1 > date2) then
     aux = date2
     date2 = date1
     date1 = aux
  end if

  date_len = len_trim(date1)
  if (date_len.eq.10) then
     start_date = date1(1:4)//"-"//date1(5:6)//"-"//date1(7:8)//"_"//date1(9:10)
  else
     write(6,*) "Error, weird date"
     stop
  end if

  date_len = len_trim(date2)
  if (date_len.eq.10) then
     end_date = date2(1:4)//"-"//date2(5:6)//"-"//date2(7:8)//"_"//date2(9:10)
  else
     write(6,*) "Error, weird date"
     stop
  end if

  call geth_idts(trim(end_date),trim(start_date),idt)
  write(6,'(i)') idt

end program geth_idts_main
