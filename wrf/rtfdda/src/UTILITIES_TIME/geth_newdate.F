subroutine geth_newdate (ndate, odate, idt)
  implicit none

!  From old date ('YYYY-MM-DD HH:MM:SS.ffff') and 
!  delta-time, compute the new date.

!  on entry     -  odate  -  the old hdate.
!                  idt    -  the change in time

!  on exit      -  ndate  -  the new hdate.

  integer, intent(in)           :: idt
  character (len=*), intent(out) :: ndate
  character (len=*), intent(in)  :: odate


!  Local Variables

!  yrold    -  indicates the year associated with "odate"
!  moold    -  indicates the month associated with "odate"
!  dyold    -  indicates the day associated with "odate"
!  hrold    -  indicates the hour associated with "odate"
!  miold    -  indicates the minute associated with "odate"
!  scold    -  indicates the second associated with "odate"

!  yrnew    -  indicates the year associated with "ndate"
!  monew    -  indicates the month associated with "ndate"
!  dynew    -  indicates the day associated with "ndate"
!  hrnew    -  indicates the hour associated with "ndate"
!  minew    -  indicates the minute associated with "ndate"
!  scnew    -  indicates the second associated with "ndate"

!  mday     -  a list assigning the number of days in each month

!  i        -  loop counter
!  nday     -  the integer number of days represented by "idt"
!  nhour    -  the integer number of hours in "idt" after taking out
!              all the whole days
!  nmin     -  the integer number of minutes in "idt" after taking out
!              all the whole days and whole hours.
!  nsec     -  the integer number of minutes in "idt" after taking out
!              all the whole days, whole hours, and whole minutes.

  integer :: nlen, olen
  integer :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
  integer :: yrold, moold, dyold, hrold, miold, scold, frold
  integer :: mday(12), nday, nhour, nmin, nsec, nfrac, i, ifrc
  logical :: opass
  character (len=10) :: hfrc
  character (len=1) :: sp
  integer, external :: nfeb

!  Assign the number of days in a months

  mday( 1) = 31
  mday( 2) = 28
  mday( 3) = 31
  mday( 4) = 30
  mday( 5) = 31
  mday( 6) = 30
  mday( 7) = 31
  mday( 8) = 31
  mday( 9) = 30
  mday(10) = 31
  mday(11) = 30
  mday(12) = 31

!  Break down old hdate into parts

  hrold = 0
  miold = 0
  scold = 0
  frold = 0
  olen = LEN(odate)
  if (olen.ge.11) then
     sp = odate(11:11)
  else
     sp = ' '
  end if

!  Use internal READ statements to convert the CHARACTER string
!  date into INTEGER components.

  read(odate(1:4),  '(i4)') yrold
  read(odate(6:7),  '(i2)') moold
  read(odate(9:10), '(i2)') dyold
  if (olen.ge.13) then
     read(odate(12:13),'(i2)') hrold
     if (olen.ge.16) then
        read(odate(15:16),'(i2)') miold
        if (olen.ge.19) then
           read(odate(18:19),'(i2)') scold
           if (olen.gt.20) then
              read(odate(21:olen),*) frold
           end if
        end if
     end if
  end if

!  Set the number of days in February for that year.

  mday(2) = nfeb(yrold)

!  Check that ODATE makes sense.

  opass = .TRUE.

!  Check that the month of ODATE makes sense.

  if ((moold.gt.12).or.(moold.lt.1)) then
     write(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
     opass = .FALSE.
  end if

!  Check that the day of ODATE makes sense.

  if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then
     write(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
     opass = .FALSE.
  end if

!  Check that the hour of ODATE makes sense.

  if ((hrold.gt.23).or.(hrold.lt.0)) then
     write(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
     opass = .FALSE.
  end if

!  Check that the minute of ODATE makes sense.

  if ((miold.gt.59).or.(miold.lt.0)) then
     write(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
     opass = .FALSE.
  end if

!  Check that the second of ODATE makes sense.

  if ((scold.gt.59).or.(scold.lt.0)) then
     write(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
     opass = .FALSE.
  end if

!  Check that the fractional part  of ODATE makes sense.

!KWM      IF ((scold.GT.59).or.(scold.LT.0)) THEN
!KWM         WRITE(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
!KWM         opass = .FALSE.
!KWM      END IF

  if (.not.opass) then
     write(*,*) 'Crazy ODATE: ', odate(1:olen), olen
     call abort()
  end if

!  Date Checks are completed.  Continue.


!  Compute the number of days, hours, minutes, and seconds in idt

  if (olen.gt.20) then !idt should be in fractions of seconds
     ifrc = olen-20
     ifrc = 10**ifrc
     nday   = abs(idt)/(86400*ifrc)
     nhour  = mod(abs(idt),86400*ifrc)/(3600*ifrc)
     nmin   = mod(abs(idt),3600*ifrc)/(60*ifrc)
     nsec   = mod(abs(idt),60*ifrc)/(ifrc)
     nfrac = mod(abs(idt), ifrc)
  else if (olen.eq.19) then  !idt should be in seconds
     ifrc = 1
     nday   = abs(idt)/86400 ! integer number of days in delta-time
     nhour  = mod(abs(idt),86400)/3600
     nmin   = mod(abs(idt),3600)/60
     nsec   = mod(abs(idt),60)
     nfrac  = 0
  else if (olen.eq.16) then !idt should be in minutes
     ifrc = 1
     nday   = abs(idt)/1440 ! integer number of days in delta-time
     nhour  = mod(abs(idt),1440)/60
     nmin   = mod(abs(idt),60)
     nsec   = 0
     nfrac  = 0
  else if (olen.eq.13) then !idt should be in hours
     ifrc = 1
     nday   = abs(idt)/24 ! integer number of days in delta-time
     nhour  = mod(abs(idt),24)
     nmin   = 0
     nsec   = 0
     nfrac  = 0
  else if (olen.eq.10) then !idt should be in days
     ifrc = 1
     nday   = abs(idt)    ! integer number of days in delta-time
     nhour  = 0
     nmin   = 0
     nsec   = 0
     nfrac  = 0
  else
     write(*,'(''GETH_NEWDATE: Strange length for ODATE: '', i3)') &
          olen
     write(*,*) odate(1:olen)
     call abort()
  end if

  if (idt.ge.0) then

     frnew = frold + nfrac
     if (frnew.ge.ifrc) then
        frnew = frnew - ifrc
        nsec = nsec + 1
     end if

     scnew = scold + nsec
     if (scnew .ge. 60) then
        scnew = scnew - 60
        nmin  = nmin + 1
     end if

     minew = miold + nmin
     if (minew .ge. 60) then
        minew = minew - 60
        nhour  = nhour + 1
     end if

     hrnew = hrold + nhour
     if (hrnew .ge. 24) then
        hrnew = hrnew - 24
        nday  = nday + 1
     end if

     dynew = dyold
     monew = moold
     yrnew = yrold
     do i = 1, nday
        dynew = dynew + 1
        if (dynew.gt.mday(monew)) then
           dynew = dynew - mday(monew)
           monew = monew + 1
           if (monew .gt. 12) then
              monew = 1
              yrnew = yrnew + 1
              ! If the year changes, recompute the number of days in February
              mday(2) = nfeb(yrnew)
           end if
        end if
     end do

  else if (idt.lt.0) then

     frnew = frold - nfrac
     if (frnew .lt. 0) then
        frnew = frnew + ifrc
        nsec = nsec + 1
     end if

     scnew = scold - nsec
     if (scnew .lt. 00) then
        scnew = scnew + 60
        nmin  = nmin + 1
     end if

     minew = miold - nmin
     if (minew .lt. 00) then
        minew = minew + 60
        nhour  = nhour + 1
     end if

     hrnew = hrold - nhour
     if (hrnew .lt. 00) then
        hrnew = hrnew + 24
        nday  = nday + 1
     end if

     dynew = dyold
     monew = moold
     yrnew = yrold
     do i = 1, nday
        dynew = dynew - 1
        if (dynew.eq.0) then
           monew = monew - 1
           if (monew.eq.0) then
              monew = 12
              yrnew = yrnew - 1
              ! If the year changes, recompute the number of days in February
              mday(2) = nfeb(yrnew)
           end if
           dynew = mday(monew)
        end if
     end do
  end if

!  Now construct the new mdate

  nlen = LEN(ndate)

  if (nlen.gt.20) then
     write(ndate(1:19),19) yrnew, monew, dynew, hrnew, minew, scnew
     write(hfrc,'(i10)') frnew+1000000000
     ndate = ndate(1:19)//'.'//hfrc(31-nlen:10)

  else if (nlen.eq.19.or.nlen.eq.20) then
     write(ndate(1:19),19) yrnew, monew, dynew, hrnew, minew, scnew
19   format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)
     if (nlen.eq.20) ndate = ndate(1:19)//'.'

  else if (nlen.eq.16) then
     write(ndate,16) yrnew, monew, dynew, hrnew, minew
16   format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2)

  else if (nlen.eq.13) then
     write(ndate,13) yrnew, monew, dynew, hrnew
13   format(i4,'-',i2.2,'-',i2.2,'_',i2.2)

  else if (nlen.eq.10) then
     write(ndate,10) yrnew, monew, dynew
10   format(i4,'-',i2.2,'-',i2.2)

  end if

  if (olen.ge.11) ndate(11:11) = sp

end subroutine geth_newdate
