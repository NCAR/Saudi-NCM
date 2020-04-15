  program merge_snd_pairs
!
  integer, parameter :: NL=40         !! total number of levels
  character(len=80) :: exename,fn1,fn2,fn3,fn4,fn5
!
  j=iargc()
  if(j < 2) then
    call getarg(0,exename)
    print*,'Usage: ',TRIM(exename),' filename1 filename2 [filename3 ...]'
    stop
  endif
!
  call getarg(1,fn1)
  call getarg(2,fn2)
  if(j > 2) call getarg(3,fn3)
  if(j > 3) call getarg(4,fn4)
  if(j > 4) call getarg(5,fn5)

  open(11,file=fn1,status='old',access='direct',recl=64)
  open(12,file=fn2,status='old',access='direct',recl=64)
  if(j > 2) open(13,file=fn3,status='old',access='direct',recl=64)
  if(j > 3) open(14,file=fn4,status='old',access='direct',recl=64)
  if(j > 4) open(15,file=fn5,status='old',access='direct',recl=64)

  call replace(11,12,NL)
  if(j > 2) call replace(11,13,NL)
  if(j > 3) call replace(11,14,NL)
  if(j > 4) call replace(11,15,NL)

  end program merge_snd_pairs
!
!
!
  subroutine replace(iunit1,iunit2,NL)
!
  character(len=10) :: atime1,atime2
  character(len=8) :: st_id1,st_id2
  integer :: id1, id2
  integer :: elev1, elev2
  real :: lat1,lon1,lat2,lon2
  character(len=64), dimension(40) :: line1,line2


irec2=0
ierr2=0
irec1=0  !yliu

loop2: do
       irec2=irec2+1
       read(iunit2,rec=irec2,iostat=ierr2) atime2,st_id2,lat2,lon2,elev2,id2

       if(ierr2 /= 0) exit loop2
       do k=1,NL
          irec2=irec2+1
          read(iunit2,rec=irec2) line2(k)
       enddo

       !irec1=0  !yliu
       ierr1=0

       loop1: do
              irec1=irec1+1
              read(iunit1,rec=irec1,iostat=ierr1) atime1,st_id1,lat1,lon1,elev1,id1

              if(ierr1 /= 0) exit loop1

              do k=1,NL
                 irec1=irec1+1
                 read(iunit1,rec=irec1) line1(k)
              enddo 

              if(atime1 == atime2 .and. st_id1 == st_id2 .and. &
                 lat1 == lat2 .and. lon1 == lon2) then

!               print*, 'Replacing record number ',irec1

                irec=irec1-NL
                write(iunit1,rec=irec) atime2,st_id2,lat2,lon2,id2
                do k=1,NL
                   irec=irec+1
                   write(iunit1,rec=irec) line2(k)
                enddo

                exit loop1

              endif
              enddo loop1
       enddo loop2

  return
  end subroutine replace
