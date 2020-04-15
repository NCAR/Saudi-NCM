    program write_wmo_stnid_into_bn
!
    character(len=100) :: line,blkline
    character(len=5) :: idc
    integer :: llen=100
    integer :: irec
!
    open(21,file='gts_sttnid_input.wmo.txt')
    open(22,file='gts_sttnid_final.wmo',access='direct',form='formatted', &
         recl=llen)
    open(23,file='gts_sttnid_cmpct.wmo')
!
    blkline = repeat ( '9', llen )
    do i = 1, 99999
      write ( 22, '(a100)', rec=i ) blkline
    enddo
!
    ierr=0
    do while ( ierr == 0)
       read(21,'(a)',iostat=ierr) line
       idc(1:5)=line(1:5) 
       read(idc,'(i5)') irec
       write(22,'(a100)',rec=irec) line
       write(23,*) line(1:80)
    enddo
!
    end program write_wmo_stnid_into_bn
