  program transpose_array

  implicit none

  character (len=80) :: fn, exe_name

  real, allocatable, dimension(:,:) :: old, new
  integer :: j,imax,jmax

  j=iargc()

  if(j /= 1) then
    call getarg(0,exe_name)
    print*,'Usage: ',trim(exe_name),' filename'
    stop
  end if

  call getarg(1,fn)
  
  open(11,file=trim(fn),form='unformatted')
  open(12,file='transposed_array.bin',form='unformatted')

  read(11) imax,jmax

  allocate(old(imax,jmax))
  allocate(new(jmax,imax))

  read(11) old

  new=transpose(old)

  print*,'new(1,1), new(50,50) = ',new(1,1), new(50,50)

  write(12) jmax,imax
  write(12) new

  end program transpose_array 
