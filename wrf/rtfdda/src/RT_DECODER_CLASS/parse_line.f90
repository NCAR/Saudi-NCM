      subroutine parse_line(line,adelim,ifield,string)

      character(len=*), INTENT(IN) :: line
      character, INTENT(IN) :: adelim
      character(len=*) :: string
      character :: c
      character(len=40), allocatable, dimension(:) :: sarray

      integer, INTENT(IN) :: ifield

      integer :: ip,istat,ibeg
      integer, allocatable, dimension(:) :: ipos

      TYPE :: IPOSITION
         INTEGER :: ipos
         TYPE (IPOSITION), POINTER :: p
      END TYPE

      TYPE (IPOSITION), POINTER :: head
      TYPE (IPOSITION), POINTER :: ptr_tmp
      TYPE (IPOSITION), POINTER :: tail

      do ip = 1,len(line)
         if (line(ip:ip) == adelim) then

            if (.not. associated(head)) then
               allocate (head,stat=istat)
               tail => head
               nullify(tail%p)
               tail%ipos = ip
            else
               allocate (tail%p,stat=istat)
               tail => tail%p
               nullify(tail%p)
               tail%ipos = ip
            end if
         end if
      end do

      ! counting how many delimiters in the line

      ptr_tmp => head
      n=0
      do
         if (.not. associated(ptr_tmp)) exit

         n=n+1

         ptr_tmp => ptr_tmp%p
      end do

      allocate(ipos(n))
      allocate(sarray(n+1))

      ! assign the delimiter position to an array

      ptr_tmp => head
      n=0
      do
         if (.not. associated(ptr_tmp)) exit

         n=n+1
         ipos(n) = ptr_tmp%ipos
         ptr_tmp => ptr_tmp%p
      end do

     !do i=1,n
     !   print*,'ipos(',i,') = ',ipos(i)
     !end do

      ! now, assign the string array

      ibeg=1
      do i=1,n+1
         if (i <= n) then
            sarray(i) = line(ibeg:ipos(i)-1)
            ibeg = ipos(i)+1
         else
            sarray(i) = line(ibeg:len(line))
         end if
      end do

      ! pick the string we want from the string array

      string = sarray(ifield)

      deallocate(ipos)
      deallocate(sarray)
      nullify(head)

      return
      end subroutine parse_line
