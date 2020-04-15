      integer function lennonblank(st)
c
c     This routine returns the number of characters in a string,
c     starting from the first character (regardless of whether it is
c     blank or not) and ending at the last non-blank character in the
c     string.
c
      integer i
      character st*(*)
      i = len(st)
      do while (i .ge. 1 .and. st(i:i) .eq. ' ')
        i = i - 1
      enddo
      lennonblank = i
      return
      end
