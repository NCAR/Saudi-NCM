!-----------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
!-----------------------------------------------------------------------------
!
! std_dev -- Compute mean, variance, and standard deviation for a time series.
!
! This version also includes data masking, to facilitate things like
! seasonal subsetting.
!
! 1.00	2008-dec-30	Original version.  By Dave Allured.
!			Adapted from corr_regress v1.00,
!			  also from standardize_seasonal v2.02.
!			Compute mean and standard deviation for 1-D time series.
! 1.01	2009-jan-06	Improvements to match std_dev_grid v1.01.
!			Change order of calling arguments.
!			Change required minimum count to fractional threshold.
!			Add variance output.
!			Fix incorrect output for degenerate time series.
!
! ----	2014-mar-02	Double precision version.
!			Remove threshold checks, require exact missing values.
!
! Input:   1-D time series, 1-D data mask, and control parameters.  See below.
!
! Output:  Mean, variance, and standard deviation, scalars.
!	   In case of insufficient data, outputs are set to vmiss.
!
! Notes:
!
! Dimension T is simply the data indexing dimension.  It is called
! the time dimension herein, but it may have any definition suitable
! to the calling program.
!
! Time subsetting for random time steps is supported, by using the
! tags(T) data mask array to select time steps to be included.
! This may be used, for example, to get seasonal statistics from
! year-round (continuous) time series.
!
! This version uses the two pass algorithm to minimize roundoff error.
!
!-----------------------------------------------------------------------------

module std__dev
   use config, only : dp
   implicit none

! Special public limit parameters for debugging.
! No check is done unless at least one is set positive.

   integer :: std_dev_min_count = -1
   integer :: std_dev_max_count = -1

contains

subroutine std_dev (data, tags, vmiss, thresh, mean, variance, stdev)
   implicit none

   real(dp), intent (in ) :: data(:)	! 1-D input time series (T)
   logical,  intent (in ) :: tags(:)	! include only TRUE data pairs (T)
   real(dp), intent (in ) :: vmiss	! missing value in input data
   real(dp), intent (in ) :: thresh	! 0-1: required fraction of data present
   					! applies only to tag selected data

   real(dp), intent (out) :: mean	! computed mean
   real(dp), intent (out) :: variance	! computed variance
   real(dp), intent (out) :: stdev	! computed standard deviation

! Local variables.

   integer ntimes, ndata, required_count, j
   real(dp) val
   real(dp) sumx, sdd, dif, dmean, dvariance

! Derived parameters.

   ntimes = size (data)

   required_count = ceiling (thresh * count (tags))   ! # of time steps required
   					! for each grid point for valid output

   required_count = max (1, required_count)	! require at least one datum
   						! at each grid point

! Consistency check.

   if (ntimes /= size (tags)) then
      print *
      write (*, '(2(a,i0))') ' *** std_dev: Time dimensions for data' &
         // ' and tag arrays = ', ntimes, ', ', size (tags)
      print *, '*** std_dev: Abort: Data and tag time dimensions differ.'
      call exit (1)
   end if

! Compute the mean for selected time steps.

   sumx  = 0
   ndata = 0

   do j = 1, ntimes
      if (.not. tags(j)) cycle			! include only selected times
      val = data(j)
      if (val /= vmiss) then
         sumx = sumx + val
         ndata = ndata + 1
      end if
   end do

   if (ndata > 0) then				! suppress divide by zero
      dmean = sumx / ndata			! double precision for calcs
      mean = dmean				! single precision for output
   end if

! Compute variance and standard deviation.

   sdd = 0

   if (ndata > 1) then				! normal points, 2 or more data:

      do j = 1, ntimes				! accumulate deviations squared
         if (.not. tags(j)) cycle
         val = data(j)
         if (val /= vmiss) then
            dif = val - dmean
            sdd = sdd + (dif * dif)
         end if
      end do

      dvariance = sdd / (ndata - 1)		! compute variance
      variance = dvariance
      stdev = sqrt (dvariance)			! compute standard deviation

   else		 				! for degenerate points
      variance = 0				! with only one datum:
      stdev = vmiss                             ! variance is undefined: WC: 2015-06-15
   end if

! DEBUG:  Check data count.  This section is completely inactive
! if debug parameters are left at their default values.

   if (std_dev_min_count >= 0 .or. std_dev_max_count >= 0) then
      if (ndata < std_dev_min_count .or. ndata > std_dev_max_count) then
         print *
         print *, '*** std_dev: tags array:'

         do j = 1, ntimes, 20
            write (*, "(i6, ' :', 20l2)") j, tags(j:min (j+19, ntimes))
         end do

         print *
         write (*, '(a, i0, a)') ' *** std_dev: Abort: Data count = ', ndata, &
            ', outside of debug limits.'
         call exit (1)
      end if
   end if

! Output missing values if data count is below threshold.

! Suppress output if data count is below threshold.

   if (ndata < required_count) then	! at least one datum always required
      mean     = vmiss
      variance = vmiss
      stdev    = vmiss
   end if

end subroutine std_dev

end module std__dev
