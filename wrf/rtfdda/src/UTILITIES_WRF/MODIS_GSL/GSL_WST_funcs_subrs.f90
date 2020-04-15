  MODULE funcs_subrs

  CONTAINS

! Provides the indices where the array is true
  FUNCTION index_true ( n_tot , n_sub , logical_arr )

    IMPLICIT NONE
    INTEGER                   :: n_sub, ii, ctr, n_tot
    INTEGER, DIMENSION(n_sub) :: index_true
    LOGICAL, DIMENSION(n_tot) :: logical_arr

    ctr = 1
    DO ii=1,n_tot
     IF (logical_arr(ii).EQV..TRUE.) THEN
      index_true(ctr) = ii
      ctr = ctr + 1
     END IF
    END DO

  END FUNCTION index_true

! Provides the index in an array for the value nearest to a given value
  FUNCTION ind_nearest_coord( arr , arr_sz , val )

    IMPLICIT NONE
    INTEGER                  :: arr_sz, ind_nearest_coord, ind1, ind2, ctr
    REAL, DIMENSION(arr_sz)  :: arr
    REAL                     :: val, diff1, diff2

    ind1 = 1
    ind2 = arr_sz
    DO ctr=1,arr_sz-1
     IF (val.GE.arr(ctr).AND.val.LE.arr(ctr+1)) THEN
      ind1 = ctr
      ind2 = ctr+1
     END IF
    END DO
    diff1 = ABS(val-arr(ind1))
    diff2 = ABS(val-arr(ind2))
    IF (diff2.LE.diff1) THEN
     ind_nearest_coord = ind2
    ELSE
     ind_nearest_coord = ind1
    END IF

  END FUNCTION ind_nearest_coord

! get_file_size_asc gets the numbers of elements in a file of character string arrays
  FUNCTION get_file_size_asc ( nchar , fname )

    IMPLICIT NONE
    INTEGER        :: get_file_size_asc, nlines, nchar
    CHARACTER(LEN=nchar)  :: fname
    CHARACTER      :: c

    OPEN(1,FILE=fname,STATUS='old')
    nlines=0
    DO
      READ(1,'(a)',END=31) c
      nlines=nlines+1
    END DO
    31 CLOSE(1)
    get_file_size_asc = nlines

  END FUNCTION get_file_size_asc

! get_param_val obtains parameters from a parameter file
  FUNCTION get_param_val ( nlines, param_file, param_name )

   IMPLICIT NONE
   CHARACTER*128 :: param_name, param_file, line, this_param_name, get_param_val, param_line
   INTEGER       :: nlines, n, index1, index2

   param_line = 'Error retrieving parameter from get_param_val'  ! default value
   OPEN(1,FILE=param_file,STATUS='old')
   DO n = 1,nlines
    READ(1,'(a)') line
    index1 = INDEX(line,';',.FALSE.)
    index2 = INDEX(line,';',.TRUE.)
    this_param_name = TRIM(line(index1+1:index2-1))
    IF (this_param_name.EQ.param_name) THEN
     param_line = TRIM(line(1:index1-1))
    END IF
   END DO
   CLOSE(1)
   get_param_val = param_line

  END FUNCTION get_param_val

  FUNCTION day_of_year ( year, month, day )

    IMPLICIT NONE
    INTEGER                :: year, month, day, day_of_year, ii
    INTEGER, DIMENSION(12) :: DaysInMonth

    DO ii=1,12
     DaysInMonth(ii) = days_in_month(year,ii)
    END DO
    IF (month.EQ.1) THEN
     day_of_year = day
    ELSE
     day_of_year = SUM(DaysInMonth(1:month-1)) + day
    END IF

  END FUNCTION day_of_year

  FUNCTION month_day ( year, doy )

    IMPLICIT NONE
    INTEGER        :: year, doy, month, day, days2monthend, ctr, month_day, ii
    INTEGER, DIMENSION(12) :: DaysInMonth
    LOGICAL                :: found_month

    DO ii=1,12
     DaysInMonth(ii) = days_in_month(year,ii)
    END DO
    ctr = 1
    found_month = .FALSE.
    DO WHILE (found_month .EQV. .FALSE.)
     days2monthend = SUM(DaysInMonth(1:ctr))
     IF (doy.GT.days2monthend) THEN
      ctr = ctr + 1
     ELSE
      month = ctr
      day = days_in_month(year,ctr)+doy-days2monthend
      found_month = .TRUE.
     END IF
    END DO
    month_day = month*100 + day

  END FUNCTION month_day

  FUNCTION days_in_month ( year, month )

    IMPLICIT NONE
    INTEGER             :: year, month, days_in_month

    IF (month.EQ.1.OR.month.EQ.3.OR.month.EQ.5.OR.month.EQ.7.OR.MONTH.EQ.8.OR.MONTH.EQ.10.OR.MONTH.EQ.12) THEN
     days_in_month = 31
    ELSE IF (month.EQ.4.OR.month.EQ.6.OR.month.EQ.9.OR.month.EQ.11) THEN
     days_in_month = 30
    ELSE IF (month.EQ.2) THEN
     IF (MOD(year,4).NE.0) THEN
      days_in_month = 28
     ELSE
      days_in_month = 29
     END IF
    END IF

  END FUNCTION days_in_month

  END MODULE funcs_subrs
