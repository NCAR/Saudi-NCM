MODULE MM5_HEADER

  INTEGER :: bhi(50,20)
  REAL :: bhr(20,20)
  CHARACTER(LEN=80) :: bhic(50,20),bhrc(20,20)
  REAL :: time
  INTEGER :: start_index(4),end_index(4)
  CHARACTER (LEN=9) :: name
  CHARACTER (LEN=4) :: staggering,ordering
  CHARACTER (LEN=24) :: current_date
  CHARACTER (LEN=25) :: units
  CHARACTER (LEN=46) :: description

END MODULE MM5_HEADER
