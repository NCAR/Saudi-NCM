   PROGRAM test
   INTEGER :: count = 0, duplicate = 0, similar = 0, empty = 0
   CHARACTER ( LEN = 32  ) :: filename
   CHARACTER ( LEN = 3200 ) :: line1 , line2
   INTEGER , PARAMETER :: idlen1 = 50+21, idlen2 = 50+30
   line1 = 'xxx'

   DO
      line2 = line1
      READ ( 3, '(a)',end=90 ) line1
      IF ( line1(idlen1:idlen2) .EQ. '  -8888888' ) THEN
         empty = empty + 1
      ELSE IF ( line2 .EQ. line1 ) THEN
         duplicate = duplicate + 1
      ELSE IF ( line2(:idlen2) .EQ. line1(:idlen2) ) THEN
         similar = similar + 1
         WRITE ( 6, '(a)' ) ' ************************** '
         WRITE ( 6, '(a)' ) TRIM(line2)
         WRITE ( 6, '(a)' ) TRIM(line1)
      ENDIF
      count = count + 1
   ENDDO
90 CONTINUE
   PRINT *, ' count     = ', count
   PRINT *, ' empty     = ', empty
   PRINT *, ' similar   = ', similar
   PRINT *, ' duplicate = ', duplicate
   PRINT *, ' =================================='
   STOP
   end
